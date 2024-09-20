{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Chess.Rules  ( module Chess.Game , module Chess.Rules ) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Bimap as B

import Data.Maybe (isNothing, isJust, fromJust)
import Control.Monad.Trans.State.Lazy ( get )
import Control.Lens ( use, (%=), (.=), at, (?=), to , _1 , (.~) )
import Control.Monad (when, forM_, unless)
import Data.List (uncons)
import Network.WebSockets
    ( Connection, sendBinaryData )
import Data.Aeson (encode)
import Control.Concurrent.STM
    ( atomically, readTMVar, TMVar )


import Chess.Game
import Chess.Structure
import Chess.Internal

import Control.Monad.Trans.Except (runExceptT, throwE)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (first)
import GHC.Natural (Natural)
import Data.Either (isLeft)
import GHC.Stack (HasCallStack)
import Data.Typeable (Typeable)


-- * Rules
type Chess a = Rule ChessState a


-- When `c` touches `y`, if `c` touched `x` right before, process the input `Touch2 c x y`
-- otherwise check if we should remember `Touch1 c y`
playerTouch :: HasCallStack => Chess PlayerTouch
playerTouch (PlayerTouch c y) = do
    touchMemory <- use touch
    case M.lookup c touchMemory of
        Nothing -> do
            cause "Touch1" $ PlayerTouch c y
            cause "UpdateSelection" $ PlayerTouch c y
        Just (x , p) -> do
            touch %= M.delete c
            cause "Touch2" $ PieceMove p x y
            cause "UpdateSelection" $ PlayerTouch c x

isCheckedMove :: Action -> Bool
isCheckedMove (Event e _) = e == "CheckedMovePiece"
isCheckedMove _ = False

markAvailableMoves :: HasCallStack => Game ChessState -> Chess PlayerTouch
markAvailableMoves g (PlayerTouch c _) = do
    -- PS: you might not want to cause UpdateSelection in your simulation
    maybeSelected <- use $ touch . at c
    currentState <- get

    whenJust maybeSelected $ \ (a , _) ->
        forM_ [(i, j) | i <- [0..7], j <- [0..7]] $ \ b -> do
            let res = simGameUntil isCheckedMove g [mkEvent "UncheckedMove" $ Move a b] currentState
            when (isLeft res) $ cause "SendMarkAvailableMove" (c, b)

clearAvailableMoves :: HasCallStack => Chess PlayerTouch
clearAvailableMoves (PlayerTouch c _) = do
    maybeSelected <- use $ touch . at c
    case maybeSelected of
        Nothing -> cause "SendClearAvailableMoves" c
        Just _  -> return ()

updateSelection :: HasCallStack => Chess PlayerTouch
updateSelection (PlayerTouch c x) = do
    touchMemory <- use touch
    case M.lookup c touchMemory of
        Nothing -> cause "SendSelect" $ PlayerSelect c x False
        -- if y is not x then something bad has happened
        Just (y , _) -> cause "SendSelect" $ PlayerSelect c y True

sendSelect :: HasCallStack => Chess PlayerSelect
sendSelect (PlayerSelect c x b) = do
    piece <- use $ board . at x
    cause "SendDrawTile" $ SendDrawTile c x piece (cmap piece b)


-- If `c` is not to move, then don't
touch1Turn :: HasCallStack => Chess PlayerTouch
touch1Turn (PlayerTouch c x) = do
    currentToMove <- use toMove
    when (currentToMove == Just c) $ do
        cause "Touch1Turn" (PlayerTouch c x)

touch1TurnRTS :: HasCallStack => Chess PlayerTouch
touch1TurnRTS (PlayerTouch c x) = cause "Touch1Turn" (PlayerTouch c x)

-- If `x` does not contain a piece, then don't
touch1Piece :: HasCallStack => Chess PlayerTouch
touch1Piece (PlayerTouch c x) = do
    currentBoard <- use board
    whenJust (M.lookup x currentBoard) $ \ p -> do
        cause "Touch1Piece" $ TouchPiece c p x

-- If the piece `p` on `x` has colour `c` then remember, otherwise don't
touch1Colour :: Chess TouchPiece
touch1Colour (TouchPiece c p x) = do
    when (snd p == c) $ do
        touch %= M.insert c (x, p)


touch2 :: HasCallStack => Chess PieceMove
touch2 (PieceMove p x y) = do
    cause "UncheckedMovePiece" (PieceMove p x y)

uncheckedMovePiece :: HasCallStack => Chess Move
uncheckedMovePiece (Move x y) = do
    currentBoard <- use board
    whenJust (M.lookup x currentBoard) $ \ p -> do
        cause "UncheckedMovePiece" $ PieceMove p x y

uncheckedMoveTurn :: HasCallStack => Chess PieceMove
uncheckedMoveTurn (PieceMove p x y) = do
    let c = snd p
    currentToMove <- use toMove
    when (currentToMove == Just c) $ do
        cause "UncheckedMoveTurn" (PieceMove p x y)

uncheckedMoveSelf :: HasCallStack => Chess PieceMove
uncheckedMoveSelf (PieceMove p x y) = do
    target <- M.lookup y <$> use board
    case target of
        Nothing -> cause "UncheckedMoveSelf" (PieceMove p x y)
        Just p' -> when (snd p' /= snd p) $ cause "UncheckedMoveSelf" (PieceMove p x y)

specializeMove :: HasCallStack => Chess PieceMove
specializeMove (PieceMove p x y) = do
    let e = "Unchecked" ++ show (fst p) ++ "Move"
    cause e $ PieceMove p x y

kingMove :: HasCallStack => Chess PieceMove
kingMove (PieceMove p x y) = when (manhattan x y <= 1) $ do
    castling %= S.delete x
    cause "CheckedMovePiece" (PieceMove p x y)

castlingMove :: HasCallStack => Chess PieceMove
castlingMove (PieceMove p a@(ax , ay) b@(bx , by)) = when (ay == by && abs (bx - ax) == 2) $ do
    castle <- use castling
    s <- get
    let c = if ax < bx then (bx + 1 , ay) else (bx - 2 , ay)
    let d = if ax < bx then (bx - 1 , ay) else (bx + 1 , ay)

    let pathOk = pathIsEmpty s $ (, ay) <$> enumFromToL' (fst c) (fst d)

    when (c `S.member` castle && a `S.member` castle && pathOk) $ do
        rookMaybe <- use $ board . at c
        castling %= S.delete c

        whenJust rookMaybe $ \ rook -> do
            castling %= S.delete a

            cause "RawMove" $ PieceMove rook c d
            cause "CheckedMovePiece" $ PieceMove p a b

queenMove :: HasCallStack => Chess PieceMove
queenMove (PieceMove p x y) = do
    s <- get
    when (rookP s x y || bishopP s x y) $ cause "CheckedMovePiece" $ PieceMove p x y

rookMove :: HasCallStack => Chess PieceMove
rookMove (PieceMove p x y) = do
    s <- get
    castling %= S.delete x
    when (rookP s x y) $ cause "CheckedMovePiece" $ PieceMove p x y

bishopMove :: HasCallStack => Chess PieceMove
bishopMove (PieceMove p x y) = do
    s <- get
    when (bishopP s x y) $ cause "CheckedMovePiece" $ PieceMove p x y

knightMove :: HasCallStack => Chess PieceMove
knightMove (PieceMove p x y) = when (knightP x y) $ do
    cause "CheckedMovePiece" $ PieceMove p x y

pawnMove :: HasCallStack => Chess PieceMove
pawnMove (PieceMove p x y) = do
    let c = snd p
    let d = direction c

    let (ax, ay) = x
    let (bx, by) = y

    target <- use $ board . at y

    when (isNothing target && ax == bx && ay + d == by) $ do
        cause "PromotionCheck" $ PromotionCheck c y p
        cause "CheckedMovePiece" $ PieceMove p x y

pawnDoubleMove :: HasCallStack => Chess PieceMove
pawnDoubleMove (PieceMove p x y) = do
    let c = snd p
    let d = direction c

    let (ax, ay) = x
    let (bx, by) = y

    target <- use $ board . at y

    when (isNothing target && ax == bx && rank (snd p) 1 == ay && ay + 2 * d == by) $ do
        currentTurn <- use turn
        enpassant ?= (moveNumber currentTurn + 1 , (ax , ay + d), y, p)
        cause "PromotionCheck" $ PromotionCheck c y p
        cause "CheckedMovePiece" $ PieceMove p x y

pawnCapture :: HasCallStack => Chess PieceMove
pawnCapture (PieceMove p x y) = do
    let c = snd p
    let d = direction c

    let (ax, ay) = x
    let (bx, by) = y

    target <- use $ board . at y

    when (isJust target && abs (ax - bx) == 1 && ay + d == by) $ do
        cause "PromotionCheck" $ PromotionCheck c y p
        cause "CheckedMovePiece" $ PieceMove p x y

pawnEP :: HasCallStack => Chess PieceMove
pawnEP (PieceMove p x y) = do
    let c = snd p
    let d = direction c

    let (ax, ay) = x
    let (bx, by) = y

    ep <- use enpassant
    currentTurn <- use turn

    forM_ ep $ \ (epTurn , epSquare , pSquare, epPiece) -> when (moveNumber currentTurn == epTurn && y == epSquare && abs (ax - bx) == 1 && ay + d == by) $ do
        cause "Take" $ Take p pSquare epPiece
        cause "PromotionCheck" $ PromotionCheck c y p
        cause "CheckedMovePiece" $ PieceMove p x y

promotionCheck :: HasCallStack => Chess PromotionCheck
promotionCheck (PromotionCheck c x p) = do
    when (rank c 7 == snd x) $ do
        promoting ?= (x , p)
        cause "Promoting" ()

pawnZero :: HasCallStack => Chess PieceMove
pawnZero (PieceMove (P, _) _ _) = cause "Zeroing" ()
pawnZero _ = return ()

moveMayCapture :: HasCallStack => Chess PieceMove
moveMayCapture (PieceMove p x y) = do
    target <- M.lookup y <$> use board
    case target of
        Nothing -> cause "NonCapture" (PieceMove p x y)
        Just q  -> cause "Capture" (Capture p x y q)
    cause "MoveEnd" ()

captureZero :: HasCallStack => Chess Capture
captureZero _ = cause "Zeroing" ()

zeroRule :: Chess ()
zeroRule () = do
    currentTurn <- use $ turn . to moveNumber
    zeroing .= currentTurn

nonCapture :: HasCallStack => Chess PieceMove
nonCapture (PieceMove p x y) = do
    cause "RawMove" $ PieceMove p x y

capture :: HasCallStack => Chess Capture
capture (Capture p x y q) = do
    cause "Take" $ Take p y q
    cause "RawMove" $ PieceMove p x y

rawMove :: HasCallStack => Chess PieceMove
rawMove (PieceMove p x y) = do
    cause "PutTile" $ PutTile y (Just p)
    cause "PutTile" $ PutTile x Nothing

rawTake :: HasCallStack => Chess Take
rawTake (Take _ y _) = do
    cause "PutTile" $ PutTile y Nothing

putTile :: HasCallStack => Chess PutTile
putTile (PutTile x p) = do
    board . at x .= p
    cause "SendTileAll" x

moveStep :: ChessState -> Turn -> Turn
moveStep st t = case _promoting st of
    Just (_, (_, c)) -> Promoting (moveNumber t) c
    Nothing          -> Normal $ moveNumber t + 1

nextSubTurn :: HasCallStack => Chess ()
nextSubTurn () = do
    st <- get
    turn %= moveStep st
    cause "NextSubTurn" ()

promotionPrompt :: HasCallStack => Chess ()
promotionPrompt () = do
    currentTurn <- use turn

    case currentTurn of
        Promoting _ c -> cause "SendPromotionPrompt" c
        Normal _ -> return ()

tryPromote :: HasCallStack => Chess (Colour , String)
tryPromote (c, str) = do
    currentTurn <- use turn

    r <- runExceptT $ case currentTurn of
        Promoting _ c' -> do
            currentPromotion <- lift $ use promoting
            let x = (,) <$> B.lookupR str pieceStr <*> currentPromotion
            --effect $ print $ (B.lookupR str pieceStr :: Maybe PieceType, currentPromotion)
            whenJust x $ \ (pt , (a , _)) -> do
                when (pt `elem` promotable && c == c') $ do
                    lift $ cause "PutTile" $ PutTile a (Just (pt, c))
                    throwE ()
        _ -> return ()

    case r of
        Left ()  -> do
            promoting .= Nothing
            cause "MoveEnd" ()
        Right () -> cause "SendPromotionPrompt" c

nMoveRule :: Natural -> Chess ()
nMoveRule n () = do
    currentTurn <- use $ turn . to moveNumber
    lastZero <- use zeroing
    when (currentTurn >= fromIntegral n + lastZero) $ cause "Draw" ()

nFoldRepetition :: Natural -> Chess ()
nFoldRepetition n () = do
    currentBoard <- use board
    currentHistory <- use history

    let k = maybe 1 (+1) (M.lookup currentBoard currentHistory)

    history . at currentBoard ?= k

    when (k >= fromIntegral n) $ cause "Draw" ()

winRule :: HasCallStack => Chess ()
winRule () = do
    currentBoard <- use board
    let kings = filter (\ x -> fst (snd x) == K) $ M.toList currentBoard
    let kingMap = M.fromListWith (++) (fmap (\ (a, (_, c)) -> (c, [a])) kings)

    case M.lookup White kingMap >>= uncons of
        Nothing -> cause "Win" Black
        Just _  -> case M.lookup Black kingMap >>= uncons of
            Nothing -> cause "Win" White
            Just _  -> cause "NextTurn" ()

winClose :: HasCallStack => Chess Colour
winClose _ = cause "CloseRoom" ()

nextTurnSendStatus :: HasCallStack => Chess ()
nextTurnSendStatus () = do
    currentTurn <- use turn
    currentToMove <- use toMove

    case currentTurn of
        Normal _      -> cause "SendRaw" $ SendRaw [White, Black] $ Status ("It's " ++ show (fromJust currentToMove) ++ "'s turn")
        Promoting _ c -> cause "SendRaw" $ SendRaw [White, Black] $ Status ("It's " ++ show c ++ "'s turn")

connectSendBoard :: HasCallStack => Chess Colour
connectSendBoard c = do
    cause "SendBoard" c

connectSendStatus :: HasCallStack => Chess Colour
connectSendStatus p = do
    currentTurn <- use turn
    currentToMove <- use toMove

    -- TODO make SendStatus?
    case currentTurn of
        Normal _      -> cause "SendRaw" $ SendRaw [p] $ Status ("It's " ++ show (fromJust currentToMove) ++ "'s turn")
        Promoting _ c -> cause "SendRaw" $ SendRaw [p] $ Status ("It's " ++ show c ++ "'s turn")

disconnect :: HasCallStack => TMVar (M.Map Colour [Connection]) -> TMVar (ChessState , Game ChessState) -> Chess Colour
disconnect refConn refGame _ = do
    effect $ do
        connM <- atomically $ readTMVar refConn
        when (M.null connM) $ do
            withTMVarIO_ refGame (return . (_1 . closing .~ True))
    cause "TestCloseRoom" ()
    
testCloseRoom :: HasCallStack => Chess ()
testCloseRoom () = do
    isClosing <- use closing
    when isClosing $ do
        cause "CloseRoom" ()

sendDraw :: HasCallStack => Chess ()
sendDraw () = do
    cause "SendRaw" $ SendRaw [White, Black] $ Status "Draw"

sendWin :: HasCallStack => Chess Colour
sendWin c = do
    let c' = if c == White then Black else White

    cause "SendRaw" $ SendRaw [c] $ Status "You win :)"
    cause "SendRaw" $ SendRaw [c'] $ Status "You lose :("

serveBoardAll :: HasCallStack => Chess ()
serveBoardAll () = do
    cause "SendBoard" White
    cause "SendBoard" Black

serveDrawTileAll :: HasCallStack => Chess Square
serveDrawTileAll a = do
    cause "SendTile" (White, a)
    cause "SendTile" (Black, a)

sendTile :: HasCallStack => Chess (Colour , Square)
sendTile (c, x) = do
    piece <- use $ board . at x
    cause "SendDrawTile" $ SendDrawTile c x piece (cmap piece False)

serveBoard :: HasCallStack => Chess Colour
serveBoard c = do
    currentBoard <- use board
    cause "SendRaw" $ SendRaw [c] $ Board (M.map (first pieceImage) currentBoard)

serveDrawTile :: HasCallStack => Chess SendDrawTile
serveDrawTile (SendDrawTile c a p s) = do
    cause "SendRaw" $ SendRaw [c] $ Tile a (first pieceImage <$> p) s

serveTurn :: HasCallStack => Chess Colour
serveTurn c = do
    cause "SendRaw" $ SendRaw [White] (Turn c)
    cause "SendRaw" $ SendRaw [Black] (Turn c)

servePromotionPrompt :: HasCallStack => Chess Colour
servePromotionPrompt c = do
    cause "SendRaw" $ SendRaw [c] Promotion

serveMarkAvailableMove :: HasCallStack => Chess (Colour , Square)
serveMarkAvailableMove (c, a) = do
    cause "SendRaw" $ SendRaw [c] (MarkAvailableMove a)

serveClearAvailableMoves :: HasCallStack => Chess Colour
serveClearAvailableMoves c = do
    cause "SendRaw" $ SendRaw [c] ClearAvailableMoves

serverRule :: TMVar (M.Map Colour [Connection]) -> Chess SendRaw
serverRule connRef (SendRaw cs d) = do
    effect $ do
        connM <- atomically $ readTMVar connRef

        forM_ cs $ \ c -> do
            whenJust (M.lookup c connM) $ \ conns -> do
                forM_ conns (flip sendBinaryData $ encode d)

roomRule :: TMVar (M.Map String (TMVar (ChessState, Game ChessState))) -> TMVar (M.Map String (TMVar (M.Map Colour [Connection]))) -> String -> Rule ChessState ()
roomRule refRefGame refRefConn room () = do
    effect $ withTMVarIO_ refRefGame (return . M.delete room)
    effect $ withTMVarIO_ refRefConn (return . M.delete room)
    running .= False

movePrintBoard :: HasCallStack => Chess ()
movePrintBoard () = cause "PrintBoard" ()

printBoard :: Chess ()
printBoard () = use board >>= effect . putStrLn . ppBoard >> effect (putStrLn "")

idRule :: Typeable a => String -> Chess a
idRule = cause

atomicExplosion :: [(Int, Int)] -> Chess Capture
atomicExplosion offsets (Capture p _ (x , y) _) = do
    forM_ offsets $ \ (dx , dy) -> do
        let b = (x + dx , y + dy)
        maybeTarget <- use $ board . at b
        whenJust maybeTarget $ \ target -> do
            when (fst target /= P || dx == 0 && dy == 0) $
                cause "Take" $ Take p b target

{-
mayCapture :: ChessEvent -> Maybe MayCapture
registerRule "NonCapture" mayCapture
mayCapture (p, a, b) = Just (p , a , b , Nothing)
registerRule "Capture" mayCapture
mayCapture (p, a, b, q) = Just (p , a , b , Just q)
mayCapture _ = Nothing

type MayCapture = (Piece , Square , Square , Maybe Piece)

checkEvents :: (e -> Maybe a) -> Game s e -> s -> [e] -> Maybe a
checkEvents p runner st es = do
    e <- fmap fst $ uncons $ snd $ simulateUntil (isJust . p) runner st es
    p e -- TODO undouble work by inlining into simulateUntil?

checkEventss :: (e -> Maybe a) -> Game s e -> s -> [[e]] -> [a]
checkEventss p runner st = mapMaybe (checkEvents p runner st)

markMoves :: Chess -> Chess
markMoves runner NextTurn = do
    currentTurn <- use toMove
    st <- get

    whenJust currentTurn $ \ c -> do
        pieces <- filter (\ (_ , (_ , c')) -> c == c') . M.toList <$> use board

        let tiles = [(i, j) | i <- [0..7], j <- [0..7]]
        let allMoves = [[UncheckedMovePiece p a b] | (a, p) <- pieces , b <- tiles ]
        let validMoves = checkEventss mayCapture runner st allMoves

        moveCache . at c ?= fmap (\ (p , a , b , _) -> (p , a , b)) validMoves
        captureCache . at c ?= mapMaybe (\case (p , a , b , Just q) -> Just ((p , a , b) , q) ; _ -> Nothing) validMoves

-}
