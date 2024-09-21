{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Chess.Rules  ( module Chess.Game , module Chess.Rules ) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Bimap as B

import Data.Maybe (isNothing, isJust, fromJust, mapMaybe, fromMaybe)
import Control.Monad.Trans.State.Lazy ( get, modify )
import Control.Lens ( use, (%=), (.=), at, (?=), to , _1 , _2 , _3 , (.~), zoom, united, Lens', ALens' )
import Control.Monad (when, forM_)
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
import Data.Function (fix)
import Data.Proxy (Proxy (..))
import Data.Dynamic (fromDynamic)

-- * Rules
type Chess a = Rule ChessState a


-- When `c` touches `y`, if `c` touched `x` right before, process the input `Touch2 c x y`
-- otherwise check if we should remember `Touch1 c y`
playerTouch :: HasCallStack => Lens' s Touch -> Rule s PlayerTouch
playerTouch touch_ (PlayerTouch c y) = do
    touchMemory <- use $ touch_ . at c
    case touchMemory of
        Nothing -> do
            cause "Touch1" $ PlayerTouch c y
            cause "UpdateSelection" $ PlayerTouch c y
        Just (x , p) -> do
            touch_ . at c .= Nothing
            cause "Touch2" $ PieceMove p x y
            cause "UpdateSelection" $ PlayerTouch c x

isCheckedMove :: Action -> Bool
isCheckedMove (Event e _) = e == "CheckedMovePiece"
isCheckedMove _ = False

markAvailableMoves :: HasCallStack => Game ChessState -> Lens' s ChessState -> Rule s PlayerTouch
markAvailableMoves g chess (PlayerTouch c _) = do
    -- PS: you might not want to cause UpdateSelection in your simulation
    maybeSelected <- use $ chess . touch . at c
    currentState <- use chess

    whenJust maybeSelected $ \ (a , _) ->
        forM_ [(i, j) | i <- [0..7], j <- [0..7]] $ \ b -> do
            let res = simGameUntil isCheckedMove g [mkEvent "UncheckedMove" $ Move a b] currentState
            when (isLeft res) $ cause "SendMarkAvailableMove" (c, b)

clearAvailableMoves :: HasCallStack => Lens' s Touch -> Rule s PlayerTouch
clearAvailableMoves touch_ (PlayerTouch c _) = do
    maybeSelected <- use $ touch_ . at c
    case maybeSelected of
        Nothing -> cause "SendClearAvailableMoves" c
        Just _  -> return ()

updateSelection :: HasCallStack => Lens' s Touch -> Rule s PlayerTouch
updateSelection touch_ (PlayerTouch c x) = do
    maybeSelected <- use $ touch_ . at c
    case maybeSelected of
        Nothing -> cause "SendSelect" $ PlayerSelect c x False
        -- if y is not x then something bad has happened
        Just (y , _) -> cause "SendSelect" $ PlayerSelect c y True

sendSelect :: HasCallStack => Lens' s ChessBoard -> Rule s PlayerSelect
sendSelect board_ (PlayerSelect c x b) = do
    piece <- use $ board_ . at x
    cause "SendDrawTile" $ SendDrawTile c x piece (cmap piece b)

-- If `c` is not to move, then don't
touch1Turn :: HasCallStack => Lens' s Turn -> Rule s PlayerTouch
touch1Turn turn_ (PlayerTouch c x) = do
    currentToMove <- use $ turn_ . to turnColour
    when (currentToMove == Just c) $ do
        cause "Touch1Turn" (PlayerTouch c x)

-- If `x` does not contain a piece, then don't
touch1Piece :: HasCallStack => Lens' s ChessBoard -> Rule s PlayerTouch
touch1Piece board_ (PlayerTouch c x) = do
    piece <- use $ board_ . at x
    whenJust piece $ \ p -> do
        cause "Touch1Piece" $ TouchPiece c p x

-- If the piece `p` on `x` has colour `c` then remember, otherwise don't
touch1Colour :: Lens' s Touch -> Rule s TouchPiece
touch1Colour touch_ (TouchPiece c p x) = do
    when (snd p == c) $ do
        touch_ %= M.insert c (x, p)

touch2 :: HasCallStack => Rule s PieceMove
touch2 (PieceMove p x y) = do
    cause "UncheckedMovePiece" (PieceMove p x y)

uncheckedMovePiece :: HasCallStack => Lens' s ChessBoard -> Rule s Move
uncheckedMovePiece board_ (Move x y) = do
    maybePiece <- use $ board_ . at x
    whenJust maybePiece $ \ p -> do
        cause "UncheckedMovePiece" $ PieceMove p x y

uncheckedMoveTurn :: HasCallStack => Lens' s Turn -> Rule s PieceMove
uncheckedMoveTurn turn_ (PieceMove p x y) = do
    let c = snd p
    currentToMove <- use $ turn_ . to turnColour
    when (currentToMove == Just c) $ do
        cause "UncheckedMoveTurn" (PieceMove p x y)

uncheckedMoveSelf :: HasCallStack => Lens' s ChessBoard -> Rule s PieceMove
uncheckedMoveSelf board_ (PieceMove p x y) = do
    target <- use $ board_ . at y
    case target of
        Nothing -> cause "UncheckedMoveSelf" (PieceMove p x y)
        Just p' -> when (snd p' /= snd p) $ cause "UncheckedMoveSelf" (PieceMove p x y)

specializeMove :: HasCallStack => Rule s PieceMove
specializeMove (PieceMove p x y) = do
    let e = "Unchecked" ++ show (fst p) ++ "Move"
    cause e $ PieceMove p x y

kingMove :: HasCallStack => Lens' s Castling -> Rule s PieceMove
kingMove castling_ (PieceMove p x y) = do
    when (manhattan x y <= 1) $ do
        castling_ %= S.delete x
        cause "CheckedMovePiece" (PieceMove p x y)

castlingMove :: HasCallStack => Lens' s ChessBoard -> Lens' s Castling -> Rule s PieceMove
castlingMove board_ castling_ (PieceMove p a@(ax , ay) b@(bx , by)) = when (ay == by && abs (bx - ax) == 2) $ do
    castle <- use castling_
    s <- use board_
    let c = if ax < bx then (bx + 1 , ay) else (bx - 2 , ay)
    let d = if ax < bx then (bx - 1 , ay) else (bx + 1 , ay)

    let pathOk = pathIsEmpty s $ (, ay) <$> enumFromToL' (fst c) (fst d)

    when (c `S.member` castle && a `S.member` castle && pathOk) $ do
        rookMaybe <- use $ board_ . at c
        castling_ %= S.delete c

        whenJust rookMaybe $ \ rook -> do
            castling_ %= S.delete a

            cause "RawMove" $ PieceMove rook c d
            cause "CheckedMovePiece" $ PieceMove p a b

queenMove :: HasCallStack => Lens' s ChessBoard -> Rule s PieceMove
queenMove board_ (PieceMove p x y) = do
    s <- use board_
    when (rookP s x y || bishopP s x y) $ cause "CheckedMovePiece" $ PieceMove p x y

rookMove :: HasCallStack => Lens' s ChessBoard -> Lens' s Castling -> Rule s PieceMove
rookMove board_ castling_ (PieceMove p x y) = do
    s <- use board_
    castling_ %= S.delete x
    when (rookP s x y) $ cause "CheckedMovePiece" $ PieceMove p x y

bishopMove :: HasCallStack => Lens' s ChessBoard -> Rule s PieceMove
bishopMove board_ (PieceMove p x y) = do
    s <- use board_
    when (bishopP s x y) $ cause "CheckedMovePiece" $ PieceMove p x y

knightMove :: HasCallStack => Rule s PieceMove
knightMove (PieceMove p x y) = do
    when (knightP x y) $ do
        cause "CheckedMovePiece" $ PieceMove p x y

pawnMove :: HasCallStack => Lens' s ChessBoard -> Rule s PieceMove
pawnMove board_ (PieceMove p x y) = do
    let c = snd p
    let d = direction c

    let (ax, ay) = x
    let (bx, by) = y

    target <- use $ board_ . at y

    when (isNothing target && ax == bx && ay + d == by) $ do
        cause "PromotionCheck" $ PromotionCheck c y p
        cause "CheckedMovePiece" $ PieceMove p x y

pawnDoubleMove :: HasCallStack => Lens' s ChessBoard -> Lens' s Turn -> Lens' s EnPassant -> Rule s PieceMove
pawnDoubleMove board_ turn_ enpassant_ (PieceMove p x y) = do
    let c = snd p
    let d = direction c

    let (ax, ay) = x
    let (bx, by) = y

    target <- use $ board_ . at y

    when (isNothing target && ax == bx && rank (snd p) 1 == ay && ay + 2 * d == by) $ do
        currentTurn <- use turn_
        enpassant_ ?= (moveNumber currentTurn + 1 , (ax , ay + d), y, p)
        cause "PromotionCheck" $ PromotionCheck c y p
        cause "CheckedMovePiece" $ PieceMove p x y

pawnCapture :: HasCallStack => Lens' s ChessBoard -> Rule s PieceMove
pawnCapture board_ (PieceMove p x y) = do
    let c = snd p
    let d = direction c

    let (ax, ay) = x
    let (bx, by) = y

    target <- use $ board_ . at y

    when (isJust target && abs (ax - bx) == 1 && ay + d == by) $ do
        cause "PromotionCheck" $ PromotionCheck c y p
        cause "CheckedMovePiece" $ PieceMove p x y

pawnEP :: HasCallStack => Lens' s Turn -> Lens' s EnPassant -> Rule s PieceMove
pawnEP turn_ enpassant_ (PieceMove p x y) = do
    let c = snd p
    let d = direction c

    let (ax, ay) = x
    let (bx, by) = y

    ep <- use enpassant_
    currentTurn <- use turn_

    forM_ ep $ \ (epTurn , epSquare , pSquare, epPiece) -> do
        when (moveNumber currentTurn == epTurn && y == epSquare && abs (ax - bx) == 1 && ay + d == by) $ do
            cause "Take" $ Take p pSquare epPiece
            cause "PromotionCheck" $ PromotionCheck c y p
            cause "CheckedMovePiece" $ PieceMove p x y

promotionCheck :: HasCallStack => Lens' s Promoting -> Rule s PromotionCheck
promotionCheck promoting_ (PromotionCheck c x p) = do
    when (rank c 7 == snd x) $ do
        promoting_ ?= (x , p)
        cause "Promoting" ()

pawnZero :: HasCallStack => Rule s PieceMove
pawnZero (PieceMove (P, _) _ _) = cause "Zeroing" ()
pawnZero _ = return ()

moveMayCapture :: HasCallStack => Lens' s ChessBoard -> Rule s PieceMove
moveMayCapture board_ (PieceMove p x y) = do
    target <- use $ board_ . at y
    case target of
        Nothing -> cause "NonCapture" (PieceMove p x y)
        Just q  -> cause "Capture" (Capture p x y q)
    cause "MoveEnd" ()

captureZero :: HasCallStack => Rule s Capture
captureZero _ = do
    cause "Zeroing" ()

zeroRule :: Lens' s Turn -> Lens' s Int -> Rule s ()
zeroRule turn_ zeroing_ () = do
    currentTurn <- use $ turn_ . to moveNumber
    zeroing_ .= currentTurn

nonCapture :: HasCallStack => Rule s PieceMove
nonCapture (PieceMove p x y) = do
    cause "RawMove" $ PieceMove p x y

capture :: HasCallStack => Rule s Capture
capture (Capture p x y q) = do
    cause "Take" $ Take p y q
    cause "RawMove" $ PieceMove p x y

rawMove :: HasCallStack => Rule s PieceMove
rawMove (PieceMove p x y) = do
    cause "PutTile" $ PutTile y (Just p)
    cause "PutTile" $ PutTile x Nothing

rawTake :: HasCallStack => Rule s Take
rawTake (Take _ y _) = do
    cause "PutTile" $ PutTile y Nothing

putTile :: HasCallStack => Lens' s ChessBoard -> Rule s PutTile
putTile board_ (PutTile x p) = do
    board_ . at x .= p
    cause "SendTileAll" x

moveStep :: Promoting -> Turn -> Turn
moveStep st t = case st of
    Just (_, (_, c)) -> Promoting (moveNumber t) c
    Nothing          -> Normal $ moveNumber t + 1

nextSubTurn :: HasCallStack => Lens' s Turn -> Lens' s Promoting -> Rule s ()
nextSubTurn turn_ promoting_ () = do
    st <- use promoting_
    turn_ %= moveStep st
    cause "NextSubTurn" ()

promotionPrompt :: HasCallStack => Lens' s Turn -> Rule s ()
promotionPrompt turn_ () = do
    currentTurn <- use turn_

    case currentTurn of
        Promoting _ c -> cause "SendPromotionPrompt" c
        Normal _ -> return ()

tryPromote :: HasCallStack => Lens' s Turn -> Lens' s Promoting -> Rule s (Colour , String)
tryPromote turn_ promoting_ (c, str) = do
    currentTurn <- use turn_

    r <- runExceptT $ case currentTurn of
        Promoting _ c' -> do
            currentPromotion <- lift $ use promoting_
            let x = (,) <$> B.lookupR str pieceStr <*> currentPromotion
            whenJust x $ \ (pt , (a , _)) -> do
                when (pt `elem` promotable && c == c') $ do
                    lift $ cause "PutTile" $ PutTile a (Just (pt, c))
                    throwE ()
        _ -> return ()

    case r of
        Left ()  -> do
            promoting_ .= Nothing
            cause "MoveEnd" ()
        Right () -> cause "SendPromotionPrompt" c

nMoveRule :: Natural -> Lens' s Turn -> Lens' s Int -> Rule s ()
nMoveRule n turn_ zeroing_ () = do
    currentTurn <- use $ turn_ . to moveNumber
    lastZero <- use zeroing_
    when (currentTurn >= fromIntegral n + lastZero) $ do
        cause "Draw" ()

nFoldRepetition :: Natural -> Lens' s ChessBoard -> Lens' s History -> Rule s ()
nFoldRepetition n board_ history_ () = do
    currentBoard <- use board_
    currentHistory <- use history_

    let k = maybe 1 (+1) (M.lookup currentBoard currentHistory)
    history_ . at currentBoard ?= k

    when (k >= fromIntegral n) $ do
        cause "Draw" ()

winRule :: HasCallStack => Lens' s ChessBoard -> Rule s ()
winRule board_ () = do
    currentBoard <- use board_
    let kings = filter (\ x -> fst (snd x) == K) $ M.toList currentBoard
    let kingMap = M.fromListWith (++) (fmap (\ (a, (_, c)) -> (c, [a])) kings)

    case M.lookup White kingMap >>= uncons of
        Nothing -> cause "Win" Black
        Just _  -> case M.lookup Black kingMap >>= uncons of
            Nothing -> cause "Win" White
            Just _  -> cause "NextTurn" ()

winClose :: HasCallStack => Rule s Colour
winClose _ = cause "CloseRoom" ()

nextTurnSendStatus :: HasCallStack => Lens' s Turn -> Rule s ()
nextTurnSendStatus turn_ () = do
    currentTurn <- use turn_

    case currentTurn of
        Normal i      -> cause "SendRaw" $ SendRaw [White, Black] $ Status ("It's " ++ show (moveNumberColour i) ++ "'s turn")
        Promoting _ c -> cause "SendRaw" $ SendRaw [White, Black] $ Status ("It's " ++ show c ++ "'s turn")

connectSendBoard :: HasCallStack => Rule s Colour
connectSendBoard c = do
    cause "SendBoard" c

connectSendStatus :: HasCallStack => Lens' s Turn -> Rule s Colour
connectSendStatus turn_ p = do
    currentTurn <- use turn_

    -- TODO make SendStatus?
    case currentTurn of
        Normal i      -> cause "SendRaw" $ SendRaw [p] $ Status ("It's " ++ show (moveNumberColour i) ++ "'s turn")
        Promoting _ c -> cause "SendRaw" $ SendRaw [p] $ Status ("It's " ++ show c ++ "'s turn")

disconnect :: HasCallStack => TMVar (M.Map Colour [Connection]) -> TMVar (ChessState , Game ChessState) -> Rule s Colour
disconnect refConn refGame _ = do
    effect $ do
        connM <- atomically $ readTMVar refConn
        when (M.null connM) $ do
            withTMVarIO_ refGame (return . (_1 . closing .~ True))
    cause "TestCloseRoom" ()

testCloseRoom :: HasCallStack => Lens' s Bool -> Rule s ()
testCloseRoom closing_ () = do
    isClosing <- use closing_
    when isClosing $ do
        cause "CloseRoom" ()

sendDraw :: HasCallStack => Rule s ()
sendDraw () = do
    cause "SendRaw" $ SendRaw [White, Black] $ Status "Draw"

sendWin :: HasCallStack => Rule s Colour
sendWin c = do
    let c' = if c == White then Black else White

    cause "SendRaw" $ SendRaw [c] $ Status "You win :)"
    cause "SendRaw" $ SendRaw [c'] $ Status "You lose :("

serveBoardAll :: HasCallStack => Rule s ()
serveBoardAll () = do
    cause "SendBoard" White
    cause "SendBoard" Black

serveDrawTileAll :: HasCallStack => Rule s Square
serveDrawTileAll a = do
    cause "SendTile" (White, a)
    cause "SendTile" (Black, a)

sendTile :: HasCallStack => Lens' s ChessBoard -> Rule s (Colour , Square)
sendTile board_ (c, x) = do
    piece <- use $ board_ . at x
    cause "SendDrawTile" $ SendDrawTile c x piece (cmap piece False)

serveBoard :: HasCallStack => Lens' s ChessBoard -> Rule s Colour
serveBoard board_ c = do
    currentBoard <- use $ board_ . to (fmap (first pieceImage))
    cause "SendRaw" $ SendRaw [c] $ Board currentBoard

serveDrawTile :: HasCallStack => Rule s SendDrawTile
serveDrawTile (SendDrawTile c a p s) = do
    cause "SendRaw" $ SendRaw [c] $ Tile a (first pieceImage <$> p) s

serveTurn :: HasCallStack => Rule s Colour
serveTurn c = do
    cause "SendRaw" $ SendRaw [White] (Turn c)
    cause "SendRaw" $ SendRaw [Black] (Turn c)

servePromotionPrompt :: HasCallStack => Rule s Colour
servePromotionPrompt c = do
    cause "SendRaw" $ SendRaw [c] Promotion

serveMarkAvailableMove :: HasCallStack => Rule s (Colour , Square)
serveMarkAvailableMove (c, a) = do
    cause "SendRaw" $ SendRaw [c] (MarkAvailableMove a)

serveClearAvailableMoves :: HasCallStack => Rule s Colour
serveClearAvailableMoves c = do
    cause "SendRaw" $ SendRaw [c] ClearAvailableMoves

serverRule :: TMVar (M.Map Colour [Connection]) -> Rule s SendRaw
serverRule connRef (SendRaw cs d) = do
    effect $ do
        connM <- atomically $ readTMVar connRef

        forM_ cs $ \ c -> do
            whenJust (M.lookup c connM) $ \ conns -> do
                forM_ conns (flip sendBinaryData $ encode d)

roomRule :: TMVar (M.Map String (TMVar (ChessState, Game ChessState))) -> TMVar (M.Map String (TMVar (M.Map Colour [Connection]))) -> String -> Lens' s Bool -> Rule s ()
roomRule refRefGame refRefConn room running_ () = do
    effect $ withTMVarIO_ refRefGame (return . M.delete room)
    effect $ withTMVarIO_ refRefConn (return . M.delete room)
    running_ .= False

movePrintBoard :: HasCallStack => Rule s ()
movePrintBoard () = cause "PrintBoard" ()

printBoard :: Lens' s ChessBoard -> Rule s ()
printBoard board_ () = use board_ >>= effect . putStrLn . ppBoard >> effect (putStrLn "")

idRule :: (Typeable a, HasCallStack) => String -> Rule s a
idRule = cause

returnRule :: Rule s a
returnRule _ = return ()

atomicExplosion :: HasCallStack => [(Int, Int)] -> Lens' s ChessBoard -> Rule s Capture
atomicExplosion offsets board_ (Capture p _ (x , y) _) = do
    forM_ offsets $ \ (dx , dy) -> do
        let b = (x + dx , y + dy)
        maybeTarget <- use $ board_ . at b
        whenJust maybeTarget $ \ target -> do
            when (fst target /= P || dx == 0 && dy == 0) $
                cause "Take" $ Take p b target

mayCapture :: Action -> Maybe (Either Move Capture)
mayCapture (Event "NonCapture" arg) = do
    (PieceMove _ a b) <- fromDynamic arg
    return $ Left $ Move a b
mayCapture (Event "Capture" arg) = do
    (Capture p a b q) <- fromDynamic arg
    return $ Right $ Capture p a b q
mayCapture _ = Nothing

type MayCapture = (Piece , Square , Square , Maybe Piece)

turnStart :: HasCallStack => Rule s a
turnStart _ = cause "TurnStart" ()

checkEvents :: (Action -> Maybe a) -> Game s -> s -> [Action] -> Maybe a
checkEvents p runner st acts = do
    e <- either (Just . snd) (const Nothing) $ simGameUntil (isJust . p) runner acts st
    p e -- TODO undouble work by inlining into simulateUntil?

markMoves :: Typeable a => (Game ChessState -> Game ChessState) -> String -> String -> Proxy a -> Lens' s ChessState -> Rule s ()
markMoves g bypassStart bypassEnd (Proxy @a) chess_ () = do
    let bypassRules = fix $ overwriteRule bypassStart (idRule bypassEnd :: Rule ChessState a) . g

    currentTurn <- use $ chess_ . toMove
    st <- use chess_

    whenJust currentTurn $ \ c -> do
        pieces <- filter (\ (_ , (_ , c')) -> c == c') . M.toList <$> use (chess_ . board)

        let tiles = [(i, j) | i <- [0..7], j <- [0..7]]
        let allMoves = [(p, a, b) | (a, p) <- pieces , b <- tiles ]
        let allMoves' = [[mkEvent "UncheckedMovePiece" $ PieceMove p a b] | (p, a, b) <- allMoves ]

        let validMoves = mapMaybe (checkEvents mayCapture bypassRules st) allMoves'

        --effect $ print validMoves

        chess_ . moveCache . at c ?= fmap (either id $ \ (Capture _ a b _) -> Move a b) validMoves
        chess_ . captureCache . at c ?= mapMaybe (either (const Nothing) Just) validMoves

checkers :: HasCallStack => Lens' s Turn -> Lens' s CaptureCache -> Rule s PieceMove
checkers turn_ captures_ (PieceMove p x y) = do
    currentTurn <- use $ turn_ . to turnColour
    whenJust currentTurn $ \ c -> do
        maybeCaptures <- use $ captures_ . at c
        let captures = fromMaybe [] maybeCaptures
        let moves = map ( \ (Capture _ a b _) -> (a , b)) captures

        when (null captures || (x, y) `elem` moves) $ do
            cause "UncheckedMoveCheckers" (PieceMove p x y)

