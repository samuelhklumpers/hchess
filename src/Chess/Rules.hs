{-# LANGUAGE TypeFamilies, TupleSections, OverloadedStrings #-}

module Chess.Rules  ( module Chess.Game , module Chess.Rules ) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Bimap as B

import Data.Maybe (isNothing, isJust, mapMaybe, fromJust)
import Control.Monad.Trans.State.Lazy ( get )
import Control.Lens ( use, (%=), (.=), at, (^.), (?=) )
import Control.Monad (when, forM_, unless)
import Data.List (intercalate, uncons)
import Data.Char (toLower)
import Network.WebSockets
    ( Connection, sendBinaryData )
import Data.Aeson (encode)
import Control.Concurrent.STM
    ( atomically, readTMVar, TMVar )



import Chess.Game
import Control.Monad.Trans.Except (runExceptT, throwE)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (first)
import GHC.IO (unsafePerformIO)


-- * Rules
type Chess = Game ChessState ChessEvent

-- When `c` touches `y`, if `c` touched `x` right before, process the input `Touch2 c x y`, otherwise check if we should remember `Touch1 c y`
touch1 :: Chess
touch1 (Touch c y) = do
    touchMemory <- use touch
    case M.lookup c touchMemory of
        Nothing -> do
            cause $ Touch1 c y
            cause $ UpdateSelection c y
        Just (x , p) -> do
            touch %= M.delete c
            cause $ Touch2 p x y
            cause $ UpdateSelection c x
touch1 _ = return ()

sendSelection :: Chess
sendSelection (UpdateSelection c x) = do
    touchMemory <- use touch
    case M.lookup c touchMemory of
        Nothing -> cause $ SendSelect c x False
        -- if y is not x then something bad has happened
        Just (y , _) -> cause $ SendSelect c y True
sendSelection _ = return ()

cmap :: Maybe Piece -> Bool -> String
cmap _     True  = "#FF0000"
cmap (Just (_ , White)) False = "#FFFFFF"
cmap (Just (_ , Black)) False = "#000000"
cmap _ _ = ""

screenTransform :: Colour -> Square -> Square
screenTransform White x       = x
screenTransform Black (x , y) = (x , 7 - y)

drawSelection :: Chess
drawSelection (SendSelect c x b) = do
    piece <- use $ board . at x
    cause $ SendDrawTile c x piece (cmap piece b)
drawSelection _ = return ()

sendTile :: Chess
sendTile (SendTile c x) = do
    piece <- use $ board . at x
    cause $ SendDrawTile c x piece (cmap piece False)
sendTile _ = return ()

connectSendStatus :: Chess
connectSendStatus (PlayerConnected p) = do
    currentTurn <- use turn
    currentToMove <- use toMove

    case currentTurn of
        Normal _      -> cause $ SendRaw p $ Status ("It's " ++ show (fromJust currentToMove) ++ "'s turn")
        Promoting _ c -> cause $ SendRaw p $ Status ("It's " ++ show c ++ "'s turn")
connectSendStatus _ = return ()

nextTurnSendStatus :: Chess
nextTurnSendStatus NextTurn = do
    currentTurn <- use turn
    currentToMove <- use toMove

    forM_ [White, Black] $ \ p ->
        case currentTurn of
            Normal _      -> cause $ SendRaw p $ Status ("It's " ++ show (fromJust currentToMove) ++ "'s turn")
            Promoting _ c -> cause $ SendRaw p $ Status ("It's " ++ show c ++ "'s turn")
nextTurnSendStatus _ = return ()




-- If `c` is not to move, then don't
touch1Turn :: Chess
touch1Turn (Touch1 c x) = do
    currentToMove <- use toMove
    when (currentToMove == Just c) $ cause $ Touch1Turn c x
touch1Turn _ = return ()

touch1TurnRTS :: Chess
touch1TurnRTS (Touch1 c x) = do
    cause $ Touch1Turn c x
touch1TurnRTS _ = return ()


-- If `x` does not contain a piece, then don't
touch1Piece :: Chess
touch1Piece (Touch1Turn c x) = do
    currentBoard <- use board
    whenJust (M.lookup x currentBoard) $
        \ p -> cause (Touch1Piece c p x)
touch1Piece _ = return ()

-- If the piece `p` on `x` has colour `c` then remember, otherwise don't
touch1Colour :: Chess
touch1Colour (Touch1Piece c p x) = do
    when (snd p == c) $ touch %= M.insert c (x, p)
touch1Colour _ = return ()

touch2Colour :: Chess
touch2Colour (Touch2 p x y) = do
    target <- M.lookup y <$> use board
    case target of
        Nothing -> cause $ UncheckedMove x y
        Just p' -> when (snd p' /= snd p) $ cause $ UncheckedMove x y
touch2Colour _ = return ()

moveMayCapture :: Chess
moveMayCapture (Move p x y) = do
    target <- M.lookup y <$> use board
    case target of
        Nothing -> cause $ NonCapture p x y
        Just _  -> cause $ Capture p x y
    cause MoveEnd
moveMayCapture _ = return ()

capture :: Chess
capture (Capture p x y) = do
    cause $ Take p y
    cause $ RawMove p x y
capture _ = return ()

nonCapture :: Chess
nonCapture (NonCapture p x y) = cause $ RawMove p x y
nonCapture _ = return ()

putTile :: Chess
putTile (PutTile x p) = do
    board . at x .= p
    cause $ SendTileAll x
putTile _ = return ()

winRule :: Chess
winRule MoveEnd = do
    currentBoard <- use board
    let kings = filter (\ x -> fst (snd x) == K) $ M.toList currentBoard
    let kingMap = M.fromListWith (++) (fmap (\ (a, (_, c)) -> (c, [a])) kings)

    case M.lookup White kingMap >>= uncons of
        Nothing -> cause $ Win Black
        Just _  -> case M.lookup Black kingMap >>= uncons of
            Nothing -> cause $ Win White
            Just _  -> cause NextTurn
winRule _ = return ()

sendWin :: Chess
sendWin (Win c) = do
    let c' = if c == White then Black else White

    cause $ SendRaw c $ Status "You win :)"
    cause $ SendRaw c' $ Status "You lose :("
sendWin _ = return ()

isMove :: ChessEvent -> Bool
isMove (Move {}) = True
isMove _ = False

sendAvailableMoves :: Chess -> Chess
sendAvailableMoves g (UpdateSelection c _) = do -- PS: you might not want to cause UpdateSelection in your simulation
    maybeSelected <- use $ touch . at c
    currentState <- get
    whenJust maybeSelected $ \ (a , _) ->
        forM_ [(i, j) | i <- [0..7], j <- [0..7]] $ \ b -> do
            let (_ , r) = simulateUntil isMove g currentState [Touch c b]
            unless (null r) $ cause $ SendMarkAvailableMove c b
sendAvailableMoves _ _ = return ()


clearAvailableMoves :: Chess
clearAvailableMoves (UpdateSelection c _) = do
    maybeSelected <- use $ touch . at c
    case maybeSelected of
        Nothing -> cause $ SendClearAvailableMoves c
        Just _  -> return ()
clearAvailableMoves _ = return ()


serverRule :: TMVar (M.Map PlayerId Connection) -> Chess
serverRule connRef e = do
    case e of
        (PlayerConnected c) -> do
            cause $ SendBoard c
        (SendDrawTile c a p s)  -> do
            cause $ SendRaw c $ Tile (screenTransform c a) (first pieceImage <$> p) s
        (SendBoard c)   -> do
            currentBoard <- use board
            cause $ SendRaw c $ Board (M.mapKeys (screenTransform c) $ M.map (first pieceImage) currentBoard)
        SendBoardAll    -> do
            cause $ SendBoard White
            cause $ SendBoard Black
        (SendTileAll a) -> do
            cause $ SendTile White a
            cause $ SendTile Black a
        (SendTurn c)    -> do
            cause $ SendRaw White $ Turn c
            cause $ SendRaw Black $ Turn c
        (SendPromotionPrompt c) -> do
            cause $ SendRaw c Promotion
        (SendMarkAvailableMove c a) -> do
            cause $ SendRaw c (MarkAvailableMove (screenTransform c a))
        (SendClearAvailableMoves c) -> do
            cause $ SendRaw c ClearAvailableMoves
        (SendRaw c d)   -> do
            connMap <- use connections
            effect $ do
                conns <- atomically $ readTMVar connRef

                whenJust (M.lookup c connMap >>= \ x -> M.lookup x conns) $ \ conn -> do
                    sendBinaryData conn $ encode d
        _               -> return ()


rawMove :: Chess
rawMove (RawMove p x y) = do
    cause $ PutTile y (Just p)
    cause $ PutTile x Nothing
rawMove _ = return ()

uncheckedMovePiece :: Chess
uncheckedMovePiece (UncheckedMove x y) = do
    currentBoard <- use board
    whenJust (M.lookup x currentBoard) $
        \ p -> cause $ UncheckedMovePiece p x y
uncheckedMovePiece _ = return ()

specializeMove :: Chess
specializeMove (UncheckedMovePiece p x y) = do
    target <- use $ board . at y
    cause $ UncheckedMoveType p (fst p) target x y
specializeMove _ = return ()

manhattan :: Square -> Square -> Int
manhattan (x, y) (v, w) = max (abs $ x - v) (abs $ y - w)

direction :: Colour -> Int
direction White = -1
direction Black = 1

rank :: Colour -> Int -> Int
rank White x = 7 - x
rank Black x = x

kingMove :: Chess
kingMove (UncheckedMoveType p K _ x y)  = do
    when (manhattan x y <= 1) $ do
        castling %= S.delete x
        cause (CheckedMovePiece p x y)
kingMove _ = return ()

castlingMove :: Chess
castlingMove (UncheckedMoveType p K _ a@(ax , ay) b@(bx , by)) = do
    when (ay == by && abs (bx - ax) == 2) $ do
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

                cause $ RawMove rook c d
                cause $ CheckedMovePiece p a b
castlingMove _ = return ()

enumFromTo' :: (Enum a, Ord a, Num a) => a -> a -> [a]
enumFromTo' x y
    | x < y = out
    | otherwise = reverse out
    where
    out = enumFromTo (min x y + 1) (max x y - 1)

enumFromToLR' :: (Enum a, Ord a) => a -> a -> [a]
enumFromToLR' x y
    | x < y = out
    | otherwise = reverse out
    where
    out = enumFromTo (min x y) (max x y)

enumFromToL' :: (Enum a, Ord a) => a -> a -> [a]
enumFromToL' x y
    | x < y = drop 1 out
    | otherwise = drop 1 $ reverse out
    where
    out = enumFromTo (min x y) (max x y)

rookPath :: Square -> Square -> [[Square]]
rookPath (ax, ay) (bx, by)
  | ax == bx = [(ax ,) <$> enumFromTo' ay by]
  | ay == by = [(, ay) <$> enumFromTo' ax bx]
  | otherwise = []

pathIsEmpty :: ChessState -> [Square] -> Bool
pathIsEmpty s xs = null $ mapMaybe (\ a -> s ^. board . at a) xs

rookP :: ChessState -> Square -> Square -> Bool
rookP s a b = any (pathIsEmpty s) $ rookPath a b

bishopPath :: Square -> Square -> [[Square]]
bishopPath (ax, ay) (bx, by)
  | abs (bx - ax) == abs (by - ay) = [zip (enumFromTo' ax bx) (enumFromTo' ay by)]
  | otherwise = []

bishopP :: ChessState -> Square -> Square -> Bool
bishopP s a b = any (pathIsEmpty s) $ bishopPath a b

knightP :: Square -> Square -> Bool
knightP (ax, ay) (bx, by) = abs ((bx - ax) * (by - ay)) == 2

queenMove :: Chess
queenMove (UncheckedMoveType p Q _ x y) = do
    s <- get
    when (rookP s x y || bishopP s x y) $ cause (CheckedMovePiece p x y)
queenMove _ = return ()

rookMove :: Chess
rookMove (UncheckedMoveType p R _ x y) = do
    s <- get
    castling %= S.delete x
    when (rookP s x y) $ cause (CheckedMovePiece p x y)
rookMove _ = return ()

bishopMove :: Chess
bishopMove (UncheckedMoveType p B _ x y) = do
    s <- get
    when (bishopP s x y) $ cause (CheckedMovePiece p x y)
bishopMove _ = return ()

knightMove :: Chess
knightMove (UncheckedMoveType p N _ x y) = do
    when (knightP x y) $ cause (CheckedMovePiece p x y)
knightMove _ = return ()

promotionCheck :: Chess
promotionCheck (PromotionCheck c x p) = do
    when (rank c 7 == snd x) $ do
        promoting .= Just (x , p)
promotionCheck _ = return ()

pawnMove :: Chess
pawnMove (UncheckedMoveType p P target x y) = do
    let c = snd p
    let d = direction c

    let (ax, ay) = x
    let (bx, by) = y


    when (isNothing target && ax == bx && ay + d == by) $ do
        cause (PromotionCheck c y p)
        cause (CheckedMovePiece p x y)
pawnMove _ = return ()

pawnDoubleMove :: Chess
pawnDoubleMove (UncheckedMoveType p P target x y) = do
    let c = snd p
    let d = direction c

    let (ax, ay) = x
    let (bx, by) = y

    when (isNothing target && ax == bx && rank (snd p) 1 == ay && ay + 2 * d == by) $ do
        currentTurn <- use turn
        enpassant .= Just (moveNumber currentTurn + 1 , (ax , ay + d), y)
        cause $ PromotionCheck c y p
        cause $ CheckedMovePiece p x y
pawnDoubleMove _ = return ()

pawnCapture :: Chess
pawnCapture (UncheckedMoveType p P target x y)  = do
    let c = snd p
    let d = direction c

    let (ax, ay) = x
    let (bx, by) = y

    when (isJust target && abs (ax - bx) == 1 && ay + d == by) $ do
        cause $ PromotionCheck c y p
        cause $ CheckedMovePiece p x y
pawnCapture _ = return ()

pawnEP :: Chess
pawnEP (UncheckedMoveType p P _ x y)  = do
    let c = snd p
    let d = direction c

    let (ax, ay) = x
    let (bx, by) = y

    ep <- use enpassant
    currentTurn <- use turn

    forM_ ep $ \ (epTurn , epSquare , pSquare) -> do
        when (moveNumber currentTurn == epTurn && y == epSquare && abs (ax - bx) == 1 && ay + d == by) $ do
            cause $ Take p pSquare
            cause $ PromotionCheck c y p
            cause $ CheckedMovePiece p x y
pawnEP _ = return ()

promotion :: Chess
promotion _ = return () -- stop MoveEnd from advancing the turn and wait

clockRule :: Chess
clockRule _ = return () -- how can a rule best set a timer to `cause` an event?

rawTake :: Chess
rawTake (Take _ y) = cause $ PutTile y Nothing
rawTake _ = return ()

generalizeMove :: Chess
generalizeMove (CheckedMovePiece p x y)  = cause $ Move p x y
generalizeMove _ = return ()

logEvent :: Chess
logEvent PrintBoard = return ()
logEvent e = effect $ print e

squares :: [[Square]]
squares = (\y -> (,y) <$> [0..7]) <$> [0..7]

pieceSymbol :: PieceType -> String
pieceSymbol K = "K"
pieceSymbol Q = "Q"
pieceSymbol R = "R"
pieceSymbol B = "B"
pieceSymbol N = "N"
pieceSymbol P = "P"

pieceStr :: B.Bimap PieceType String
pieceStr = B.fromList [(K, "K"), (Q, "Q"), (R, "R"), (B, "B"), (N, "N"), (P, "p")]

promotable :: [PieceType]
promotable = [Q,R,B,N]

ppBoard :: ChessBoard -> String
ppBoard b = intercalate "\n" (concatMap symbol <$> boardArray)
    where
    boardArray = fmap (fmap $ flip M.lookup b) squares

    symbol :: Maybe Piece -> String
    symbol Nothing = "."
    symbol (Just p) = let c = pieceSymbol (fst p) in
        if snd p == White then c else toLower <$> c

movePrintBoard :: Chess
movePrintBoard MoveEnd = cause PrintBoard
movePrintBoard _ = return ()

moveStep :: ChessState -> Turn -> Turn
moveStep st t = case _promoting st of
    Just (_, (_, c)) -> Promoting (moveNumber t) c
    Nothing          -> Normal $ moveNumber t + 1

moveEnd :: Chess
moveEnd MoveEnd = do
    st <- get
    turn %= moveStep st
    cause NextSubTurn
moveEnd _ = return ()

promotionPrompt :: Chess
promotionPrompt NextSubTurn = do
    currentTurn <- use turn

    case currentTurn of
        Promoting _ c -> cause $ SendPromotionPrompt c
        Normal _ -> return ()
promotionPrompt _ = return ()

promote1 :: Chess
promote1 (Promote c str) = do
    currentTurn <- use turn

    r <- runExceptT $ do
        case currentTurn of
            Promoting _ c' -> do
                currentPromotion <- lift $ use promoting
                let x = (,) <$> B.lookupR str pieceStr <*> currentPromotion
                --effect $ print $ (B.lookupR str pieceStr :: Maybe PieceType, currentPromotion)
                whenJust x $ \ (pt , (a , _)) -> do
                    when (pt `elem` promotable && c == c') $ do
                        lift $ board . at a ?= (pt , c)
                        throwE ()
            _ -> return ()

    case r of
        Left ()  -> do
            promoting .= Nothing
            cause MoveEnd
        Right () -> do
            cause $ SendPromotionPrompt c
promote1 _ = return ()

printBoard :: Chess
printBoard PrintBoard = use board >>= effect . putStrLn . ppBoard >> effect (putStrLn "")
printBoard _ = return ()

disconnect :: Chess
disconnect (PlayerDisconnected c) = do
    connections %= M.delete c
    currentConnections <- use connections

    when (M.null currentConnections) $ cause CloseRoom
disconnect _ = return ()

winClose :: Chess
winClose (Win _) = cause CloseRoom
winClose _ = return ()