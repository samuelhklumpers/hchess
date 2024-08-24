{-# LANGUAGE TypeFamilies, TupleSections, TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Lib (chessRunner, chessGame, chessServer) where

import qualified Data.Map as M
import qualified Data.Set as S
import Text.Parsec
    ( char,
      digit,
      letter,
      choice,
      getState,
      many,
      putState,
      runParser,
      Parsec, ParseError )
import Text.Parsec.Token (natural)

import Data.Either ( fromRight )
import Data.Maybe (catMaybes, isNothing, isJust, mapMaybe)
import Control.Monad.Trans.State.Lazy ( StateT(runStateT), get )
import Control.Monad.Trans.Writer.Lazy ( tell, WriterT (runWriterT) )
import Control.Lens ( makeLenses, use, (%=), (.=), at, Getter, to, (^.), view, (&), (.~), (?~), (%~), set, over )
import Control.Monad.Trans.Class ( MonadTrans(..) )
import Control.Monad (when, forM_, forever)
import Text.Parsec.Language (haskell)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate, uncons)
import Data.Char (toLower)
import Network.WebSockets
    ( acceptRequest,
      receiveDataMessage,
      runServer,
      Connection,
      ServerApp,
      WebSocketsData(fromDataMessage), sendBinaryData )
import Control.Concurrent.Timeout ( timeout )
import Data.Aeson (decode, ToJSON, FromJSON, encode)
import GHC.Generics (Generic)
import Control.Concurrent.STM
    ( atomically, newTMVarIO, readTMVar, takeTMVar, writeTMVar, TMVar )
import qualified Data.Bimap as B
import qualified Data.ByteString.Lazy as LB


cause :: (MonadTrans t) => a -> t (WriterT [a] IO) ()
cause = lift . tell . (:[])

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust = forM_

-- * Definitions
type Game s e = e -> StateT s (WriterT [e] IO) ()

combine :: Game s e -> Game s e -> Game s e
combine f g e = f e >> g e

compile :: Foldable t => t (Game s e) -> Game s e
compile = foldr1 combine

data Colour = Black | White deriving (Show, Eq, Ord, Generic)
data PieceType = K | Q | R | B | N | P deriving (Show, Eq, Generic)

type Piece = (PieceType, Colour)

type Rank = Int
type File = Int
type Square = (File, Rank)

type Room = String
type PlayerName = String
type PlayerId = (Room, PlayerName)

type ChessBoard = M.Map Square Piece
data ChessState = ChessState
    { _board :: ChessBoard , _turn :: Int, _castling :: S.Set Square
    , _enpassant :: Maybe (Int, Square, Square) , _zeroing :: Int
    , _touch :: M.Map Colour (Square, Piece) , _players :: B.Bimap String Colour
    , _connections :: M.Map Colour PlayerId } deriving (Show, Eq)
makeLenses ''ChessState

toMove :: Getter ChessState Colour
toMove = turn . to colour
    where
    colour :: Int -> Colour
    colour x | even x = White
    colour _ = Black

-- * Setup
fenParser :: Parsec String Square ChessBoard
fenParser = M.fromList . catMaybes <$> many tokenFEN
    where
    letters :: M.Map Char Piece
    letters = M.fromList [
            ('K', (K, White)),
            ('Q', (Q, White)),
            ('R', (R, White)),
            ('B', (B, White)),
            ('N', (N, White)),
            ('P', (P, White)),
            ('k', (K, Black)),
            ('q', (Q, Black)),
            ('r', (R, Black)),
            ('b', (B, Black)),
            ('n', (N, Black)),
            ('p', (P, Black))
        ]


    tokenFEN :: Parsec String Square (Maybe (Square, Piece))
    tokenFEN = choice [
            parsePiece,
            parseSkip,
            parseLine
        ]
        where
        parsePiece :: Parsec String Square (Maybe (Square, Piece))
        parsePiece = do
            p <- letter
            (x , y) <- getState
            putState (x + 1, y)

            return $ ((x, y),) <$> M.lookup p letters

        parseSkip :: Parsec String Square (Maybe (Square, Piece))
        parseSkip = do
            d <- digit
            let n = (read [d] :: Int)
            (x , y) <- getState
            putState (x + n, y)

            return Nothing

        parseLine :: Parsec String Square (Maybe (Square, Piece))
        parseLine = do
            _ <- char '/'
            (_ , y) <- getState
            putState (0, y + 1)
            return Nothing


parseFEN :: String -> Either ParseError ChessBoard
parseFEN = runParser fenParser (0 , 0) ""

chessInitial :: ChessState
chessInitial = ChessState initialBoard 0 (S.fromList [(0, 0), (4, 0), (7, 0), (0, 7), (4, 7), (7, 7)]) Nothing 0 M.empty B.empty M.empty
    where
    initialFEN :: String
    initialFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"

    initialBoard :: ChessBoard
    initialBoard = fromRight undefined $ parseFEN initialFEN

-- * Evaluation
recGame :: Game s e -> Runner s e
recGame _ s [] = return s
recGame g s (e : es) = do
    ((_ , s') , es') <- runWriterT (runStateT (g e) s)
    s'' <- recGame g s' es'
    recGame g s'' es


type Runner s e = s -> [e] -> IO s

runGame :: Runner s e -> IO e -> s -> IO s
runGame runner input s = do
    e <- input
    s' <- runner s [e]
    runGame runner input s'


-- * Rules
data ChessEvent = UncheckedMove Square Square | UncheckedMovePiece Piece Square Square | UncheckedMoveType Piece PieceType (Maybe Piece) Square Square
    | Touch Colour Square | Touch1 Colour Square | Touch1Turn Colour Square | Touch1Piece Colour Piece Square | Touch2 Piece Square Square
    | CheckedMovePiece Piece Square Square | Take Piece Square | RawMove Piece Square Square
    | Move Piece Square Square | Capture Piece Square Square
    | NonCapture Piece Square Square | Set Square Piece | PrintBoard
    | TimedOut Colour | PlayerConnected Colour
    | SendBoard Colour | SendTile Colour Square | SendBoardAll | SendTileAll Square | PutTile Square (Maybe Piece)
    | SendTurn Colour | SendRaw Colour ChessOutMessage | MoveEnd | Win Colour deriving Show


type Chess = Game ChessState ChessEvent

-- When `c` touches `y`, if `c` touched `x` right before, process the input `Touch2 c x y`, otherwise check if we should remember `Touch1 c y`
touch1 :: Chess
touch1 (Touch c y) = do
    touchMemory <- use touch
    case M.lookup c touchMemory of
        Nothing -> cause $ Touch1 c y
        Just (x , p)  -> do
            touch %= M.delete c
            cause $ Touch2 p x y
touch1 _ = return ()

-- If `c` is not to move, then don't
touch1Turn :: Chess
touch1Turn (Touch1 c x) = do
    currentToMove <- use toMove
    when (currentToMove == c) $ cause (Touch1Turn c x)
touch1Turn _ = return ()

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

-- Temporary
touch2Colour :: Chess
touch2Colour (Touch2 p x y) = do
    target <- M.lookup y <$> use board
    case target of
        Nothing -> cause $ UncheckedMove x y
        Just p'  -> when (snd p' /= snd p) $ cause $ UncheckedMove x y
touch2Colour _ = return ()

moveMayCapture :: Chess
moveMayCapture (Move p x y) = do
    target <- M.lookup y <$> use board
    case target of
        Nothing -> cause $ NonCapture p x y
        Just _  -> cause $ Capture p x y
    cause $ MoveEnd
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
            Just _  -> return ()
winRule _ = return ()

sendWin :: Chess
sendWin (Win c) = do
    let c' = if c == White then Black else White

    cause $ SendRaw c $ Status "You win :)"
    cause $ SendRaw c' $ Status "You lose :("
sendWin _ = return ()

data ChessOutMessage = Board ChessBoard | Tile Square (Maybe Piece) | Turn Colour | Status String deriving (Show, Eq, Generic)

instance ToJSON ChessOutMessage where
instance FromJSON ChessOutMessage where
instance ToJSON Colour where
instance FromJSON Colour where
instance ToJSON PieceType where
instance FromJSON PieceType where

serverRule :: TMVar (M.Map PlayerId Connection) -> Chess
serverRule connRef e = do
    case e of
        (PlayerConnected c) -> do
            cause $ SendBoard c
        (SendTile c a)  -> do
            p <- use $ board . at a
            cause $ SendRaw c $ Tile a p
        (SendBoard c)   -> do
            currentBoard <- use board
            cause $ SendRaw c $ Board currentBoard
        SendBoardAll    -> do
            cause $ SendBoard White
            cause $ SendBoard Black
        (SendTileAll a) -> do
            cause $ SendTile White a
            cause $ SendTile Black a
        (SendTurn c)    -> do
            cause $ SendRaw White $ Turn c
            cause $ SendRaw Black $ Turn c
        (SendRaw c d)   -> do
            conns <- liftIO $ atomically $ readTMVar connRef
            connMap <- use connections

            whenJust (M.lookup c connMap >>= \ x -> M.lookup x conns) $ \ conn -> do
                liftIO $ sendBinaryData conn $ encode d
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

        when (c `S.member` castle && a `S.member` castle && rookP s c d) $ do
            rookMaybe <- use $ board . at c
            castling %= S.delete c

            whenJust rookMaybe $ \ rook -> do
                castling %= S.delete a

                cause $ RawMove rook c d
                cause $ CheckedMovePiece p a b
castlingMove _ = return ()

enumFromToLR' :: (Enum a, Ord a) => a -> a -> [a]
enumFromToLR' x y
    | x < y = enumFromTo x y
    | otherwise = reverse $ enumFromTo y x

enumFromTo' :: (Enum a, Ord a, Num a) => a -> a -> [a]
enumFromTo' x y = enumFromTo (min x y + 1) (max x y - 1)

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

pawnMove :: Chess
pawnMove (UncheckedMoveType p P target x y) = do
    let d = direction (snd p)

    let (ax, ay) = x
    let (bx, by) = y

    when (isNothing target && ax == bx && ay + d == by) $ cause (CheckedMovePiece p x y)
pawnMove _ = return ()

pawnDoubleMove :: Chess
pawnDoubleMove (UncheckedMoveType p P target x y) = do
    let d = direction $ snd p

    let (ax, ay) = x
    let (bx, by) = y

    when (isNothing target && ax == bx && rank (snd p) 1 == ay && ay + 2 * d == by) $ do
        currentTurn <- use turn
        enpassant .= Just (currentTurn + 1 , (ax , ay + d), y)
        cause (CheckedMovePiece p x y)
pawnDoubleMove _ = return ()

pawnCapture :: Chess
pawnCapture (UncheckedMoveType p P target x y)  = do
    let d = direction $ snd p

    let (ax, ay) = x
    let (bx, by) = y

    when (isJust target && abs (ax - bx) == 1 && ay + d == by) $ do
        cause $ CheckedMovePiece p x y
pawnCapture _ = return ()

pawnEP :: Chess
pawnEP (UncheckedMoveType p P _ x y)  = do
    let d = direction $ snd p

    let (ax, ay) = x
    let (bx, by) = y

    ep <- use enpassant
    currentTurn <- use turn

    forM_ ep $ \ (epTurn , epSquare , pSquare) -> do
        when (currentTurn == epTurn && y == epSquare && abs (ax - bx) == 1 && ay + d == by) $ do
            cause $ Take p pSquare
            cause $ CheckedMovePiece p x y
pawnEP _ = return ()

rawTake :: Chess
rawTake (Take _ y) = cause $ PutTile y Nothing
rawTake _ = return ()

generalizeMove :: Chess
generalizeMove (CheckedMovePiece p x y)  = cause $ Move p x y
generalizeMove _ = return ()

logEvent :: Chess
logEvent PrintBoard = return ()
logEvent e = liftIO $ print e

squares :: [[Square]]
squares = (\y -> (,y) <$> [0..7]) <$> [0..7]

pieceSymbol :: PieceType -> String
pieceSymbol K = "K"
pieceSymbol Q = "Q"
pieceSymbol R = "R"
pieceSymbol B = "B"
pieceSymbol N = "N"
pieceSymbol P = "P"

ppBoard :: ChessBoard -> String
ppBoard b = intercalate "\n" (concatMap symbol <$> boardArray)
    where
    boardArray = fmap (fmap $ flip M.lookup b) squares

    symbol :: Maybe Piece -> String
    symbol Nothing = "."
    symbol (Just p) = let c = pieceSymbol (fst p) in
        if snd p == White then c else toLower <$> c

movePrintBoard :: Chess
movePrintBoard (Capture {}) = cause PrintBoard
movePrintBoard (NonCapture {}) = cause PrintBoard
movePrintBoard _ = return ()

moveEnd :: Chess
moveEnd MoveEnd = do
    turn %= (+1)
moveEnd _ = return ()

printBoard :: Chess
printBoard PrintBoard = use board >>= liftIO . putStrLn . ppBoard >> liftIO (putStrLn "")
printBoard _ = return ()

chess :: Game ChessState ChessEvent
chess = compile [
        --logEvent, printBoard, movePrintBoard,
        touch1, touch1Turn, touch1Piece, touch1Colour, touch2Colour, uncheckedMovePiece, specializeMove,
        kingMove, generalizeMove, capture, nonCapture, moveMayCapture, pawnMove, moveEnd,
        pawnDoubleMove, pawnCapture, pawnEP, rawTake, rawMove, queenMove, rookMove, bishopMove, knightMove, castlingMove,
        putTile, winRule, sendWin
    ]

chessInput :: IO ChessEvent
chessInput = do
    str <- getLine

    case runParser parseSquare () "" str of
        Left _  -> chessInput
        Right y -> return y

secondUs :: Integer
secondUs = 1000 * 1000

data ChessMessage = TouchMsg Square | Register String String deriving (Show, Eq, Generic)

instance ToJSON ChessMessage where
instance FromJSON ChessMessage where

interpretMessage :: Colour -> ChessMessage -> Maybe ChessEvent
interpretMessage p (TouchMsg a) = Just $ Touch p a
interpretMessage _ _ = Nothing

withTMVarIO :: TMVar a -> (a -> IO a) -> IO ()
withTMVarIO var f = do
    val  <- atomically $ readTMVar var
    val' <- f val
    atomically $ writeTMVar var val'


chessApp :: TMVar (M.Map PlayerId Connection) -> TMVar (M.Map String (TMVar ChessState)) -> ServerApp
chessApp connRef refGames pending = do
    conn <- acceptRequest pending

    maybeMsg <- timeout (30 * secondUs) $ receiveDataMessage conn
    let maybeRoom = maybeMsg >>= decode . fromDataMessage

    whenJust maybeRoom $ \case
        Register room ident -> do
            games <- atomically $ readTMVar refGames

            refGame <- case view (at room) games of
                Just refGame -> do
                    return refGame
                Nothing -> do
                    refGame <- newTMVarIO chessInitial
                    atomically $ writeTMVar refGames (games & at room ?~ refGame)
                    return refGame

            game <- atomically $ readTMVar refGame
            let playerMap = view players game

            maybeColour <- case B.lookup ident playerMap of
                Just colour -> do
                    return $ Just colour -- TODO this is somewhat bad if the socket is still connected because now you can easily log in twice for better or worse
                Nothing     -> case B.lookupR White playerMap of
                    Just _  -> case B.lookupR Black playerMap of
                        Just _  -> do
                            return Nothing
                        Nothing -> do
                            atomically $ writeTMVar refGame (game & players %~ B.insert ident Black)
                            return $ Just Black
                    Nothing -> do
                        atomically $ writeTMVar refGame (game & players %~ B.insert ident White)
                        return $ Just White

            whenJust maybeColour $ \ colour -> mainloop colour (room , ident) refGame conn
        _ -> return ()

    where
    mainloop :: Colour -> PlayerId -> TMVar ChessState -> Connection -> IO ()
    mainloop colour ident st conn = do
        withTMVarIO connRef (return . M.insert ident conn)
        withTMVarIO st (return . over connections (M.insert colour ident))

        withTMVarIO st (`runner` [PlayerConnected colour])

        forever $ do
            maybeMsg <- timeout (10 * 60 * secondUs) $ receiveDataMessage conn
            let maybeEvent = maybeMsg >>= decode . fromDataMessage >>= interpretMessage colour

            whenJust maybeEvent $ \ ev -> do
                withTMVarIO st (`runner` [ev])
            where
            runner = recGame $ combine chess (serverRule connRef)

            -- TODO you should probably be able to connect/disconnect/reconnect/crash/timeout


chessServer :: IO ()
chessServer = do
    refGames <- newTMVarIO M.empty
    refConn  <- newTMVarIO M.empty

    runServer "0.0.0.0" 12345 (chessApp refConn refGames)

parseSquare :: Parsec String () ChessEvent
parseSquare = do
    c <- choice [char 'W' >> return White , char 'B' >> return Black]
    _ <- char ' '
    x <- fromInteger <$> natural haskell
    y <- fromInteger <$> natural haskell

    return $ Touch c (x, y)

chessRunner :: Runner ChessState ChessEvent
chessRunner = recGame chess

chessGame :: IO ChessState
chessGame = runGame chessRunner chessInput chessInitial

testEnPassant :: IO ()
testEnPassant = do
    newState <- chessRunner chessInitial
        [ Touch White (4, 6), Touch White (4, 4)
        , Touch Black (0, 1), Touch Black (0, 2)
        , Touch White (4, 4), Touch White (4, 3)
        , Touch Black (3, 1), Touch Black (3, 3)
        , Touch White (4, 3), Touch White (3, 2)
        , Touch Black (0, 2), Touch Black (0, 3)
        , Touch White (3, 2), Touch White (2, 1)]

    let targetBoard = parseFEN "rnbqkbnr/1pP1pppp/8/p7/8/8/PPPP1PPP/RNBQKBNR"
    let targetState = chessInitial {
        _board = fromRight undefined targetBoard,
        _turn  = 7,
        _enpassant = Just (4,(3,2),(3,3))
    }

    if newState == targetState then
        putStrLn "All good!"
    else
        putStrLn ":("

testLongCastle :: IO ()
testLongCastle = do
    newState <- chessRunner chessInitial
        [ Touch White (3, 6), Touch White (3, 5)
        , Touch Black (2, 1), Touch Black (2, 2)
        , Touch White (1, 7), Touch White (2, 5)
        , Touch Black (3, 1), Touch Black (3, 2)
        , Touch White (2, 7), Touch White (4, 5)
        , Touch Black (4, 1), Touch Black (4, 2)
        , Touch White (3, 7), Touch White (3, 6)
        , Touch Black (5, 1), Touch Black (5, 2)
        , Touch White (4, 7), Touch White (2, 7)]

    let targetBoard = parseFEN "rnbqkbnr/pp4pp/2pppp2/8/8/2NPB3/PPPQPPPP/2KR1BNR"
    let targetState = chessInitial {
        _board = fromRight undefined targetBoard,
        _turn  = 9,
        _castling = S.fromList [(0, 0), (4, 0), (7, 0), (7, 7)]
    }

    if newState == targetState then
        putStrLn "All good!"
    else
        putStrLn ":("