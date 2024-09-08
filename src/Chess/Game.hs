{-# LANGUAGE TypeFamilies, TupleSections, TemplateHaskell, DeriveGeneric, OverloadedStrings, LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Chess.Game ( module Chess.Game ) where

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

import Data.Either ( fromRight )
import Data.Maybe (catMaybes, mapMaybe)
import Control.Monad.Trans.State.Lazy ( StateT(runStateT) )
import Control.Monad.Trans.Writer.Lazy ( tell, Writer, runWriter )
import Control.Lens ( makeLenses, Getter, to )
import Control.Monad.Trans.Class ( MonadTrans(..) )
import Control.Monad (forM_)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Bimap as B


-- What does this do?
-- > IO cannot influence the game! (IO must come from the driver)
data Action e = Event e | Effect (IO ())

cause :: (MonadTrans t) => a -> t (Writer [Action a]) ()
cause = lift . tell . (:[]) . Event

effect :: (MonadTrans t) => IO () -> t (Writer [Action a]) ()
effect = lift . tell . (:[]) . Effect

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust = forM_

type Game s e = e -> StateT s (Writer [Action e]) ()

combine :: Game s e -> Game s e -> Game s e
combine f g e = f e >> g e

compile :: Foldable t => t (Game s e) -> Game s e
compile = foldr1 combine

data Colour = Black | White deriving (Show, Eq, Ord, Generic)
data PieceType = K | Q | R | B | N | P deriving (Show, Eq, Generic, Ord)

type Piece = (PieceType, Colour)

type Rank = Int
type File = Int
type Square = (File, Rank)
type Move = (Piece, Square, Square)
type Capture = (Move, Piece)

type Room = String
type PlayerName = String
type PlayerId = (Room, PlayerName)

instance ToJSON Colour where
instance FromJSON Colour where
instance ToJSON PieceType where
instance FromJSON PieceType where


data Turn = Normal Int | Promoting Int Colour deriving (Show, Eq)

pieceImage :: PieceType -> String
pieceImage K = "king.svg"
pieceImage Q = "queen.svg"
pieceImage R = "rook.svg"
pieceImage B = "bishop.svg"
pieceImage N = "knight.svg"
pieceImage P = "pawn.svg"

moveNumber :: Turn -> Int
moveNumber (Normal n) = n
moveNumber (Promoting n _) = n

type ChessBoard = M.Map Square Piece
data ChessState = ChessState
    { _board :: ChessBoard , _turn :: Turn, _castling :: S.Set Square
    , _enpassant :: Maybe (Int, Square, Square) , _zeroing :: Int , _history :: M.Map ChessBoard Int
    , _touch :: M.Map Colour (Square, Piece) , _players :: B.Bimap String Colour
    , _promoting :: Maybe (Square, Piece)
    , _moves :: M.Map Colour [Move], _captures :: M.Map Colour [Capture] 
    , _connections :: M.Map Colour PlayerId , _running :: Bool } deriving (Show, Eq)
makeLenses ''ChessState

-- in Agda you would make this a dependent map and just add types on the fly
-- https://hackage.haskell.org/package/dependent-map-0.4.0.0/docs/Data-Dependent-Map.html
-- https://github.com/obsidiansystems/dependent-map
-- see Test.hs.bak


chessInitial :: ChessState
chessInitial = ChessState {
        _board       = initialBoard,
        _turn        = Normal 0,
        _castling    = S.fromList [(0, 0), (4, 0), (7, 0), (0, 7), (4, 7), (7, 7)],
        _enpassant   = Nothing,
        _touch       = M.empty,
        _zeroing     = 0,
        _history     = M.empty,
        _players     = B.empty,
        _promoting   = Nothing,
        _connections = M.empty,
        _moves       = M.empty,
        _captures    = M.empty,
        _running     = True
    }
    where
    initialFEN :: String
    initialFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"

    initialBoard :: ChessBoard
    initialBoard = fromRight undefined $ parseFEN initialFEN


toMove :: Getter ChessState (Maybe Colour)
toMove = turn . to colour
    where
    colour :: Turn -> Maybe Colour
    colour (Promoting _ _) = Nothing
    colour (Normal x) | even x = Just White
    colour (Normal _)          = Just Black

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

-- * Evaluation
recGame :: Game s e -> Runner s e
recGame _ s [] = return s
recGame g s (a : es) = case a of 
    Event e -> do
        let ((_ , s') , es') = runWriter (runStateT (g e) s)
        s'' <- recGame g s' es'
        recGame g s'' es
    Effect e -> do
        e 
        recGame g s es

fromEvent :: Action e -> Maybe e
fromEvent (Event e) = Just e
fromEvent _ = Nothing

simulate :: Game s e -> s -> [e] -> s
simulate _ s [] = s
simulate g s (e : queue') =
        let ((_ , s') , consequences) = runWriter (runStateT (g e) s) in
        let s'' = simulate g s' (mapMaybe fromEvent consequences) in
        simulate g s'' queue'

simulateUntil :: (e -> Bool) -> Game s e -> s -> [e] -> (s , [e])
simulateUntil _ _ s [] = (s , [])
simulateUntil p g s queue@(e : queue')
    | p e       = (s , queue)
    | otherwise = 
        let ((_ , s') , consequences) = runWriter (runStateT (g e) s) in
        let (s'', remainder) = simulateUntil p g s' (mapMaybe fromEvent consequences) in
        if null remainder then
            simulateUntil p g s'' queue'
        else
            (s'' , remainder ++ queue')
{-
simulateUntilDbg :: Show e => (e -> Bool) -> Game s e -> s -> [e] -> IO (s , [e])
simulateUntilDbg _ _ s [] = return  (s , [])
simulateUntilDbg p g s queue@(e : queue')
    | p e       = print e >> return (s , queue)
    | otherwise = do
        print e
        let ((_ , s') , consequences) = runWriter (runStateT (g e) s)
        (s'', remainder) <- simulateUntilDbg p g s' (mapMaybe fromEvent consequences)
        if null remainder then
            simulateUntilDbg p g s'' queue'
        else
            return (s'' , remainder ++ queue')
-}

type Runner s e = s -> [Action e] -> IO s

runGame :: Runner s e -> IO e -> s -> IO s
runGame runner input s = do
    e <- input
    s' <- runner s [Event e]
    runGame runner input s'



data ChessOutMessage = Board (M.Map Square (String , Colour)) | Tile Square (Maybe (String , Colour)) String | Turn Colour
    | Status String | Promotion | MarkAvailableMove Square | ClearAvailableMoves deriving (Show, Eq, Generic)

instance ToJSON ChessOutMessage where
instance FromJSON ChessOutMessage where


data ChessEvent = UncheckedMove Square Square | UncheckedMovePiece Piece Square Square | UncheckedMoveType Piece PieceType (Maybe Piece) Square Square
    | Touch Colour Square | Touch1 Colour Square | Touch1Turn Colour Square | Touch1Piece Colour Piece Square | Touch2 Piece Square Square
    | CheckedMovePiece Piece Square Square | Take Piece Square | RawMove Piece Square Square
    | Move Piece Square Square | Capture Piece Square Square | PromotionCheck Colour Square Piece
    | NonCapture Piece Square Square | Set Square Piece | PrintBoard
    | TimedOut Colour | PlayerConnected Colour
    | SendBoard Colour | SendTile Colour Square | SendDrawTile Colour Square (Maybe Piece) String | SendBoardAll | SendTileAll Square | PutTile Square (Maybe Piece)
    | SendSelect Colour Square Bool | NextSubTurn | Zeroing
    | SendTurn Colour | SendRaw Colour ChessOutMessage | MoveEnd | Win Colour | Draw | PlayerDisconnected Colour
    | CloseRoom | UpdateSelection Colour Square | SendPromotionPrompt Colour | Promote Colour String
    | NextTurn | SendMarkAvailableMove Colour Square | SendClearAvailableMoves Colour 
    | UncheckedMoveSelf Piece Square Square | UncheckedMoveTurn Piece Square Square deriving Show



