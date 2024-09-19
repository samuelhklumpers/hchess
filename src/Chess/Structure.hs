{-# LANGUAGE TypeFamilies, DeriveGeneric, OverloadedStrings, LambdaCase,
    ScopedTypeVariables, TemplateHaskell, TupleSections #-}


module Chess.Structure ( module Chess.Structure ) where

import qualified Data.Map as M
import Text.Parsec
    ( char,
      choice,
      runParser,
      Parsec, ParseError, getState, putState, digit, letter, many )
import Control.Lens
    ( makeLenses, Getter, to, at, (^.) )
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import qualified Data.Bimap as B
import Data.Either (fromRight)
import qualified Data.Set as S
import Data.Maybe (catMaybes, mapMaybe)
import Type.Reflection (Typeable)

import Chess.Internal
import Data.List (intercalate)
import Data.Char (toLower)

-- * Gamestate for Chess

data Colour = Black | White deriving (Show, Eq, Ord, Generic)
data PieceType = K | Q | R | B | N | P deriving (Show, Eq, Generic, Ord)

type Piece = (PieceType, Colour)

type Rank = Int
type File = Int

type Square = (File, Rank)

    
data PlayerTouch = PlayerTouch Colour Square deriving (Show, Eq, Typeable)
data PlayerSelect = PlayerSelect Colour Square Bool deriving (Show, Eq, Typeable)
type HTMLColour = String
data SendDrawTile = SendDrawTile Colour Square (Maybe Piece) HTMLColour deriving (Show, Eq, Typeable)
data TouchPiece = TouchPiece Colour Piece Square deriving (Show, Eq, Typeable)
data Move = Move Square Square deriving (Show, Eq, Typeable)
data PieceMove = PieceMove Piece Square Square deriving (Show, Eq, Typeable)
data Capture = Capture Piece Square Square Piece deriving (Show, Eq, Typeable)
data Take = Take Piece Square Piece deriving (Show, Eq, Typeable)
data PromotionCheck = PromotionCheck Colour Square Piece deriving (Show, Eq, Typeable)
data PutTile = PutTile Square (Maybe Piece) deriving (Show, Eq, Typeable)

data ChessOutMessage = Board (M.Map Square (String , Colour)) | Tile Square (Maybe (String , Colour)) String | Turn Colour
    | Status String | Promotion | MarkAvailableMove Square | ClearAvailableMoves deriving (Show, Eq, Generic)

instance ToJSON ChessOutMessage where
instance FromJSON ChessOutMessage where

data SendRaw = SendRaw [Colour] ChessOutMessage deriving (Show, Eq, Typeable) 

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
    , _enpassant :: Maybe (Int, Square, Square, Piece) , _zeroing :: Int , _history :: M.Map ChessBoard Int
    , _touch :: M.Map Colour (Square, Piece) , _players :: B.Bimap String Colour
    , _promoting :: Maybe (Square, Piece)
    , _moveCache :: M.Map Colour [Move], _captureCache :: M.Map Colour [Capture]
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
        _moveCache       = M.empty,
        _captureCache    = M.empty,
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

cmap :: Maybe Piece -> Bool -> String
cmap _     True  = "#FF0000"
cmap (Just (_ , White)) False = "#FFFFFF"
cmap (Just (_ , Black)) False = "#000000"
cmap _ _ = ""

direction :: Colour -> Int
direction White = -1
direction Black = 1

rank :: Colour -> Int -> Int
rank White x = 7 - x
rank Black x = x


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
