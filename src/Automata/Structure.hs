{-# LANGUAGE TemplateHaskell #-}

module Automata.Structure ( module Automata.Structure ) where

import qualified Data.Map as M
import Control.Lens.TH (makeLenses)

-- | Where am I?
type Ix = (Int, Int)

-- | Where am I going?
data Direction = N | E | S | W deriving (Show, Read, Eq, Enum, Bounded)

-- | What am I standing on?
type Board = M.Map Ix Tile
type Tile = (Maybe Colour, Maybe Mark)
type Mark = String
type Colour = String

-- | What should I do?
data Op = Step | TurnL | TurnR | Reset deriving (Show, Eq)
type Instr = (Op, Maybe Colour)

data Automata = MkAutomata
    { _board :: Board
    , _tape  :: [Instr]
    , _tapeIx :: Int
    , _boardIx :: Ix
    , _dir :: Direction
    } deriving (Show, Eq)
makeLenses ''Automata

