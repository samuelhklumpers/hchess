{-# LANGUAGE LambdaCase #-}
module Automata where

import qualified Data.Map as M
import Game (Game, registerRule, runGame', mkEvent)
import Data.Function ((&))

import Automata.Structure (Automata (..), Direction (..), Board, Instr, Op (..), AutomataLevels (..), aut)
import Automata.Rules
import Text.Parsec (parse, char, oneOf, digit, Parsec)
import Data.Either (fromRight)
import Text.Parsec.Combinator


parseBoard :: IO Board
parseBoard = mkBoard 0 <$> go
    where
    mkBoard j (x:xs) = mkLine 0 j x (mkBoard (j + 1) xs)
    mkBoard j [] = mempty

    mkLine i j [] m = m
    mkLine i j (x:xs) m = mkLine (i + 1) j xs (maybe m (\ x -> M.insert (i, j) x m) x)

    go = do
        x <- getLine

        if null x then
            return []
        else do
            xs <- go
            return (parseLine x : xs)

    parseTile ' ' = Nothing
    parseTile 'R' = Just (Just "Red", Nothing)
    parseTile 'B' = Just (Just "Blue", Nothing)
    parseTile '.' = Just (Nothing, Nothing)
    parseTile _ = error "parsing board"

    parseLine = map parseTile

parseInit :: IO (Int, Int, Direction)
parseInit = do
    xs <- getLine
    let [x, y, z] = words xs
    return (read x, read y, read z)

initial :: Automata
initial = MkAutomata
    { _board = mempty
    , _tapes = []
    , _iPtr = (0, 0)
    , _stack = []
    , _boardIx = (0, 0)
    , _dir = N
    }

{-
parseAutomata :: IO Automata
parseAutomata = do
    bd <- parseBoard
    (x, y, d) <- parseInit
    tapes <- parseTapes

    return $  MkAutomata
        { _board = bd
        , _tapes = tapes
        , _iPtr = (0, 0)
        , _stack = []
        , _boardIx = (x, y)
        , _dir = d
        }
-}

automata :: Game AutomataLevels
automata = mempty
    & registerRule "loadLevel"  loadLevel
    & registerRule "askSubmit"  (submit    aut)
    & registerRule "step"       (step      aut)
    & registerRule "runOp"      (runOp     aut)
    & registerRule "next"       (next      aut)
    & registerRule "endStep"    (showAndWait 100 aut)
    & registerRule "die"        die
    & registerRule "star"       win1

automataMain :: IO ()
automataMain = do
    _ <- runGame' automata (MkAutomataLevels initial 0) [mkEvent "loadLevel" ()]
    return ()

{-
automataMain :: IO () 
automataMain = do
    g <- parseAutomata
    go g (runGame' automata)
    where
    go g r = do
        putStrLn $ pprintAutomata g
        _ <- getLine
        g' <- r g [mkEvent "step" ()]
        go g' r
-}

{-
- limited program size
- function calls
- painting
- marks
-}

{-
- no program, just a table of state, tape -> state, instruction
-}