module Automata where

import Automata.Structure (Automata (..), Direction (..), Board, Instr, Op (..))
import qualified Data.Map as M
import Game (Game, registerRule)
import Data.Function ((&))
import Automata.Rules

{-
B R.R
. . .
. . .
.   .
R...R

2 2 N

sRr
-}


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

    parseLine x = map parseTile x

    parseTile ' ' = Nothing
    parseTile 'R' = Just (Just "Red", Nothing)
    parseTile 'B' = Just (Just "Blue", Nothing)
    parseTile '.' = Just (Nothing, Nothing)
    parseTile _ = error "parsing board"

parseInit :: IO (Int, Int, Direction)
parseInit = do
    xs <- getLine
    let [x, y, z] = words xs
    return (read x, read y, read z)

parseProg :: IO [Instr]
parseProg = do
    x <- getChar

    if x == '\n' then
        return []
    else do
        let y = case x of
                'R' -> Just "Red"
                'B' -> Just "Blue"
                _ -> Nothing
        k <- case y of
            Nothing -> return (parseOp x, Nothing)
            Just c  -> do
                z <- getChar
                return (parseOp z, Just c)
        xs <- parseProg
        return (k : xs)
        where
        parseOp 's' = Step
        parseOp 'r' = TurnR
        parseOp 'l' = TurnL
        parseOp 'x' = Reset
        parseOp y = error $ "parsing instruction: " ++ [y]

initial :: Automata
initial = MkAutomata
    { _board = mempty
    , _tape = []
    , _tapeIx = 0
    , _boardIx = (0, 0)
    , _dir = N
    }

parseAutomata :: IO Automata
parseAutomata = do
    bd <- parseBoard
    (x, y, d) <- parseInit
    prog <- parseProg

    return $  MkAutomata
        { _board = bd
        , _tape = prog
        , _tapeIx = 0
        , _boardIx = (x, y)
        , _dir = d
        }

pprintAutomata :: Automata -> String
pprintAutomata g = unlines $
    [[checkTile i j | i <- [0..x]] | j <- [0..y]]
    where
    checkTile i j = if (i, j) == _boardIx g
        then mkDir (_dir g)
        else mkTile (bd M.!? (i, j))

    x = maximum (fst <$> M.keys bd)
    y = maximum (snd <$> M.keys bd)

    mkDir N = '^'
    mkDir E = '>'
    mkDir S = 'v'
    mkDir W = '<'

    mkTile Nothing = ' '
    mkTile (Just (Nothing, _)) = '.'
    mkTile (Just (Just c, _)) = head c

    bd = _board g

automata :: Game Automata
automata = mempty
    & registerRule "step" step
    & registerRule "runOp" runOp
    & registerRule "die" die
    & registerRule "next" next

{-
- limited program size
- function calls
- painting
- marks
-}

{-
- no program, just a table of state, tape -> state, instruction
-}