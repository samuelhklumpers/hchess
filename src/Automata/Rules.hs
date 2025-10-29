{-# LANGUAGE RankNTypes #-}

module Automata.Rules where

import GHC.Stack (HasCallStack)
import Control.Lens.Combinators (use, At (..), Ixed (..), pre, to, _head, Lens', Zoom (..))

import Game (Rule, cause, effect)
import Automata.Structure
import Control.Lens.Operators ((%=), (.=))
import Data.Maybe (isJust)
import Internal (cycleNext, cyclePrev)
import Text.Parsec
import Data.Either (fromRight)
import qualified Data.Map as M
import Control.Concurrent (threadDelay)
import Control.Monad.Trans.State (get)
import System.IO (withFile, IOMode (ReadMode), hGetContents', stdout, hFlush)
import Data.Foldable (Foldable(..))
import Debug.Trace (traceM)
import Control.Monad (when)
import Data.List (intercalate)


step :: HasCallStack => Lens' s Automata -> Rule s ()
step aut () = zoom aut $ do
    pos <- use boardIx
    mTile <- use $ board . at pos

    p@(i, j) <- use iPtr
    mInstr <- use $ tapes . pre (ix i . ix j)

    -- tp <- use tapes
    -- traceM $ show (pos, i, j, mInstr, tp)

    st <- use stack
    effect $ putStrLn $ intercalate "->" (fmap show (p:st))

    case mTile of
        Nothing -> cause "die" ()
        Just tile -> do
            if snd tile == Just "*" then
                cause "star" ()
            else case mInstr of
                Nothing -> do
                    frame <- use (stack . pre _head)
                    stack %= tail
                    case frame of
                        Nothing -> cause "die" ()
                        Just ptr' -> do
                            iPtr .= ptr'
                            cause "step" ()
                Just instr -> cause "runOp" (instr, tile, pos, p)


direction :: Direction -> (Int, Int)
direction N = (0, -1)
direction E = (1, 0)
direction S = (0, 1)
direction W = (-1, 0)

vadd :: Num a => (a, a) -> (a, a) -> (a, a)
vadd (x, y) (v, w) = (x + v, y + w)

matchColour :: Maybe Colour -> Maybe Colour -> Bool
matchColour Nothing _ = True
matchColour (Just c) (Just c') = c == c'
matchColour _ _ = False

runOp :: HasCallStack => Lens' s Automata -> Rule s (Instr, Tile, Ix, (Int, Int))
runOp aut ((op, ocol), (tcol, _), pos, ptr@(i, j)) = zoom aut $ do
    if matchColour ocol tcol then
        case op of
            Step -> do
                dx <- use (dir . to direction)
                let pos' = vadd pos dx
                ok <- use $ board . at pos' . to isJust

                if ok then do
                    boardIx .= pos'
                    cause "next" ptr
                else
                    cause "die" ()
            TurnL -> do
                dir %= cyclePrev
                cause "next" ptr
            TurnR -> do
                dir %= cycleNext
                cause "next" ptr
            Call i' -> do
                iPtr .= (i', 0)
                Just tl <- use $ tapes . pre (ix i . to length)
                when (j + 1 < tl) $ -- tail-call optimization
                    stack %= ((i, j + 1) :)
                cause "endStep" ()
            Paint _ -> error "no you don't"
    else
        cause "next" ptr

die :: HasCallStack => Rule s ()
die () = do
    effect $ do
        putStrLn "You died :("
        putStrLn ""
    cause "loadLevel" ()

next :: HasCallStack => Lens' s Automata -> Rule s (Int, Int)
next aut (i, j) = zoom aut $ do
    iPtr .= (i, j + 1)
    cause "endStep" ()

parseTapes :: IO [[Instr]]
parseTapes = go (0 :: Int)    
    where
    go i = do
        putStr ("f" ++ show i ++ ": ")
        hFlush stdout
        xs <- getLine
        
        if null xs then
            return []
        else do
            let t = either (\ x -> error $ "error parsing program: " ++ xs ++ ",\n" ++ show x) id (parse tapeParser "" xs)
            fmap (t :) (go (i + 1))

intP :: Parsec String u Int
intP = read <$> many1 digit

tapeParser :: Parsec String u [Instr]
tapeParser = flip sepBy (char ' ') $ do
    c <- fmap (:[]) <$> optionMaybe (oneOf "br")
    o <- choice
        [ char '>' >> pure Step
        , char 'L' >> pure TurnL
        , char 'R' >> pure TurnR
        , char 'f' >> fmap Call intP
        ]
    return (o, c)

submit :: Lens' s Automata -> Rule s ()
submit aut () = zoom aut $ do
    x <- get
    tapes' <- effect $ do
        putStrLn $ pprintAutomata x
        putStrLn ""
        putStrLn "please enter your program below, press Enter twice to finalize:"
        parseTapes

    tapes .= tapes'
    cause "step" ()

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
    mkTile (Just (Nothing, mark)) = maybe '.' head mark
    mkTile (Just (Just c, _)) = head c

    bd = _board g

showAndWait :: Int -> Lens' s Automata -> Rule s ()
showAndWait ms aut () = do
    x <- use aut
    effect $ do
        putStrLn $ pprintAutomata x
        threadDelay (1000 * ms)
        -- this is bad behaviour 
        -- I'll let you know when I figure out how to push events back on the queue from forkIO
    cause "step" ()

win1 :: Rule AutomataLevels ()
win1 () = do
    effect $ do
        putStrLn "yippie you win :)"
        putStrLn ""
    level %= (+1)
    cause "loadLevel" ()

parseLevel :: Int -> IO (Board, (Ix, Direction)) -- and restrictions
parseLevel i = do
    let fp = "data/automata/levels/level" ++ show (i + 1) ++ ".dat"
    withFile fp ReadMode $ \ fh -> do
        x <- hGetContents' fh

        return $ either (\ x -> error $ "error parsing level: " ++ fp ++ ",\n" ++ show x) id
               $ parse ((,) <$> boardP <*> ((,) <$> ixP <*> dirP)) fp x

ixP :: Parsec String u Ix
ixP = do
    x <- intP
    _ <- char ' '
    y <- intP
    _ <- char '\n'
    return (x, y)

dirP :: Parsec String u Direction
dirP = read . (:[]) <$> oneOf "NESW"

boardP :: Parsec String u Board
boardP = do
    xss <- flip sepBy (char '\n') $ many $ choice
        [ char ' ' >> pure Nothing
        , char '.' >> pure (Just (Nothing, Nothing))
        , char '*' >> pure (Just (Nothing, Just "*"))
        , fmap (\ c -> Just (Just [c], Nothing)) (oneOf "rb")
        ]

    return $ foldl' (\ m (i, xs) -> foldl' (\ m' (j, x) -> M.alter (const x) (j, i) m') m (zip [0..] xs)) mempty (zip [0..] xss)

loadLevel :: Rule AutomataLevels ()
loadLevel () = do
    x <- use level
    (bd, (pos, d)) <- effect (parseLevel x)
    aut .=  MkAutomata
        { _board = bd
        , _tapes = []
        , _iPtr = (0, 0)
        , _stack = []
        , _boardIx = pos
        , _dir = d
        , _gas = 1000
        }
    cause "askSubmit" ()

{-
runOutOfGas :: Lens' s Automata -> Rule s ()
runOutOfGas = _ 
-}