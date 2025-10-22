{-# LANGUAGE RankNTypes #-}
module Automata.Rules where

import GHC.Stack (HasCallStack)
import Control.Lens.Combinators (use, At (..), Ixed (..), pre, to)
import Control.Monad (when)

import Game (Rule, cause, effect)
import Automata.Structure
import Control.Lens.Operators ((%=), (.=))
import Data.Maybe (isJust)
import Internal (cycleNext, cyclePrev)


step :: HasCallStack => Rule Automata ()
step () = do
    pos <- use boardIx
    mt <- use $ board . at pos

    ptr <- use tapeIx
    mp <- use $ tape . pre (ix ptr)

    case mp of
        Nothing -> error $ "instruction out of bounds: " ++ show ptr
        Just instr -> case mt of
            Nothing -> error $ "tile out of bounds: " ++ show pos
            Just tile -> do
                    cause "runOp" (instr, tile, pos, ptr)

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

runOp :: HasCallStack => Rule Automata (Instr, Tile, Ix, Int)
runOp ((op, ocol), (tcol, _), pos, ptr) = do
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
            Reset -> do
                tapeIx .= 0
                --cause "step" ()
    else
        cause "next" ptr

die :: HasCallStack => Rule Automata ()
die () = effect (putStrLn "You died :(")

next :: HasCallStack => Rule Automata Int
next ptr = do
    tapeLen <- use (tape . to length)
    tapeIx .= (ptr + 1) `mod` tapeLen
    --cause "step" ()
