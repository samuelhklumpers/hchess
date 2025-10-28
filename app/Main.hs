module Main (main) where

import Automata
import Game (runGame', mkEvent)
import System.Directory

main :: IO ()
main = do
    -- putStrLn =<< getCurrentDirectory
    automataMain