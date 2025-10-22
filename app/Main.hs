module Main (main) where

import Automata
import Game (runGame', mkEvent)

main :: IO ()
main = do
    g <- parseAutomata
    go g (runGame' automata)
    where
    go g r = do
        putStrLn $ pprintAutomata g
        _ <- getLine
        g' <- r g [mkEvent "step" ()]
        go g' r