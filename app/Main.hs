module Main (main) where

import Lib (chessGame)

main :: IO ()
main = chessGame >> return ()
