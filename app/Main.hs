module Main (main) where

import Lib (chessGame, chessServer)

main :: IO ()
main = do
    chessServer
