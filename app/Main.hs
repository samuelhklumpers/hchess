module Main (main) where

import Chess (chessServer)

--import Network.Wai.Handler.Warp as W
--import Network.Wai.Application.Static
import Control.Concurrent

main :: IO ()
main = do
    --_ <- forkIO $ W.run 58845 (staticApp (defaultFileServerSettings "/mnt/c/Users/samue/OneDrive/Desktop/programming/games/hchess/web"))
    chessServer
