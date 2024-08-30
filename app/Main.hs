{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Chess (chessServer)

import Network.Wai.Handler.Warp as W
import Network.Wai.Application.Static
import Control.Concurrent

import GHC.Generics
import Data.Aeson
import System.FilePath
import System.Directory
import WaiAppStatic.Types (MaxAge(..))


cfgFileName :: FilePath
cfgFileName = "chess_server.json"

data Cfg = Cfg { http_port :: Int , game_port :: Int , data_dir :: String } deriving Generic

instance FromJSON Cfg where

findConfig :: FilePath -> IO (Maybe Cfg)
findConfig dir = do
    fileExists <- doesFileExist fp
    if fileExists then
        decodeFileStrict' fp
    else
        if isDrive dir then
            return Nothing
        else
            findConfig (takeDirectory dir)
    where
    fp = dir </> cfgFileName


main :: IO ()
main = do
    maybeCfg <- findConfig "."

    case maybeCfg of 
        Nothing  -> return ()
        Just cfg -> do
            let settings = defaultFileServerSettings (data_dir cfg)
            _ <- forkIO $ W.run (http_port cfg) (staticApp $ settings { ssMaxAge = NoCache })
            chessServer (game_port cfg)