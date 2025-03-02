{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module ServerTypes ( module ServerTypes ) where


import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Data.ByteString.Lazy
import Chess.Game
import Network.WebSockets
import Control.Concurrent.STM (TMVar)
import qualified Data.Map as M


newtype Room = Room String deriving (Eq, Ord, Show, Generic)
newtype User = User { userName :: String } deriving (Eq, Ord, Show, Generic)

instance ToJSON Room where
instance FromJSON Room where

instance ToJSON User where
instance FromJSON User where

data GameServer = forall a. GameServer a (Game a) (ServerEvent -> Maybe Action)
data ServerEvent = ServerStarted (TMVar (M.Map User Connection)) | Connect User | Message User ByteString | Disconnect User