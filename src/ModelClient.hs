module ModelClient ( client ) where

import Network.WebSockets (runClient, receiveDataMessage)
import Network.WebSockets.Connection
    ( sendBinaryData, Connection )
import Data.Aeson (encode)
import MenuServer
import ServerTypes
import Chess.CatanStruct

client :: IO ()
client = runClient "192.168.178.56" 58846 "" $ \ conn -> do
    sendBinaryData conn $ encode (Room "Room3", User "Jon")
    print =<< receiveDataMessage conn -- "mode?"
    sendBinaryData conn $ encode ModeCatan
    print =<< receiveDataMessage conn -- "Welcome Jon"
    sendBinaryData conn $ encode (BuildSettleMsg (VertIx (TileIx 0 0) False) BSettlement)
    sendBinaryData conn $ encode (BuildRoadMsg (LineIx (TileIx 0 0) Three1))
    print =<< receiveDataMessage conn -- ""
    print =<< receiveDataMessage conn -- ""
    print =<< receiveDataMessage conn -- ""

clientRe :: IO ()
clientRe = runClient "localhost" 58846 "" $ \ conn -> do
    sendBinaryData conn $ encode (Room "Room1", User "Jon")
    print =<< receiveDataMessage conn

mkConn :: IO Connection
mkConn = runClient "localhost" 58846 "" return