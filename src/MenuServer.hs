{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}


module MenuServer ( module MenuServer ) where

import qualified Data.Map as M

import Control.Lens ( at, (&), (?~)  )
import Control.Monad (forever)
import Network.WebSockets
    ( acceptRequest,
      receiveDataMessage,
      runServer,
      ServerApp,
      WebSocketsData(fromDataMessage), ConnectionException, Connection, sendBinaryData )
import Control.Concurrent.Timeout ( timeout )
import Data.Aeson (decode, ToJSON, FromJSON, encode)
import GHC.Generics (Generic)
import Control.Concurrent.STM
    ( newTMVarIO, TMVar )


import Control.Monad.Trans.Maybe
import Control.Exception (catch, throwTo, handle, SomeException)
import Control.Monad.IO.Class (liftIO)

import Control.Concurrent (myThreadId, ThreadId)
import Data.Functor (void)
import Data.ByteString.Lazy
import Data.Maybe (maybeToList)

import Chess.Game
import Chess.Internal
import Chess.CatanStruct
import ServerTypes
import Control.Exception.Base (IOException)

menuServer :: Int -> IO ()
menuServer port = do
    tConns  <- newTMVarIO M.empty
    tGames <- newTMVarIO M.empty
    thread <- myThreadId

    runServer "0.0.0.0" port (menuApp tConns tGames thread)

data Modes = ModeChess | ModeCatan deriving (Eq, Ord, Show, Generic)

instance ToJSON Modes where
instance FromJSON Modes where

menuApp :: TMVar (M.Map Room (TMVar (M.Map User Connection))) -> TMVar (M.Map Room (TMVar GameServer)) -> ThreadId -> ServerApp
menuApp tConns tGames thread req = handle (throwTo thread :: SomeException -> IO ()) $ void (runMaybeT login)
    where
    login :: MaybeT IO ()
    login = do
        conn <- liftIO $ acceptRequest req
        loginMsg  <- MaybeT $ catch (timeout (30 * secondUs) $ receiveDataMessage conn) $ \ (_ :: ConnectionException) -> return Nothing

        (room :: Room, user :: User) <- hoistMaybe $ decode (fromDataMessage loginMsg)
        liftIO $ print (room, user)

        -- TODO inlining this might just be more readable
        tRoom <- liftIO $ withTMVarIO tConns $ \ roomTConns -> do
            case M.lookup room roomTConns of
                Nothing     -> do
                    refConn <- newTMVarIO $ M.singleton user conn
                    return (roomTConns & at room ?~ refConn , refConn)
                Just refConn -> do
                    withTMVarIO_ refConn $ \ roomConns -> do
                        return $ M.insert user conn roomConns
                    return (roomTConns, refConn)

        tServer <- liftIO $ withTMVarIO tGames $ \ roomTGames ->
            case M.lookup room roomTGames of
                Nothing -> do
                    sendBinaryData conn $ encode ("mode?" :: String)
                    modeMsg <- catch (timeout (30 * secondUs) $ receiveDataMessage conn) $ \ (_ :: ConnectionException) -> return Nothing

                    case decode . fromDataMessage =<< modeMsg :: Maybe Modes of
                        Nothing -> return (roomTGames, Nothing)
                        Just mode -> case mode of
                            ModeChess -> error "a"
                            ModeCatan -> do
                                tServer <- newTMVarIO $ GameServer catan0 catanGame catanDecode
                                withTMVarIO_ tServer $ pushEvent $ ServerStarted tRoom
                                return (M.insert room tServer roomTGames, Just tServer)
                Just tGame -> return (roomTGames, Just tGame)

        case tServer of
            Nothing -> return ()
            Just server -> liftIO $ do
                mainloop user server conn
                withTMVarIO_ server $ pushEvent $ Disconnect user

        liftIO $ withTMVarIO_ tRoom (return . M.delete user)
        where
        mainloop :: User -> TMVar GameServer -> Connection -> IO ()
        mainloop me tGame myConn = do
            withTMVarIO_ tGame $ pushEvent $ Connect me

            void $ runMaybeT $ forever $ do
                msg :: ByteString <- MaybeT $ do
                    let rcv = timeout (10 * 60 * secondUs) $ receiveDataMessage myConn
                    fmap (fmap fromDataMessage) $ catch rcv $ \ (_ :: SomeException) -> return Nothing

                liftIO $ print msg

                MaybeT $ catch  (do
                        withTMVarIO_ tGame $ pushEvent $ Message me msg
                        return $ Just ()) $
                    \ (_ :: SomeException) -> return Nothing -- sunglasses

catanDecode :: ServerEvent -> Maybe Action
catanDecode (ServerStarted r) = Just $ mkEvent "ServerStarted" r
catanDecode (Connect u) = Just $ mkEvent "UserConnect" u
catanDecode (Message u m) = decode m >>= \case
    BuildSettleMsg ix b -> Just $ mkEvent "UserBuildSettlement" (u, ix, b)
    BuildRoadMsg ix -> Just $ mkEvent "UserBuildRoad" (u, ix)
catanDecode (Disconnect u) = Just $ mkEvent "UserDisconnect" u

pushEvent :: ServerEvent -> GameServer -> IO GameServer
pushEvent e (GameServer gameState game gameDecode) = do
    (_, gameState') <- logGame' game gameState (maybeToList $ gameDecode e)
    return $ GameServer gameState' game gameDecode

secondUs :: Integer
secondUs = 1000 * 1000