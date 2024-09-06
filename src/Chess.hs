{-# LANGUAGE TypeFamilies, DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}


module Chess (chessRunner, chessGame, chessServer) where

import qualified Data.Bimap as B
import qualified Data.Map as M
import qualified Data.List as L
import Text.Parsec
    ( char,
      choice,
      runParser,
      Parsec )
import Text.Parsec.Token (natural)

import Control.Lens ( at, view, (&), (?~), (%~), (.=), (^.), use, (.~) )
import Control.Monad (forever, unless, void, when)
import Text.Parsec.Language (haskell)
import Network.WebSockets
    ( acceptRequest,
      receiveDataMessage,
      runServer,
      ServerApp,
      WebSocketsData(fromDataMessage), ConnectionException, Connection )
import Control.Concurrent.Timeout ( timeout )
import Data.Aeson (decode, ToJSON, FromJSON)
import GHC.Generics (Generic)
import Control.Concurrent.STM
    ( atomically, newTMVarIO, readTMVar, writeTMVar, TMVar, takeTMVar )


import Chess.Rules
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Exception (catch)
import Control.Monad.IO.Class (liftIO)
import Data.Either (fromRight)
import Network.WebSockets.Connection (Connection(..))


chess :: Game ChessState ChessEvent
chess = compile [
        logEvent,
        --printBoard, movePrintBoard,
        touch1, touch1Turn, touch1Piece, touch1Colour, touch2Unchecked, moveTurn, uncheckedMovePiece, specializeMove,
        kingMove, generalizeMove, capture, nonCapture, moveMayCapture, pawnMove, moveEnd,
        pawnDoubleMove, pawnCapture, pawnEP, rawTake, rawMove, queenMove, rookMove, bishopMove, knightMove, castlingMove,
        putTile, winRule, sendWin, testCloseRoom, winClose, sendSelection,
        promotionCheck, promotionPrompt, promote1, drawSelection, sendTile, connectSendStatus, nextTurnSendStatus,
        sendAvailableMoves chess, clearAvailableMoves, noSelfCapture,
        pawnZero, captureZero, zeroRule, nMoveRule 50, nFoldRepetition 3
    ]

chessInput :: IO ChessEvent
chessInput = do
    str <- getLine

    case runParser parseSquare () "" str of
        Left _  -> chessInput
        Right y -> return y

secondUs :: Integer
secondUs = 1000 * 1000

data ChessMessage = TouchMsg Square | Register String String Colour | PromoteMsg String deriving (Show, Eq, Generic)

instance ToJSON ChessMessage where
instance FromJSON ChessMessage where

interpretMessage :: Colour -> ChessMessage -> Maybe ChessEvent
interpretMessage p (TouchMsg a) = Just $ Touch p (screenTransform p a)
interpretMessage p (PromoteMsg str) = Just $ Promote p str
interpretMessage _ _ = Nothing

withTMVarIO_ :: TMVar a -> (a -> IO a) -> IO ()
withTMVarIO_ var f = do
    val  <- atomically $ takeTMVar var
    val' <- f val
    atomically $ writeTMVar var val'

withTMVarIO :: TMVar a -> (a -> IO (a, b)) -> IO b
withTMVarIO var f = do
    val  <- atomically $ takeTMVar var
    (val', ret) <- f val
    atomically $ writeTMVar var val'
    return ret

type Registration = (String, String, Colour)

fromRegister :: ChessMessage -> Maybe Registration
fromRegister (Register a b c) = Just (a , b , c)
fromRegister _ = Nothing

{-
setdefault :: Ord k => k -> a -> M.Map k a -> (M.Map k a , a)
setdefault k a m = case m ^. at k of
    Nothing -> (m & at k ?~ a, a)
    Just a' -> (m, a')

findInsertM :: (Ord k, Monad m) => (a -> a -> a) -> k -> m a -> M.Map k a -> m (M.Map k a , a)
findInsertM f k ma m = case m ^. at k of
    Nothing -> ma >>= \a -> return (m & at k ?~ a, a)
    Just a' -> return (m, a')
-}

-- almost insertLookupWithKey except for not doing the effect if the key exists
setdefaultM :: (Ord k, Monad m) => k -> m a -> M.Map k a -> m (M.Map k a , a)
setdefaultM k ma m = case m ^. at k of
    Nothing -> ma >>= \a -> return (m & at k ?~ a, a)
    Just a' -> return (m, a')

instance Eq Connection where
    (==) :: Connection -> Connection -> Bool
    c == c' = connectionSentClose c == connectionSentClose c'

chessApp :: TMVar (M.Map String (TMVar (M.Map Colour [Connection]))) -> TMVar (M.Map String (TMVar ChessState)) -> ServerApp
chessApp refRefConn refRefGame pending = void (runMaybeT acceptor)
    where
    acceptor :: MaybeT IO ()
    acceptor = do
        conn <- liftIO $ acceptRequest pending
        msg  <- MaybeT $ timeout (30 * secondUs) $ receiveDataMessage conn
        (room, ident, colour) <- hoistMaybe $ decode (fromDataMessage msg) >>= fromRegister

        refGame <- liftIO $ withTMVarIO refRefGame $ setdefaultM room (newTMVarIO chessInitial)
        refConn <- liftIO $ withTMVarIO refRefConn $ \ connRefM -> do
            case M.lookup room connRefM of
                Nothing     -> do
                    refConn <- newTMVarIO $ M.singleton colour [conn]
                    return (M.insert room refConn connRefM, refConn)
                Just refConn -> do
                    withTMVarIO_ refConn $ \ connM -> do
                        return $ M.insertWith (++) colour [conn] connM
                    return (connRefM, refConn)

        spectator <- liftIO $ withTMVarIO refGame $ \ game -> do
            let spectator = maybe False (ident /=) (B.lookupR colour $ game ^. players)

            if spectator then
                return (game , spectator)
            else
                return (game & players %~ B.insert ident colour , spectator)


        let baseRules = combine chess (serverRule refConn)
        let rules = if spectator then
                        baseRules
                    else
                        combine baseRules (
                        combine (disconnectClose refConn refGame) (
                                roomRule refRefGame refRefConn room))

        let runner = recGame rules
        liftIO $ withTMVarIO_ refGame (`runner` [Event $ PlayerConnected colour])
        liftIO $ mainloop runner colour spectator refGame conn

        liftIO $ withTMVarIO_ refConn (return . M.adjust (L.delete conn) colour)
        where
        mainloop :: Runner ChessState ChessEvent -> Colour -> Bool -> TMVar ChessState -> Connection -> IO ()
        mainloop runner colour spectator st conn = do
            _ <- runMaybeT $ forever $ do
                isRunning <- view running <$> liftIO (atomically $ readTMVar st)
                unless isRunning $ hoistMaybe Nothing

                maybeMsg <- lift $ do
                    catch (timeout (10 * 60 * secondUs) $ receiveDataMessage conn) $
                        \ (_ :: ConnectionException) -> return Nothing

                case maybeMsg of
                    Nothing  -> do
                        unless spectator $ do
                            lift $ withTMVarIO_ st (`runner` [Event $ PlayerDisconnected colour])
                        hoistMaybe Nothing
                    Just msg -> do
                        unless spectator $ do
                            let maybeEvent = decode (fromDataMessage msg) >>= interpretMessage colour

                            whenJust maybeEvent $ \ ev -> do
                                lift $ withTMVarIO_ st (`runner` [Event ev])
            return ()

roomRule :: TMVar (M.Map String (TMVar ChessState)) -> TMVar (M.Map String (TMVar (M.Map Colour [Connection]))) -> String -> Chess
roomRule refRefGame refConn room e = case e of
    CloseRoom -> do
        effect $ withTMVarIO_ refRefGame (return . M.delete room)
        effect $ withTMVarIO_ refConn (return . M.delete room)
        running .= False
    _         -> return ()

disconnectClose :: TMVar (M.Map Colour [Connection]) -> TMVar ChessState -> Chess
disconnectClose refConn refGame (PlayerDisconnected _) = do
    effect $ do
        connM <- atomically $ readTMVar refConn
        when (M.null connM) $ do
            withTMVarIO_ refGame (return . (closing .~ True))
    cause TestCloseRoom
disconnectClose _ _ _ = return ()

testCloseRoom :: Chess
testCloseRoom TestCloseRoom = do
    isClosing <- use closing
    when isClosing $ cause CloseRoom
testCloseRoom _ = return ()


{-
disconnect :: Chess
disconnect (PlayerDisconnected c) = do
    connections %= M.delete c
    currentConnections <- use connections

    when (M.null currentConnections) $ cause CloseRoom
disconnect _ = return ()
-}

chessServer :: Int -> IO ()
chessServer port = do
    refRefGame <- newTMVarIO M.empty
    refConn  <- newTMVarIO M.empty

    let app = chessApp refConn refRefGame
    runServer "0.0.0.0" port app

parseSquare :: Parsec String () ChessEvent
parseSquare = do
    c <- choice [char 'W' >> return White , char 'B' >> return Black]
    _ <- char ' '
    x <- fromInteger <$> natural haskell
    y <- fromInteger <$> natural haskell

    return $ Touch c (x, y)

chessRunner :: Runner ChessState ChessEvent
chessRunner = recGame chess

chessGame :: IO ChessState
chessGame = runGame chessRunner chessInput chessInitial


testPromotion :: IO ()
testPromotion = do
    let targetBoard = fromRight undefined $ parseFEN "2R5/3P4/8/8/8/4kp2/P7/5K2"
    newState <- chessRunner (chessInitial { _board = targetBoard }) $ map Event
        [ PrintBoard
        , Touch White (3, 1), Touch White (3, 0), Promote White "Q"
        , Touch Black (4, 5), Touch Black (3, 5)
        , Touch White (3, 0), Touch White (3, 5) ]

    let targetState = chessInitial {
        _board = targetBoard,
        _turn  = Normal 9
    }

    if newState == targetState then
        putStrLn "All good!"
    else
        putStrLn ":("

testQueen :: IO ()
testQueen = do
    _ <- chessRunner chessInitial $ map Event
        [ Touch White (4, 6), Touch White (4, 4)
        , Touch Black (0, 1), Touch Black (0, 2)
        , Touch White (3, 7), Touch White (7, 3)]
    return ()
