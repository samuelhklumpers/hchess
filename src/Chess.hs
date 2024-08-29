{-# LANGUAGE TypeFamilies, DeriveGeneric, OverloadedStrings, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- [x] properly close session on disconnect/win
-- [x] test if game proceeds properly
--   [x] promoting
--     [x] turns, await text input
 
-- [ ] checkers
--   [ ] simultation
--     [ ] speedup (datakinds + rule trie?)
--     [ ] split pure/IO output of rules (e.g. StateT s (Writer ([e], IO ()) ())
-- [ ] torus
-- [ ] line of sight
-- [ ] funky pieces

-- [ ] change event handling and everything to let rules decide when to cause something themselves (e.g. timer)


module Chess (chessRunner, chessGame, chessServer) where

import qualified Data.Map as M
import Text.Parsec
    ( char,
      choice,
      runParser,
      Parsec )
import Text.Parsec.Token (natural)

import Control.Lens ( at, view, (&), (?~), (%~), over, (.=) )
import Control.Monad (forever, unless)
import Text.Parsec.Language (haskell)
import Network.WebSockets
    ( acceptRequest,
      receiveDataMessage,
      runServer,
      Connection,
      ServerApp,
      WebSocketsData(fromDataMessage), ConnectionException )
import Control.Concurrent.Timeout ( timeout )
import Data.Aeson (decode, ToJSON, FromJSON)
import GHC.Generics (Generic)
import Control.Concurrent.STM
    ( atomically, newTMVarIO, readTMVar, writeTMVar, TMVar )
import qualified Data.Bimap as B


import Chess.Rules
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Exception (catch)
import Control.Monad.IO.Class (liftIO)
import Data.Either (fromRight)


chess :: Game ChessState ChessEvent
chess = compile [
        logEvent,
        printBoard, movePrintBoard,
        touch1, touch1TurnRTS, touch1Piece, touch1Colour, touch2Colour, uncheckedMovePiece, specializeMove,
        kingMove, generalizeMove, capture, nonCapture, moveMayCapture, pawnMove, moveEnd,
        pawnDoubleMove, pawnCapture, pawnEP, rawTake, rawMove, queenMove, rookMove, bishopMove, knightMove, castlingMove,
        putTile, winRule, sendWin, disconnect, winClose, sendSelection,
        promotionCheck, promotionPrompt, promote1, drawSelection, sendTile, connectSendStatus, nextTurnSendStatus
    ]

chessInput :: IO ChessEvent
chessInput = do
    str <- getLine

    case runParser parseSquare () "" str of
        Left _  -> chessInput
        Right y -> return y

secondUs :: Integer
secondUs = 1000 * 1000

data ChessMessage = TouchMsg Square | Register String String | PromoteMsg String deriving (Show, Eq, Generic)

instance ToJSON ChessMessage where
instance FromJSON ChessMessage where

interpretMessage :: Colour -> ChessMessage -> Maybe ChessEvent
interpretMessage p (TouchMsg a) = Just $ Touch p (screenTransform p a) -- TODO maybe screentransform should be clientside?
interpretMessage p (PromoteMsg str) = Just $ Promote p str
interpretMessage _ _ = Nothing

withTMVarIO :: TMVar a -> (a -> IO a) -> IO ()
withTMVarIO var f = do
    val  <- atomically $ readTMVar var
    val' <- f val
    atomically $ writeTMVar var val'


chessApp :: TMVar (M.Map PlayerId Connection) -> TMVar (M.Map String (TMVar ChessState)) -> ServerApp
chessApp connRef refGames pending = do
    conn <- acceptRequest pending

    maybeMsg <- timeout (30 * secondUs) $ receiveDataMessage conn
    let maybeRoom = maybeMsg >>= decode . fromDataMessage

    whenJust maybeRoom $ \case
        Register room ident -> do
            games <- atomically $ readTMVar refGames

            refGame <- case view (at room) games of
                Just refGame -> do
                    return refGame
                Nothing -> do
                    refGame <- newTMVarIO chessInitial
                    atomically $ writeTMVar refGames (games & at room ?~ refGame)
                    return refGame

            game <- atomically $ readTMVar refGame
            let playerMap = view players game

            maybeColour <- case B.lookup ident playerMap of
                Just colour -> do
                    return $ Just colour
                Nothing     -> case B.lookupR White playerMap of
                    Just _  -> case B.lookupR Black playerMap of
                        Just _  -> do
                            return Nothing
                        Nothing -> do
                            atomically $ writeTMVar refGame (game & players %~ B.insert ident Black)
                            return $ Just Black
                    Nothing -> do
                        atomically $ writeTMVar refGame (game & players %~ B.insert ident White)
                        return $ Just White

            whenJust maybeColour $ \ colour -> mainloop colour (room , ident) refGame conn
        _ -> return ()

    where
    mainloop :: Colour -> PlayerId -> TMVar ChessState -> Connection -> IO ()
    mainloop colour ident@(room , _) st conn = do
        withTMVarIO connRef (return . M.insert ident conn)
        withTMVarIO st (return . over connections (M.insert colour ident))

        withTMVarIO st (`runner` [PlayerConnected colour])

        _ <- runMaybeT $ forever $ do
            isRunning <- view running <$> liftIO (atomically $ readTMVar st)
            unless isRunning $ hoistMaybe Nothing

            maybeMsg <- lift $ do
                catch (timeout (10 * 60 * secondUs) $ receiveDataMessage conn) $
                    \ (_ :: ConnectionException) -> return Nothing

            case maybeMsg of
                Nothing  -> do
                    lift $ withTMVarIO st (`runner` [PlayerDisconnected colour])
                    hoistMaybe Nothing
                Just msg -> do
                    let maybeEvent = decode (fromDataMessage msg) >>= interpretMessage colour

                    whenJust maybeEvent $ \ ev -> do
                        lift $ withTMVarIO st (`runner` [ev])

        withTMVarIO connRef (return . M.delete ident)
            where
            runner = recGame $ combine chess (combine (roomRule refGames room) (serverRule connRef))

roomRule :: TMVar (M.Map String (TMVar ChessState)) -> String -> Chess
roomRule refGames room e = case e of
    CloseRoom -> do
        liftIO $ withTMVarIO refGames (return . M.delete room)
        running .= False
    _         -> return ()



chessServer :: IO ()
chessServer = do
    refGames <- newTMVarIO M.empty
    refConn  <- newTMVarIO M.empty

    runServer "0.0.0.0" 58846 (chessApp refConn refGames)

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
    newState <- chessRunner (chessInitial { _board = targetBoard })
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
    _ <- chessRunner chessInitial
        [ Touch White (4, 6), Touch White (4, 4)
        , Touch Black (0, 1), Touch Black (0, 2)
        , Touch White (3, 7), Touch White (7, 3)] 

    return ()