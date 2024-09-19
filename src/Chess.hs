{-# LANGUAGE TypeFamilies, DeriveGeneric, OverloadedStrings, LambdaCase, ScopedTypeVariables, TupleSections, ImplicitParams  #-}


module Chess ( chessServer ) where

import qualified Data.Map as M
import Text.Parsec
    ( char,
      choice,
      Parsec )
import Text.Parsec.Token (natural)

import Control.Lens ( at, view, (&), (?~), (%~), over )
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


import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Exception (catch, throwTo, handle, SomeException)
import Control.Monad.IO.Class (liftIO)
import Data.Either (fromRight)
import Data.Dynamic (toDyn)
import GHC.Stack (HasCallStack)


import Chess.Internal
import Chess.Structure
import Chess.Rules
import Control.Concurrent (myThreadId, ThreadId)


chess' :: HasCallStack => Game ChessState -> Game ChessState
chess' self = mempty
    & registerRule "Touch" playerTouch
        & registerRule "UpdateSelection" (markAvailableMoves self)
        & registerRule "UpdateSelection" clearAvailableMoves
        & registerRule "UpdateSelection" updateSelection
        & registerRule "SendSelect" sendSelect

        & registerRule "Touch1" touch1Turn
        & registerRule "Touch1Turn" touch1Piece
        & registerRule "Touch1Piece" touch1Colour

    & registerRule "Touch2" touch2
    & registerRule "UncheckedMove" uncheckedMovePiece
        & registerRule "UncheckedMovePiece" uncheckedMoveTurn
        & registerRule "UncheckedMoveTurn" uncheckedMoveSelf
        & registerRule "UncheckedMoveSelf" specializeMove
            & registerRule "UncheckedKMove" kingMove
            & registerRule "UncheckedKMove" castlingMove
            & registerRule "UncheckedQMove" queenMove
            & registerRule "UncheckedRMove" rookMove
            & registerRule "UncheckedBMove" bishopMove
            & registerRule "UncheckedNMove" knightMove
            & registerRule "UncheckedPMove" pawnMove
            & registerRule "UncheckedPMove" pawnDoubleMove
            & registerRule "UncheckedPMove" pawnCapture
            & registerRule "UncheckedPMove" pawnEP

        & registerRule "PromotionCheck" promotionCheck
        & registerRule "CheckedMovePiece" pawnZero

    & registerRule "CheckedMovePiece" moveMayCapture
        & registerRule "NonCapture" nonCapture
        & registerRule "Capture" captureZero
            & registerRule "Zeroing" zeroRule
        & registerRule "Capture" capture

        & registerRule "RawMove" rawMove
        & registerRule "Take" rawTake
            & registerRule "PutTile" putTile

    & registerRule "MoveEnd" (nMoveRule 50)
    & registerRule "MoveEnd" (nFoldRepetition 3)
    & registerRule "MoveEnd" winRule
        & registerRule "Win" winClose
    & registerRule "NextTurn" nextSubTurn
        & registerRule "NextSubTurn" promotionPrompt
            & registerRule "NextSubTurn" nextTurnSendStatus
            & registerRule "TryPromote" tryPromote

    & registerRule "PlayerConnected" connectSendBoard
    & registerRule "PlayerConnected" connectSendStatus
    & registerRule "PlayerDisconnected" disconnect
    & registerRule "Draw" sendDraw
    & registerRule "Win" sendWin

    & registerRule "SendBoardAll" serveBoardAll
    & registerRule "SendTileAll" serveDrawTileAll
    & registerRule "SendTile" sendTile
    & registerRule "SendDrawTile" serveDrawTile
    & registerRule "SendBoard" serveBoard
    & registerRule "SendTurn" serveTurn
    & registerRule "SendPromotionPrompt" servePromotionPrompt
    & registerRule "SendMarkAvailableMove" serveMarkAvailableMove
    & registerRule "SendClearAvailableMoves" serveClearAvailableMoves

    & registerRule "MoveEnd" movePrintBoard
    -- & registerRule "PrintBoard" printBoard


chess :: Game ChessState
chess = chess' chess

-- TODO overwriting
{-
chessRTS :: Game ChessState
chessRTS = chess
    & overwriteRule "touch1" touch1TurnRTS
-}
-- TODO splicing
{-
chessAtomic :: Game ChessState
chessAtomic = chess
    & spliceRule "Capture" "Capture2" captureExplode
-}

-- TODO registerRules
{-

    & registerRule "UncheckedMoveTurn" [noSelfCapture, somethingElse]
-}


secondUs :: Integer
secondUs = 1000 * 1000

data ChessMessage = TouchMsg Square | Register String String | PromoteMsg String deriving (Show, Eq, Generic)

instance ToJSON ChessMessage where
instance FromJSON ChessMessage where

interpretMessage :: Colour -> ChessMessage -> Maybe Action
interpretMessage p (TouchMsg a) = Just $ Event "Touch" $ toDyn (PlayerTouch p a)
interpretMessage p (PromoteMsg str) = Just $ Event "TryPromote" $ toDyn (p , str)
interpretMessage _ _ = Nothing


    -- & registerRule "SendRaw " (serverRule _)
chessApp :: TMVar (M.Map PlayerId Connection) -> TMVar (M.Map String (TMVar ChessState)) -> ThreadId -> ServerApp
chessApp connRef refGames serverThreadId pending = handle (throwTo serverThreadId :: SomeException -> IO ()) $ do 
    -- dirtiest of hacks but please give my callstacks back ^
    conn <- acceptRequest pending

    maybeMsg <- timeout (30 * secondUs) $ receiveDataMessage conn
    let maybeRoom = maybeMsg >>= decode . fromDataMessage

    whenJust maybeRoom $ \case
        Register room ident -> do
            games <- atomically $ readTMVar refGames

            refGame <- case view (at room) games of
                Just refGame -> return refGame
                Nothing -> do
                    refGame <- newTMVarIO chessInitial
                    atomically $ writeTMVar refGames (games & at room ?~ refGame)
                    return refGame

            game <- atomically $ readTMVar refGame
            let playerMap = view players game

            maybeColour <- case B.lookup ident playerMap of
                Just colour -> return $ Just colour
                Nothing     -> case B.lookupR White playerMap of
                    Just _  -> case B.lookupR Black playerMap of
                        Just _  -> return Nothing
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

        withTMVarIO st (`runner` [Event "PlayerConnected" $ toDyn colour])

        _ <- runMaybeT $ forever $ do
            isRunning <- view running <$> liftIO (atomically $ readTMVar st)
            unless isRunning $ hoistMaybe Nothing

            maybeMsg <- lift $ catch (timeout (10 * 60 * secondUs) $ receiveDataMessage conn) $
                \ (_ :: ConnectionException) -> return Nothing

            case maybeMsg of
                Nothing  -> do
                    lift $ withTMVarIO st (`runner` [Event "PlayerDisconnected" $ toDyn colour])
                    hoistMaybe Nothing
                Just msg -> do
                    let maybeEvent = decode (fromDataMessage msg) >>= interpretMessage colour

                    whenJust maybeEvent $ \ ev -> lift $ withTMVarIO st (`runner` [ev])

        withTMVarIO connRef (return . M.delete ident)
            where
            runner = runGame' $ chess
                & registerRule "CloseRoom" (roomRule refGames room)
                & registerRule "SendRaw" (serverRule connRef)

chessServer :: Int -> IO ()
chessServer port = do
    refGames <- newTMVarIO M.empty
    refConn  <- newTMVarIO M.empty

    serverThreadId <- myThreadId

    runServer "0.0.0.0" port (chessApp refConn refGames serverThreadId)

parseSquare :: Parsec String () Action
parseSquare = do
    c <- choice [char 'W' >> return White , char 'B' >> return Black]
    _ <- char ' '
    x <- fromInteger <$> natural haskell
    y <- fromInteger <$> natural haskell

    return $ Event "Touch" $ toDyn $ PlayerTouch c (x, y)

logEvent :: Show a => Rule ChessState a
logEvent e = effect $ print e

chessRunner :: Runner' ChessState
chessRunner = runGame' chess

testQueen :: IO ()
testQueen = do
    _ <- chessRunner chessInitial $ map (uncurry Event . ("Touch",) . toDyn)
        [ PlayerTouch White (4, 6)
        , PlayerTouch White (4, 4)
        , PlayerTouch Black (0, 1)
        , PlayerTouch Black (0, 2)
        , PlayerTouch White (3, 7)
        , PlayerTouch White (7, 3)]
    return ()

testPromotion :: IO ()
testPromotion = do
    let targetBoard = fromRight undefined $ parseFEN "2R5/3P4/8/8/8/4kp2/P7/5K2"
    newState <- chessRunner (chessInitial { _board = targetBoard }) $ map (uncurry Event)
        [ ("PrintBoard", toDyn ())
        , ("Touch", toDyn $ PlayerTouch White (3, 1))
        , ("Touch", toDyn $ PlayerTouch White (3, 0))
        , ("TryPromote", toDyn (White, "Q" :: String))
        , ("Touch", toDyn $ PlayerTouch Black (4, 5))
        , ("Touch", toDyn $ PlayerTouch Black (3, 5))
        , ("Touch", toDyn $ PlayerTouch White (3, 0))
        , ("Touch", toDyn $ PlayerTouch White (3, 5))]

    let targetState = chessInitial {
        _board = targetBoard,
        _turn  = Normal 9
    }

    if _board newState == _board targetState then
        putStrLn "All good!"
    else
        putStrLn ":("

    putStrLn $ ppBoard $ _board newState
    putStrLn ""
    putStrLn $ ppBoard $ _board targetState

{-
chessInput :: IO ChessEvent
chessInput = do
    str <- getLine

    case runParser parseSquare () "" str of
        Left _  -> chessInput
        Right y -> return y
-}
