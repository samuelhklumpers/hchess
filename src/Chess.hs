{-# LANGUAGE TypeFamilies, DeriveGeneric, OverloadedStrings, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}


module Chess ( chessServer ) where

import qualified Data.Bimap as B
import qualified Data.Map as M
import qualified Data.List as L
import Text.Parsec
    ( char,
      choice,
      Parsec )
import Text.Parsec.Token (natural)

import Control.Lens ( at, view, (&), (?~), (%~) , _2, _1, (^.) )
import Control.Monad (forever, unless)
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
    ( atomically, newTMVarIO, readTMVar, TMVar )


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
import qualified Data.Set as S
import Data.Function (fix)
import Debug.Trace (trace)
import Network.WebSockets.Connection (connectionSentClose)
import Data.Functor (void)


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
    & registerRule "testCloseRoom" testCloseRoom
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
    & registerRule "PrintBoard" printBoard


--data ChessOpts = Atomic -- | Checkers | SelfCapture
--    deriving (Show, Ord, Eq, Generic)

type ChessOpts = String

atomicDefaults :: [(Int, Int)]
atomicDefaults =
    [ (-1 , -1), ( 0,  -1), ( 1 , -1)
    , (-1 ,  0), ( 0 ,  0), ( 1 ,  0)
    , (-1 ,  1), ( 0 ,  1), ( 1 ,  1)]

applyChessOpt :: ChessOpts -> Game ChessState -> Game ChessState
applyChessOpt "Atomic" = registerRule "Capture" (atomicExplosion atomicDefaults)
applyChessOpt opt = trace ("Unrecognized option: " ++ opt)
--applyChessOpt Checkers = spliceRule "UncheckedMoveSelf" "UncheckedMoveCheckers" checkers
--applyChessOpt SelfCapture = overwriteRule "UncheckedMoveTurn" (idRule "UncheckedMoveSelf")

chessWithOpts :: S.Set ChessOpts -> Game ChessState
chessWithOpts opts = fix $ S.foldl' (\ f -> (f .) . applyChessOpt) chess' opts

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

data ChessMessage = TouchMsg Square | Register String String Colour [ChessOpts] | PromoteMsg String deriving (Show, Eq, Generic)

--instance ToJSON ChessOpts
--instance FromJSON ChessOpts

instance ToJSON ChessMessage where
instance FromJSON ChessMessage where

interpretMessage :: Colour -> ChessMessage -> Maybe Action
interpretMessage p (TouchMsg a) = Just $ Event "Touch" $ toDyn (PlayerTouch p a)
interpretMessage p (PromoteMsg str) = Just $ Event "TryPromote" $ toDyn (p , str)
interpretMessage _ _ = Nothing

type Registration = (String, String, Colour, [ChessOpts])

fromRegister :: ChessMessage -> Maybe Registration
fromRegister (Register a b c d) = Just (a , b , c , d)
fromRegister _ = Nothing

instance Eq Connection where
    c == c' = connectionSentClose c == connectionSentClose c'


chessApp :: TMVar (M.Map String (TMVar (M.Map Colour [Connection]))) -> TMVar (M.Map String (TMVar (ChessState, Game ChessState))) -> ThreadId -> ServerApp
chessApp refRefConn refRefGame serverThreadId pending = handle (throwTo serverThreadId :: SomeException -> IO ()) $ void (runMaybeT acceptor)
    -- dirtiest of hacks but please give my callstacks back ^
    where
    acceptor :: MaybeT IO ()
    acceptor = do
        conn <- liftIO $ acceptRequest pending
        msg  <- MaybeT $ timeout (30 * secondUs) $ receiveDataMessage conn

        (room, ident, colour, opts) <- hoistMaybe $ decode (fromDataMessage msg) >>= fromRegister

        refGame <- liftIO $ withTMVarIO refRefGame $ setdefaultM room $ newTMVarIO (chessInitial, chessWithOpts (S.fromList opts))
        refConn <- liftIO $ withTMVarIO refRefConn $ \ connRefM -> do
            case M.lookup room connRefM of
                Nothing     -> do
                    refConn <- newTMVarIO $ M.singleton colour [conn]
                    return (connRefM & at room ?~ refConn , refConn)
                Just refConn -> do
                    withTMVarIO_ refConn $ \ connM -> do
                        return $ M.insertWith (++) colour [conn] connM
                    return (connRefM, refConn)

        spectator <- liftIO $ withTMVarIO refGame $ \ game -> do
            let spectator = maybe False (ident /=) (B.lookupR colour $ game ^. _1 . players)

            if spectator then
                return (game , spectator)
            else
                return (game & _1 . players %~ B.insert ident colour , spectator)

        game <- liftIO $ atomically $ readTMVar refGame

        let baseRules = view _2 game
                & registerRule "SendRaw" (serverRule refConn)

        let runner = runGame' $ if spectator then baseRules else baseRules
                & registerRule "PlayerDisconnected" (disconnect refConn refGame)
                & registerRule "CloseRoom" (roomRule refRefGame refRefConn room)

        liftIO $ withTMVarIO_ refGame $ _1 (`runner` [Event "PlayerConnected" $ toDyn colour])
        liftIO $ mainloop colour spectator refGame conn runner
        liftIO $ withTMVarIO_ refConn (return . M.adjust (L.delete conn) colour)
        where
        mainloop :: Colour -> Bool -> TMVar (ChessState , Game ChessState) -> Connection -> Runner' ChessState -> IO ()
        mainloop colour spectator st conn runner = void $ runMaybeT $ forever $ do
                isRunning <- view (_1 . running) <$> liftIO (atomically $ readTMVar st)
                unless isRunning $ hoistMaybe Nothing

                maybeMsg <- lift $ catch (timeout (10 * 60 * secondUs) $ receiveDataMessage conn) $
                    \ (_ :: ConnectionException) -> return Nothing

                case maybeMsg of
                    Nothing  -> do
                        unless spectator $ do
                            lift $ withTMVarIO_ st $ _1 (`runner` [Event "PlayerDisconnected" $ toDyn colour])
                        hoistMaybe Nothing
                    Just msg -> do
                        unless spectator $ do
                            let maybeEvent = decode (fromDataMessage msg) >>= interpretMessage colour

                            whenJust maybeEvent $ \ ev -> do
                                lift $ withTMVarIO_ st $ _1 (`runner` [ev])

chessServer :: Int -> IO ()
chessServer port = do
    refRefGame <- newTMVarIO M.empty
    refConn  <- newTMVarIO M.empty

    serverThreadId <- myThreadId

    runServer "0.0.0.0" port (chessApp refConn refRefGame serverThreadId)

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
