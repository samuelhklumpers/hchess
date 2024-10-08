{-# LANGUAGE TypeFamilies, DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeApplications #-}


module Chess ( chessServer , chessRunner ) where

import qualified Data.Bimap as B
import qualified Data.Map as M
import qualified Data.List as L

import Control.Lens ( at, view, (&), (?~), (%~) , _2, _1, (^.) )
import Control.Monad (forever, unless)
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
import Data.Dynamic (toDyn)
import GHC.Stack (HasCallStack)


import Chess.Internal
import Chess.Structure
import Chess.Rules
import Control.Concurrent (myThreadId, ThreadId)
import Data.Function (fix, on)
import Debug.Trace (trace)
import Network.WebSockets.Connection (connectionSentClose)
import Data.Functor (void)
import Data.Proxy (Proxy(..))


chess' :: HasCallStack => Game ChessState -> Game ChessState
chess' self = mempty
    & registerRule "Touch" (playerTouch touch)
        & registerRule "UpdateSelection" (markAvailableMoves self id)
        & registerRule "UpdateSelection" (clearAvailableMoves touch)
        & registerRule "UpdateSelection" (updateSelection touch)
        & registerRule "SendSelect" (sendSelect board)

        & registerRule "Touch1" (touch1Turn turn)
        & registerRule "Touch1Turn" (touch1Piece board)
        & registerRule "Touch1Piece" (touch1Colour touch)

    & registerRule "Touch2" touch2
    & registerRule "UncheckedMove" (uncheckedMovePiece board) -- TODO how does zooming interact with compiling?
        & registerRule "UncheckedMovePiece" (uncheckedMoveTurn turn)
        & registerRule "UncheckedMoveTurn" (uncheckedMoveSelf board)
        & registerRule "UncheckedMoveSelf" specializeMove
            & registerRule "UncheckedKMove" (kingMove castling)
            & registerRule "UncheckedKMove" (castlingMove board castling)
            & registerRule "UncheckedQMove" (queenMove board)
            & registerRule "UncheckedRMove" (rookMove board castling)
            & registerRule "UncheckedBMove" (bishopMove board)
            & registerRule "UncheckedNMove" knightMove
            & registerRule "UncheckedPMove" (pawnMove board)
            & registerRule "UncheckedPMove" (pawnDoubleMove board turn enpassant)
            & registerRule "UncheckedPMove" (pawnCapture board)
            & registerRule "UncheckedPMove" (pawnEP turn enpassant)
        & spliceRule "UncheckedMoveSelf" "UncheckedMoveNull" (nullMove "UncheckedMoveNull")

        & registerRule "PromotionCheck" (promotionCheck promoting)
        & registerRule "CheckedMovePiece" pawnZero

    & registerRule "CheckedMovePiece" (moveMayCapture board)
        & registerRule "NonCapture" nonCapture
        & registerRule "Capture" captureZero
            & registerRule "Zeroing" (zeroRule turn zeroing)
        & registerRule "Capture" capture

        & registerRule "RawMove" rawMove
        & registerRule "Take" rawTake
            & registerRule "PutTile" (putTile board)

    & registerRule "MoveEnd" (nMoveRule 50 turn zeroing)
    & registerRule "MoveEnd" (nFoldRepetition 3 board history)
    & registerRule "MoveEnd" (winRule board)
    & registerRule "NextTurn" (nextSubTurn turn promoting)
        & registerRule "NextSubTurn" (promotionPrompt turn)
            & registerRule "NextSubTurn" (nextTurnSendStatus turn)
            & registerRule "NextSubTurn" (turnStart :: Chess ()) -- TODO make this more accurate
            & registerRule "TryPromote" (tryPromote turn promoting)

    & registerRule "PlayerConnected" connectSendBoard
    & registerRule "PlayerConnected" (turnStart :: Chess Colour) -- TODO don't do this instead do RoomCreated or so

    & registerRule "PlayerConnected" (connectSendStatus turn)
    & registerRule "TestCloseRoom" (testCloseRoom closing)
    & registerRule "Draw" sendDraw
    & registerRule "Win" sendWin
    & registerRule "Win" (winClose :: Chess Colour)
    & registerRule "Draw" (winClose :: Chess ()) 

    & registerRule "SendBoardAll" serveBoardAll
    & registerRule "SendTileAll" serveDrawTileAll
    & registerRule "SendTile" (sendTile board)
    & registerRule "SendDrawTile" sendTileImage
    & registerRule "SendDrawTileImage" serveDrawTile
    & registerRule "SendBoard" (serveBoard board)
    & registerRule "SendTurn" serveTurn
    & registerRule "SendPromotionPrompt" servePromotionPrompt
    & registerRule "SendMarkAvailableMove" serveMarkAvailableMove
    & registerRule "SendClearAvailableMoves" serveClearAvailableMoves

    & registerRule "MoveEnd" movePrintBoard

    & registerRule "TurnStart" (idRule "MarkMoves" :: Chess ()) -- TODO bypassing should probably not be like this
    & registerRule "MarkMoves" (markMoves self "UncheckedMoveSelf" "UncheckedMoveSelf" (Proxy @PieceMove) id)
    & registerRule "StaleCheck" (staleMate moveCache turn :: Rule ChessState ())


    -- hush
    & registerRule "PrintBoard" (returnRule :: Rule ChessState ())
    & registerRule "SendRaw" (returnRule :: Rule ChessState SendRaw)
    -- & registerRule "TurnStart" (returnRule :: Rule ChessState ())


--data ChessOpts = Atomic -- | Checkers | SelfCapture
--    deriving (Show, Ord, Eq, Generic)

type ChessOpts = String

atomicDefaults :: [(Int, Int)]
atomicDefaults =
    [ (-1 , -1), ( 0,  -1), ( 1 , -1)
    , (-1 ,  0), ( 0 ,  0), ( 1 ,  0)
    , (-1 ,  1), ( 0 ,  1), ( 1 ,  1)]

applyChessOpt :: ChessOpts -> ChessBuilder -> ChessBuilder
applyChessOpt "Atomic" _ = registerRule "Capture" (atomicExplosion atomicDefaults board)
applyChessOpt "RTS" _ = overwriteRule "Touch1" (idRule "Touch1Turn" :: Chess PlayerTouch)
                      . overwriteRule "UncheckedMovePiece" (idRule "UncheckedMoveTurn" :: Chess PieceMove)
applyChessOpt "SelfCapture" _ = overwriteRule "UncheckedMoveTurn" (idRule "UncheckedMoveSelf" :: Chess PieceMove)
applyChessOpt "Anti" _ = spliceRule "Win" "AntiWin" antiChess
applyChessOpt "Checkers" self = spliceRule "UncheckedMoveSelf" "UncheckedMoveCheckers" (checkers turn captureCache)
                              . overwriteRule "MarkMoves" (markMoves (fix self) "UncheckedMoveSelf" "UncheckedMoveCheckers" (Proxy @PieceMove) id)
applyChessOpt "StrategoV" _ = spliceRule "SendDrawTileImage" "SendDrawTileImageStratego" stratego
                            . overwriteRule "SendBoard" serveBoardTiles
applyChessOpt "StrategoC" _ = spliceRule "Capture" "StrategoCapture" (strategoCapturesAA)
applyChessOpt opt _ = trace ("Unrecognized option: " ++ opt)

chessWithOpts :: [ChessOpts] -> ChessBuilder -> ChessBuilder
chessWithOpts []           _ = id
chessWithOpts (opt : opts) self = chessWithOpts opts self . applyChessOpt opt self

type ChessBuilder = Game ChessState -> Game ChessState


chess :: Game ChessState
chess = fix chess'


secondUs :: Integer
secondUs = 1000 * 1000

type FEN = String
data ChessMessage = TouchMsg Square | Register String String Colour [ChessOpts] (Maybe FEN) | PromoteMsg String | Ping deriving (Show, Eq, Generic)

--instance ToJSON ChessOpts
--instance FromJSON ChessOpts

instance ToJSON ChessMessage where
instance FromJSON ChessMessage where

interpretMessage :: Colour -> ChessMessage -> Maybe Action
interpretMessage p (TouchMsg a) = Just $ Event "Touch" $ toDyn (PlayerTouch p a)
interpretMessage p (PromoteMsg str) = Just $ Event "TryPromote" $ toDyn (p , str)
interpretMessage _ _ = Nothing

type Registration = (String, String, Colour, [ChessOpts], Maybe FEN)

fromRegister :: ChessMessage -> Maybe Registration
fromRegister (Register a b c d e) = Just (a , b , c , d, e)
fromRegister _ = Nothing

instance Eq Connection where
    c == c' = connectionSentClose c == connectionSentClose c'

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

chessApp :: TMVar (M.Map String (TMVar (M.Map Colour [Connection]))) -> TMVar (M.Map String (TMVar (ChessState, Game ChessState))) -> ThreadId -> ServerApp
chessApp refRefConn refRefGame serverThreadId pending = handle (throwTo serverThreadId :: SomeException -> IO ()) $ void (runMaybeT acceptor)
    -- dirtiest of hacks but please give me my callstacks back ^
    where
    acceptor :: MaybeT IO ()
    acceptor = do
        conn <- liftIO $ acceptRequest pending
        msg  <- MaybeT $ catch (timeout (30 * secondUs) $ receiveDataMessage conn) $ \ (_ :: ConnectionException) -> return Nothing
        (room, ident, colour, opts, maybeFEN) <- hoistMaybe $ decode (fromDataMessage msg) >>= fromRegister

        --liftIO $ print opts
        let ruleset = chessWithOpts opts ruleset . chess'
        --liftIO $ print $ fst $ fix $ ruleset

        let initialState = case maybeFEN >>= rightToMaybe . parseFEN of
                Nothing -> chessInitial 
                Just x  -> chessInitial { _board = x }

        refGame <- liftIO $ withTMVarIO refRefGame $ setdefaultM room $ newTMVarIO (initialState, fix ruleset)
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
                & overwriteRule "SendRaw" (serverRule refConn)

        let runner = runGame' $ if spectator then baseRules else baseRules
                & registerRule "PlayerDisconnected" (disconnect refConn refGame)
                & registerRule "CloseRoom" (roomRule refRefGame refRefConn room running)
                & overwriteRule "PrintBoard" (printBoard board)

        liftIO $ withTMVarIO_ refGame $ _1 (`runner` [Event "PlayerConnected" $ toDyn colour])
        liftIO $ mainloop colour spectator refGame conn runner
        liftIO $ withTMVarIO_ refConn (return . M.adjust (L.delete conn) colour)
        where
        mainloop :: Colour -> Bool -> TMVar (ChessState , Game ChessState) -> Connection -> Runner' ChessState -> IO ()
        mainloop colour spectator st conn runner = void $ runMaybeT $ forever $ do
                isRunning <- view (_1 . running) <$> liftIO (atomically $ readTMVar st)
                unless isRunning $ hoistMaybe Nothing

                maybeMsg <- lift $ do
                    let rcv = receiveDataMessage conn
                    let rcv' = if spectator
                        then Just <$> rcv
                        else timeout (10 * 60 * secondUs) rcv
                    msg <- catch rcv' $ \ (_ :: ConnectionException) -> return Nothing

                    return (msg >>= decode . fromDataMessage)

                case maybeMsg of
                    Nothing  -> do
                        unless spectator $ do
                            lift $ withTMVarIO_ st $ _1 (`runner` [Event "PlayerDisconnected" $ toDyn colour])
                        hoistMaybe Nothing
                    Just msg -> do
                        let maybeEvent = msg >>= interpretMessage colour

                        whenJust maybeEvent $ \ ev -> do
                            lift $ withTMVarIO_ st $ _1 (`runner` [ev])

chessServer :: Int -> IO ()
chessServer port = do
    refRefGame <- newTMVarIO M.empty
    refConn  <- newTMVarIO M.empty

    serverThreadId <- myThreadId

    runServer "0.0.0.0" port (chessApp refConn refRefGame serverThreadId)

chessRunner :: Runner' ChessState
chessRunner = runGame' chess

{-
parseSquare :: Parsec String () Action
parseSquare = do
    c <- choice [char 'W' >> return White , char 'B' >> return Black]
    _ <- char ' '
    x <- fromInteger <$> natural haskell
    y <- fromInteger <$> natural haskell

    return $ Event "Touch" $ toDyn $ PlayerTouch c (x, y)

logEvent :: Show a => Rule ChessState a
logEvent e = effect $ print e

chessInput :: IO ChessEvent
chessInput = do
    str <- getLine

    case runParser parseSquare () "" str of
        Left _  -> chessInput
        Right y -> return y
-}
