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
import Data.Function (fix)
import Debug.Trace (trace)
import Network.WebSockets.Connection (connectionSentClose)
import Data.Functor (void)
import Data.Proxy (Proxy(..))
import Control.Monad.Trans.State.Lazy (StateT, modify, execStateT)
import Control.Monad.Trans.Reader (Reader, ask, runReader)
import Data.Typeable (Typeable)

type ChessGame = Game ChessState
type Build a = StateT a (Reader a) ()

registerRuleS :: (Monad m, Typeable a) => String -> Rule s a -> StateT (Game s) m ()
registerRuleS e a = modify $ registerRule e a

spliceRuleS :: (Monad m, Typeable a) => String -> String -> Rule s a -> StateT (Game s) m ()
spliceRuleS e e' a = modify $ spliceRule e e' a

overwriteRuleS :: (Monad m, Typeable a) => String -> Rule s a -> StateT (Game s) m ()
overwriteRuleS e a = modify $ overwriteRule e a

build :: Monoid a => Build a -> a
build b = fix $ runReader (execStateT b mempty)

chessBuilder :: HasCallStack => Build ChessGame
chessBuilder = do
    self <- lift ask
    registerRuleS "Touch" (playerTouch touch)
    _ <- do
        registerRuleS "UpdateSelection" (markAvailableMoves self id)
        registerRuleS "UpdateSelection" (clearAvailableMoves touch)
        registerRuleS "UpdateSelection" (updateSelection touch)
        registerRuleS "SendSelect" (sendSelect board)
        registerRuleS "Touch1" (touch1Turn turn)
        registerRuleS "Touch1Turn" (touch1Piece board)
        registerRuleS "Touch1Piece" (touch1Colour touch)
    registerRuleS "Touch2" touch2
    registerRuleS "UncheckedMove" (uncheckedMovePiece board) -- TODO how does zooming interact with compiling?
    _ <- do
        registerRuleS "UncheckedMovePiece" (uncheckedMoveTurn turn)
        registerRuleS "UncheckedMoveTurn" (uncheckedMoveSelf board)
        registerRuleS "UncheckedMoveSelf" specializeMove
        _ <- do
            registerRuleS "UncheckedKMove" (kingMove castling)
            registerRuleS "UncheckedKMove" (castlingMove board castling)
            registerRuleS "UncheckedQMove" (queenMove board)
            registerRuleS "UncheckedRMove" (rookMove board castling)
            registerRuleS "UncheckedBMove" (bishopMove board)
            registerRuleS "UncheckedNMove" knightMove
            registerRuleS "UncheckedPMove" (pawnMove board)
            registerRuleS "UncheckedPMove" (pawnDoubleMove board turn enpassant)
            registerRuleS "UncheckedPMove" (pawnCapture board)
            registerRuleS "UncheckedPMove" (pawnEP turn enpassant)
        spliceRuleS "UncheckedMoveSelf" "UncheckedMoveNull" (nullMove "UncheckedMoveNull")

        registerRuleS "PromotionCheck" (promotionCheck promoting)
        registerRuleS "CheckedMovePiece" pawnZero

    registerRuleS "CheckedMovePiece" (moveMayCapture board)
    _ <- do
        registerRuleS "NonCapture" nonCapture
        registerRuleS "Capture" captureZero
        _ <- do
            registerRuleS "Zeroing" (zeroRule turn zeroing)
        registerRuleS "Capture" capture

        registerRuleS "RawMove" rawMove
        registerRuleS "Take" rawTake
        _ <- do
            registerRuleS "PutTile" (putTile board)
        return ()

    registerRuleS "MoveEnd" (nMoveRule 50 turn zeroing)
    registerRuleS "MoveEnd" (nFoldRepetition 3 board history)
    registerRuleS "MoveEnd" (winRule board)
    registerRuleS "NextTurn" (nextSubTurn turn promoting)
    _ <- do
        registerRuleS "NextSubTurn" (promotionPrompt turn)
    _ <- do
            registerRuleS "NextSubTurn" (nextTurnSendStatus turn)
            registerRuleS "NextSubTurn" (turnStart :: Chess ()) -- TODO make this more accurate
            registerRuleS "TryPromote" (tryPromote turn promoting)

    registerRuleS "PlayerConnected" connectSendBoard
    registerRuleS "PlayerConnected" (turnStart :: Chess Colour) -- TODO don't do this instead do RoomCreated or so

    registerRuleS "PlayerConnected" (connectSendStatus turn)
    registerRuleS "TestCloseRoom" (testCloseRoom closing)
    registerRuleS "Draw" sendDraw
    registerRuleS "Win" sendWin
    registerRuleS "Win" (winClose :: Chess Colour)
    registerRuleS "Draw" (winClose :: Chess ())

    registerRuleS "SendBoardAll" serveBoardAll
    registerRuleS "SendTileAll" serveDrawTileAll
    registerRuleS "SendTile" (sendTile board)
    registerRuleS "SendDrawTile" sendTileImage
    registerRuleS "SendDrawTileImage" serveDrawTile
    registerRuleS "SendBoard" (serveBoard board)
    registerRuleS "SendTurn" serveTurn
    registerRuleS "SendPromotionPrompt" servePromotionPrompt
    registerRuleS "SendMarkAvailableMove" serveMarkAvailableMove
    registerRuleS "SendClearAvailableMoves" serveClearAvailableMoves

    registerRuleS "MoveEnd" movePrintBoard

    registerRuleS "TurnStart" (idRule "MarkMoves" :: Chess ()) -- TODO bypassing should probably not be like this
    registerRuleS "MarkMoves" (markMoves self "UncheckedMoveSelf" "UncheckedMoveSelf" (Proxy @PieceMove) id)
    registerRuleS "StaleCheck" (staleMate moveCache turn :: Rule ChessState ())


    -- hush
    registerRuleS "PrintBoard" (returnRule :: Rule ChessState ())
    registerRuleS "SendRaw" (returnRule :: Rule ChessState SendRaw)
    -- registerRuleS "TurnStart" (returnRule :: Rule ChessState ())

    return ()

--data ChessOpts = Atomic -- | Checkers | SelfCapture
--    deriving (Show, Ord, Eq, Generic)

type ChessOpts = String

atomicDefaults :: [(Int, Int)]
atomicDefaults =
    [ (-1 , -1), ( 0,  -1), ( 1 , -1)
    , (-1 ,  0), ( 0 ,  0), ( 1 ,  0)
    , (-1 ,  1), ( 0 ,  1), ( 1 ,  1)]

applyChessOpt :: ChessOpts -> Build ChessGame
applyChessOpt "Atomic" = do
    registerRuleS "Capture" (atomicExplosion atomicDefaults board)
applyChessOpt "RTS"    = do 
    overwriteRuleS "Touch1" (idRule "Touch1Turn" :: Chess PlayerTouch)
    overwriteRuleS "UncheckedMovePiece" (idRule "UncheckedMoveTurn" :: Chess PieceMove)
applyChessOpt "SelfCapture" = do
    overwriteRuleS "UncheckedMoveTurn" (idRule "UncheckedMoveSelf" :: Chess PieceMove)
applyChessOpt "Anti" = do
    spliceRuleS "Win" "AntiWin" antiChess
applyChessOpt "Checkers" = do
    self <- lift ask
    spliceRuleS "UncheckedMoveSelf" "UncheckedMoveCheckers" (checkers turn captureCache)
    overwriteRuleS "MarkMoves" (markMoves self "UncheckedMoveSelf" "UncheckedMoveCheckers" (Proxy @PieceMove) id)
applyChessOpt "StrategoV" = do
    spliceRuleS "SendDrawTileImage" "SendDrawTileImageStratego" stratego
    overwriteRuleS "SendBoard" serveBoardTiles
applyChessOpt "StrategoC" = do
    spliceRuleS "Capture" "StrategoCapture" strategoCapturesAA
applyChessOpt opt = trace ("Unrecognized option: " ++ opt) $ return ()

chessWithOpts :: [ChessOpts] -> Build ChessGame
chessWithOpts []           = chessBuilder
chessWithOpts (opt : opts) = do
    chessWithOpts opts
    applyChessOpt opt

chess :: Game ChessState
chess = build chessBuilder


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

        let ruleset = build $ chessWithOpts opts
        
        let initialState = case maybeFEN >>= rightToMaybe . parseFEN of
                Nothing -> chessInitial
                Just x  -> chessInitial { _board = x }

        refGame <- liftIO $ withTMVarIO refRefGame $ setdefaultM room $ newTMVarIO (initialState, ruleset)
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
