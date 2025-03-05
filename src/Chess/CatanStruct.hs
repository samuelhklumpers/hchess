{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Chess.CatanStruct ( module Chess.CatanStruct ) where

import Control.Lens
import System.Random
import qualified Data.Map as M

import Chess.Game
    ( Game, Rule, Action(Event), registerRule, logGame', cause, mkEvent, Consequence )
import Chess.Internal ( whenJust, withTMVarIO_ )

import GHC.Exts ()
import GHC.IO (unsafePerformIO)
import Data.Maybe (mapMaybe, fromJust)
import Control.Monad (when, replicateM, unless, forM_)
import Control.Monad.IO.Class
import Control.Applicative ((<|>))
import Data.Bifunctor

import Data.Aeson (ToJSON, FromJSON, encode, FromJSONKey, ToJSONKey)
import GHC.Generics (Generic)
import ServerTypes
import Control.Concurrent.STM
import Network.WebSockets (Connection, sendBinaryData)


unimplemented :: a
unimplemented = error "Implement me!"

newtype Player = Player { player :: Int } deriving (Eq, Ord, Show, Generic)

instance ToJSON Player where
instance FromJSON Player where

data Phase = Initial Bool | Normal deriving (Eq, Ord, Show, Generic)

instance ToJSON Phase where
instance FromJSON Phase where

isInitial :: Phase -> Bool
isInitial (Initial _) = True
isInitial _ = False

phaseIsNormal :: Phase -> Bool
phaseIsNormal Normal = True
phaseIsNormal _ = False

data CatanTurn = CatanTurn { _turnPlayer :: Player, _turnPhase :: Phase } deriving (Eq, Ord, Generic)
makeLenses ''CatanTurn

instance ToJSON CatanTurn where
instance FromJSON CatanTurn where


instance Show CatanTurn where
    show t = show (_turnPhase t) ++ " " ++ show (player $ _turnPlayer t)

type Inventory = M.Map Item Int

data Resource = Brick | Grain | Ore | Sheep | Wood
    deriving (Eq, Ord, Show, Generic)

instance ToJSON Resource where
instance FromJSON Resource where

data Item = Road | Settlement | Resource Resource
    deriving (Eq, Ord, Show, Generic)

instance ToJSON Item where
instance FromJSON Item where

instance ToJSONKey Item where
instance FromJSONKey Item where

data Three = Three1 | Three2 | Three3 deriving (Eq, Ord, Show, Generic)
data TileIx = TileIx Int Int deriving (Eq, Ord, Show, Generic)
data LineIx = LineIx TileIx Three deriving (Eq, Ord, Show, Generic)
data VertIx = VertIx TileIx Bool deriving (Eq, Ord, Show, Generic)

instance ToJSON Three where
instance ToJSON TileIx where
instance ToJSON LineIx where
instance ToJSON VertIx where

instance FromJSON Three where
instance FromJSON TileIx where
instance FromJSON LineIx where
instance FromJSON VertIx where

data Building = BSettlement | BCity deriving (Eq, Ord, Show, Generic)

instance ToJSON Building where
instance FromJSON Building where

newtype Connections = Connections { connections :: Maybe (TMVar (M.Map User Connection)) }
makeLenses ''Connections

instance Show Connections where
    show _ = "Connections ..."

data CatanAct = ActBuildRoad | ActBuild deriving (Eq, Ord, Show, Generic)

instance ToJSON CatanAct where
instance FromJSON CatanAct where


data Catan = Catan {
    _catanStarted :: Bool,
    _catanTurn :: CatanTurn,
    _catanTurnLog :: [CatanAct],
    _catanPlayers :: M.Map User Player,
    _catanMaxPlayers :: Int,
    _catanInventories :: M.Map Player Inventory,
    _catanTiles :: M.Map TileIx (Resource, Int),
    _catanRoads :: M.Map LineIx Player,
    _catanVertx :: M.Map VertIx (Player, Building),
    _catanConns :: Connections
    }
    deriving (Show, Generic)
makeLenses ''Catan


vertVertNeighs :: VertIx -> [VertIx]
vertVertNeighs (VertIx (TileIx x y) False) = [
    VertIx (TileIx x (y - 1)) True,
    VertIx (TileIx (x - 1) (y - 1)) True,
    VertIx (TileIx (x - 1) (y - 2)) True
    ]
vertVertNeighs (VertIx (TileIx x y) True) = [
    VertIx (TileIx x (y + 1)) False,
    VertIx (TileIx (x + 1) (y + 1)) False,
    VertIx (TileIx (x + 1) (y + 2)) False
    ]

lineLineNeighs :: LineIx -> [LineIx]
lineLineNeighs (LineIx (TileIx x y) Three1) = [
    LineIx (TileIx x (y - 1)) Three2,
    LineIx (TileIx (x + 1) y) Three3,
    LineIx (TileIx x (y - 1)) Three3,
    LineIx (TileIx (x - 1) (y - 1)) Three2
    ]
lineLineNeighs (LineIx (TileIx x y) Three2) = [
    LineIx (TileIx x (y + 1)) Three1,
    LineIx (TileIx (x + 1) (y + 1)) Three3,
    LineIx (TileIx (x + 1) y) Three3,
    LineIx (TileIx (x + 1) (y + 1)) Three1
    ]
lineLineNeighs (LineIx (TileIx x y) Three3) = [
    LineIx (TileIx (x - 1) y) Three1,
    LineIx (TileIx (x - 1) (y - 1)) Three2,
    LineIx (TileIx (x - 1) y) Three2,
    LineIx (TileIx x (y + 1)) Three1
    ]



lineVertNeighs :: LineIx -> [VertIx]
lineVertNeighs (LineIx (TileIx x y) Three1) = [
    VertIx (TileIx x y) False,
    VertIx (TileIx x (y - 1)) True
    ]
lineVertNeighs (LineIx (TileIx x y) Three2) = [
    VertIx (TileIx x y) True,
    VertIx (TileIx (x + 1) (y + 1)) False
    ]
lineVertNeighs (LineIx (TileIx x y) Three3) = [
    VertIx (TileIx x (y + 1)) False,
    VertIx (TileIx (x - 1) (y - 1)) True
    ]

tileVertNeighs :: TileIx -> [VertIx]
tileVertNeighs (TileIx x y) = [
    VertIx (TileIx x y) False,
    VertIx (TileIx x y) True,
    VertIx (TileIx x (y + 1)) False,
    VertIx (TileIx (x + 1) (y + 1)) False,
    VertIx (TileIx (y - 1) (y - 1)) True,
    VertIx (TileIx x (y - 1)) True
    ]

tileTileNeighs :: TileIx -> [TileIx]
tileTileNeighs (TileIx x y) = uncurry TileIx <$> [
    (x+1,y),(x-1,y),
    (x+1,y+1),(x-1,y-1),
    (x,y+1),(x,y-1)
    ]

{-
      F     .
     / a   / \
    /   \ /   \
   .     .     .
   | 0,0 | 1,0 |
   c     |     |
   .     .     .
  / a   b a   /
 /   \ /   \ /
.     T     .
| 0,1 | 1,1 |
c     c     |
.     .     .
 \   b \   b
  \ /   \ /
   .     .
-}

vertLineNeighs :: VertIx -> [LineIx]
vertLineNeighs (VertIx (TileIx x y) False) = [
    LineIx (TileIx x y) Three1,
    LineIx (TileIx (x - 1) (y - 1)) Three2,
    LineIx (TileIx x (y - 1)) Three3
    ]
vertLineNeighs (VertIx (TileIx x y) True) = [
    LineIx (TileIx x (y + 1)) Three1,
    LineIx (TileIx x y) Three2,
    LineIx (TileIx (x + 1) (y + 1)) Three3
    ]


data CatanMsg = BuildSettleMsg VertIx Building
    | BuildRoadMsg LineIx
    deriving (Eq, Ord, Show, Generic)

instance ToJSON CatanMsg where
instance FromJSON CatanMsg where

data Tile = TileDesert | TileResource Resource deriving (Eq, Ord, Show, Generic)

instance ToJSON Tile where
instance FromJSON Tile where

data CatanResp = RespWelcome String
    | RespTile TileIx Tile
    | RespVert VertIx Player (Maybe Building)
    | RespInventory Player Inventory
    | RespRoad LineIx (Maybe Player)
    | RespNextTurn CatanTurn
    | RespUserInvalidInput
    deriving (Eq, Ord, Show, Generic)

instance ToJSON CatanResp where
instance FromJSON CatanResp where

catan0 :: Catan
catan0 = Catan False (CatanTurn (Player 0) (Initial False)) [] mempty 4 mempty mempty mempty mempty (Connections Nothing)

catanGame :: Game Catan
catanGame = mempty
    & registerRule "ServerStarted" serverStarted
    & registerRule "Start" startRule
    & registerRule "UserConnect" userConnect
    & registerRule "UserDisconnect" userDisconnect
    & registerRule "Send" sendRule
    & registerRule "InitialStart" initialStart
    & registerRule "UserBuildSettlement" userBuildSettlement
    & registerRule "BuildSettlement" buildSettlement
    & registerRule "CheckInitialEnd" checkInitialEnd
    & registerRule "UpdateInventory" updateInventory
    & registerRule "UserBuildRoad" userBuildRoad
    & registerRule "BuildRoad" buildRoad
    & registerRule "EndTurn" endTurn
    & registerRule "NextTurn" nextTurn

tmp1 :: [Action] -> [String]
tmp1 as = mapMaybe go $ fst $ catanFinal as
    where
    go (Event name _) = Just name
    go _ = Nothing

tmp2 :: [Action] -> Catan
tmp2 as = snd $ catanFinal as

evs :: [Action]
evs = [
    mkEvent "UserConnect" (User "Ping"),
    mkEvent "UserConnect" (User "Shoira"),
    mkEvent "UserConnect" (User "Lou"),
    mkEvent "UserConnect" (User "Freek"),
    mkEvent "UserBuildRoad" (User "Ping", LineIx (TileIx 0 0) Three1),
    mkEvent "UserBuildSettlement" (User "Ping", VertIx (TileIx 0 0) False, BSettlement),
    mkEvent "UserBuildRoad" (User "Ping", LineIx (TileIx 0 0) Three1),
    mkEvent "UserBuildSettlement" (User "Shoira", VertIx (TileIx 1 0) False, BSettlement),
    mkEvent "UserBuildSettlement" (User "Shoira", VertIx (TileIx 1 0) False, BSettlement),
    mkEvent "UserBuildRoad" (User "Shoira", LineIx (TileIx 1 0) Three1),
    mkEvent "UserBuildSettlement" (User "Lou", VertIx (TileIx 0 1) False, BSettlement),
    mkEvent "UserBuildRoad" (User "Lou", LineIx (TileIx 0 1) Three1),
    mkEvent "UserBuildSettlement" (User "Freek", VertIx (TileIx 1 1) False, BSettlement),
    mkEvent "UserBuildRoad" (User "Freek", LineIx (TileIx 1 1) Three1),
    mkEvent "UserBuildSettlement" (User "Freek", VertIx (TileIx 2 0) False, BSettlement),
    mkEvent "UserBuildRoad" (User "Freek", LineIx (TileIx 2 0) Three1),
    mkEvent "UserBuildSettlement" (User "Lou", VertIx (TileIx 2 1) False, BSettlement),
    mkEvent "UserBuildRoad" (User "Lou", LineIx (TileIx 2 1) Three1),
    mkEvent "UserBuildSettlement" (User "Shoira", VertIx (TileIx 3 0) False, BSettlement),
    mkEvent "UserBuildRoad" (User "Shoira", LineIx (TileIx 3 0) Three1),
    mkEvent "UserBuildSettlement" (User "Ping", VertIx (TileIx 3 1) False, BSettlement),
    mkEvent "UserBuildRoad" (User "Ping", LineIx (TileIx 3 1) Three1),
    mkEvent "Warn me" ()
    ]

catan :: Catan -> [Action] -> IO ([Action], Catan)
catan = logGame' catanGame

catanFinal :: [Action] -> ([Action], Catan)
catanFinal as = unsafePerformIO $ catan catan0 as

startRule :: Rule Catan ()
startRule () = do
    numPlayers <- use catanMaxPlayers
    catanInventories .= M.fromList [(Player i, mempty) | i <- [0 .. numPlayers - 1]]
    catanStarted .= True
    cause "InitialStart" ()

serverStarted :: Rule Catan (TMVar (M.Map User Connection))
serverStarted r = do
    catanConns .= Connections (Just r)
    liftIO $ putStrLn "Catan server started!"

userConnect :: Rule Catan User
userConnect u = do
    started <- use catanStarted
    unless started $ cause "Start" ()

    players <- use catanPlayers
    cause "Send" ([u], RespWelcome $ if u `M.member` players then "Welcome back " ++ userName u else "Welcome " ++ userName u)

    let n = M.size players

    when (u `M.notMember` players) $ do
        catanPlayers %= M.insert u (Player n)

userDisconnect :: Rule Catan User
userDisconnect u = do
    liftIO $ putStrLn $ "Disconnected: " ++ userName u

sendRule :: Rule Catan ([User], CatanResp)
sendRule (users, resp) = do
    mtConns <- connections <$> use catanConns

    case mtConns of
        Just tConns -> do
            liftIO $ withTMVarIO_ tConns $ \ conns -> do
                forM_ users $ \ u -> do
                    whenJust (conns M.!? u) $ \ c -> do
                        sendBinaryData c $ encode resp
                return conns
        Nothing -> liftIO $ print (userName <$> users, resp)

initialStart :: Rule Catan ()
initialStart () = do
    catanTurn .= CatanTurn (Player 0) (Initial False)
    catanInventories %= fmap (const $ M.fromList [(Road, 2), (Settlement, 2)])

endTurn :: Rule Catan Player
endTurn p = do
    CatanTurn p' phase <- use catanTurn
    when (p == p' && phaseIsNormal phase) $ do
        cause "NextTurn" ()

buildCheck :: [[(Item, Int)]] -> Inventory -> Maybe Inventory
buildCheck costs items = foldr ((<|>) . ok) Nothing costs
    where
    ok cost = do
        let x = M.fromList [(k, M.findWithDefault 0 k items - v) | (k, v) <- cost]
        if all (>=0) x then Just (M.union x items) else Nothing

buildCost :: Building -> [[(Item, Int)]]
buildCost BSettlement = [[(Settlement, 1)], first Resource <$> [(Brick, 1), (Grain, 1), (Sheep, 1), (Wood, 1)]]
buildCost BCity = [[(Resource Ore, 3), (Resource Grain, 2)]]

roadCost :: [[(Item, Int)]]
roadCost = [[(Road, 1)], first Resource <$> [(Brick, 1), (Wood, 1)]]

buildValid :: Player -> VertIx -> Building -> Consequence Catan Bool
buildValid p v b = do
    roads <- use catanRoads
    houses <- use catanVertx
    phase <- use $ catanTurn . turnPhase

    let emptyOk = (houses M.!? v) `elem` Nothing : [Just (p, BSettlement) | b == BCity]
    let roadOk = Just p `elem` ((roads M.!?) <$> vertLineNeighs v)
    let distOk = null $ mapMaybe (houses M.!?) (vertVertNeighs v)

    return $ emptyOk && distOk && (roadOk || isInitial phase)

buildRoadValid :: Player -> LineIx -> Consequence Catan Bool
buildRoadValid p i = do
    roads <- use catanRoads
    houses <- use catanVertx

    let emptyOk = i `M.notMember` roads
    let roadOk = p `elem` mapMaybe (roads M.!?) (lineLineNeighs i)
    let houseOk = p `elem` (fst <$> mapMaybe (houses M.!?) (lineVertNeighs i))

    return $ emptyOk && (roadOk || houseOk)

userBuildSettlement :: Rule Catan (User, VertIx, Building)
userBuildSettlement (u, v, b) = do
    mp <- use $ catanPlayers . at u

    whenJust mp $ \ p -> do
        p' <- use $ catanTurn . turnPlayer
        when (p == p') $ do
            items <- fromJust <$> use (catanInventories . at p)
            whenJust (buildCheck (buildCost b) items) $ \ items' -> do
                ok <- buildValid p v b

                phase <- use $ catanTurn . turnPhase
                turnLog <- use catanTurnLog
                -- TODO if (phase == Initial True) give resources
                let logOk = not (isInitial phase) || ActBuild `notElem` turnLog

                if ok && logOk then do
                    cause "BuildSettlement" (p, v, b)
                    cause "UpdateInventory" (p, items')
                else do
                    cause "Send" ([u], RespUserInvalidInput)

buildSettlement :: Rule Catan (Player, VertIx, Building)
buildSettlement (p, v, b) = do
    users <- M.keys <$> use catanPlayers

    catanVertx . at v ?= (p, b)
    catanTurnLog %= (ActBuild:)
    cause "Send" (users, RespVert v p (Just b))
    cause "CheckInitialEnd" ()
    cause "CheckVictory" ()
    -- TODO bind CheckVictory

checkInitialEnd :: Rule Catan ()
checkInitialEnd () = do
    phase <- use $ catanTurn . turnPhase

    when (isInitial phase) $ do
        turnLog <- use catanTurnLog

        when (all (`elem` turnLog) [ActBuildRoad, ActBuild]) $ do
            catanTurnLog .= mempty
            cause "NextTurn" ()

updateInventory :: Rule Catan (Player, Inventory)
updateInventory (p, items) = do
    users <- M.keys <$> use catanPlayers

    catanInventories . at p ?= items
    cause "Send" (users, RespInventory p items)

userBuildRoad :: Rule Catan (User, LineIx)
userBuildRoad (u, l) = do
    mp <- use $ catanPlayers . at u

    whenJust mp $ \ p -> do
        p' <- use $ catanTurn . turnPlayer
        when (p == p') $ do
            items <- fromJust <$> use (catanInventories . at p)
            whenJust (buildCheck roadCost items) $ \ items' -> do
                ok <- buildRoadValid p l

                phase <- use $ catanTurn . turnPhase
                turnLog <- use catanTurnLog

                let logOk = not (isInitial phase) || ActBuildRoad `notElem` turnLog

                if ok && logOk then do
                    cause "BuildRoad" (p, l)
                    cause "UpdateInventory" (p, items')
                else
                    cause "Send" ([u], RespUserInvalidInput)

buildRoad :: Rule Catan (Player, LineIx)
buildRoad (p, l) = do
    users <- M.keys <$> use catanPlayers

    catanRoads . at l ?= p
    cause "Send" (users, RespRoad l (Just p))
    catanTurnLog %= (ActBuildRoad:)
    cause "CheckInitialEnd" ()
    cause "CheckVictory" ()

checkVictory :: Rule Catan ()
checkVictory () = do
    -- TODO
    return ()

nextTurn' :: Int -> CatanTurn -> CatanTurn
nextTurn' n (CatanTurn (Player p) phase) = case phase of
    Initial False -> if p == n - 1
        then CatanTurn (Player $ n - 1) (Initial True)
        else CatanTurn (Player (p + 1)) (Initial False)
    Initial True -> if p == 0
        then CatanTurn (Player 0) Normal
        else CatanTurn (Player (p - 1)) (Initial True)
    Normal -> CatanTurn (Player $ (p + 1) `mod` n) Normal

nextTurn :: Rule Catan ()
nextTurn () = do
    numPlayers <- use catanMaxPlayers

    turn' <- nextTurn' numPlayers <$> use catanTurn

    catanTurn .= turn'

    users <- M.keys <$> use catanPlayers
    cause "Send" (users, RespNextTurn turn')

-- TODO, technically, you should:
-- * add a new Phase: Lock
-- * prevent the user from spending resources
--   right before they are supposed to lose them
-- But this is not (yet) a problem, 
-- because the state is in a TMVar,
-- so you can't actually get in while the interpreter
-- is running.
startNormalTurn :: Rule Catan ()
startNormalTurn _ = do
    diceRoll <- liftIO $ rollxDy 2 6

    tiles <- M.toList <$> use catanTiles
    -- TODO optimize
    forM_ tiles $ \ (i, (r, n)) ->
        when (diceRoll == n) $ do
            forM_ (tileVertNeighs i) $ \ vi -> do
                v <- use $ catanVertx . at vi
                whenJust v $ \ (p, b) ->
                    catanInventories
                        . at p . non mempty
                        . at (Resource r) . non 0 %= (+ buildingPower b)

buildingPower :: Building -> Int
buildingPower BSettlement = 1
buildingPower BCity = 2

rollDy :: Int -> IO Int
rollDy y = getStdRandom (randomR (1, y))

rollxDy :: Int -> Int -> IO Int
rollxDy x y = sum <$> replicateM x (rollDy y)
