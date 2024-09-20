{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Experiment.Experiment2 ( module Experiment.Experiment2 ) where

import Control.Monad.Trans.State (StateT, modify, runStateT, execStateT, get, put)
import Control.Monad.Trans.Writer (Writer, tell, runWriter)
import Type.Reflection (SomeTypeRep (..), Typeable, (:~~:) (..), eqTypeRep, typeOf, someTypeRep)
import Data.Dynamic (toDyn)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Class ( lift )

import qualified Data.Map as M
import Data.Proxy (Proxy(..))
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Unsafe.Coerce (unsafeCoerce)

-- put the table of events with their associated types in a Reader so we can be a bit proactive about reporting bad usages of cause

data Action = forall a. Event String a | Effect (IO ())
type Consequence s = StateT s (ReaderT Events (Writer [Action]))
type Rule s a = a -> Consequence s ()

data SomeRule s where
    SomeRule :: forall s a. Typeable a => Rule s a -> SomeRule s

argType :: SomeRule s -> SomeTypeRep
argType (SomeRule @s @a _) = someTypeRep $ Proxy @a

type Events = M.Map String SomeTypeRep
type Rules s = M.Map String [SomeRule s]

type Game s = (Events, Rules s)
type Runner s = s -> [Action] -> IO s
type Runner' s = [Action] -> StateT s IO ()

cause :: Typeable a => String -> a -> Consequence s ()
cause e a = lift $ do
    es <- ask

    case M.lookup e es of
        Nothing -> error $ "Unregistered event: " ++ e
        Just (SomeTypeRep t)  -> case eqTypeRep t (typeOf a) of
            Nothing -> error $ "Argument type " ++ show (typeOf a) ++ " does not match expected type " ++ show t ++ " for event " ++ e
            Just HRefl -> lift $ tell [Event e $ toDyn a]

effect :: IO () -> Consequence s ()
effect = lift . lift . tell . (:[]) . Effect

addRule :: Rule Int Int
addRule n = do
    modify (+n)
    cause "added" ()

showRule :: Rule s ()
showRule () = do
    effect $ putStrLn "added"

registerEvent :: String -> SomeTypeRep -> Game s -> Game s
registerEvent e t (es , rs) = (_ , rs)

-- or make registering a rule automatically register it's event if possible

registerRule :: String -> SomeRule s -> Game s -> Game s
registerRule e r (es , rs) = (es , _)

-- TODO yeah that seems about right, test it
recGame' :: Game s -> Runner' s
recGame' g@(events, rules) acts = do
    forM_ acts $ \case 
        Event e a -> do
            forM_ (M.findWithDefault [] e rules) $ \ (SomeRule rule) -> do
                s_ <- get
                let ((_ , s_') , acts') = runWriter $ flip runReaderT events $ flip runStateT s_ $ rule (unsafeCoerce a)
                put s_'
                recGame' g acts'
        Effect ef -> liftIO ef
        
recGame :: Game s -> Runner s
recGame g s acts = execStateT (recGame' g acts) s

experiment :: IO ()
experiment = do
    (_, game) <- flip runStateT (M.empty, M.empty) $ do
        modify $ registerEvent "add" (someTypeRep $ Proxy @Int)
        modify $ registerEvent "added" (someTypeRep $ Proxy @())
        modify $ registerRule "add" $ SomeRule addRule
        modify $ registerRule "added" $ SomeRule showRule

    s <- recGame game 0 [Event "add" (1 :: Int), Event "add" (2 :: Int)]

    putStrLn $ "final state: " ++ show s