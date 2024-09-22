{-# LANGUAGE TypeFamilies, LambdaCase, FlexibleInstances,
RankNTypes, GADTs, ScopedTypeVariables, TypeApplications, PatternSynonyms #-}


module Chess.Game ( module Chess.Game ) where

import qualified Data.Map as M

import Control.Monad.Trans.State.Lazy ( StateT(runStateT) , get, execStateT, put )
import Control.Monad.Trans.Writer.Lazy ( tell, Writer, runWriter )
import Control.Monad.Trans.Class ( MonadTrans(..) )
import Control.Monad (forM_)
import Data.Dynamic (Dynamic (..), Typeable, toDyn)
import Type.Reflection (SomeTypeRep (..), type (:~~:) (..), eqTypeRep, typeOf, pattern TypeRep, TypeRep)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Control.Lens ((?~), at, (<>~), (^.))
import Data.Foldable (foldrM)
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Debug.Trace (trace)


-- * Events, Rules, and Games

data Action = Event String Dynamic | Effect (IO ())
type Consequence s = StateT s (ReaderT Events (Writer [Action]))
type Rule s a = a -> Consequence s ()

argType :: Typeable a => Rule s a -> TypeRep a
argType _ = TypeRep

someArgType :: SomeRule s -> SomeTypeRep
someArgType (SomeRule r) = SomeTypeRep $ argType r

data SomeRule s where
    SomeRule :: Typeable a => Rule s a -> SomeRule s

type Events = M.Map String SomeTypeRep
type Rules s = M.Map String [SomeRule s]

type Game s = (Events, Rules s)
type Runner' s = s -> [Action] -> IO s
type Runner s = [Action] -> StateT s IO ()

mkEvent :: Typeable a => String -> a -> Action
mkEvent e a = Event e $ toDyn a

cause :: (Typeable a, HasCallStack) => String -> a -> Consequence s ()
cause e a = lift $ do
    es <- ask

    --seq (unsafePerformIO $ putStrLn e) (return ())

    case M.lookup e es of
        Nothing -> trace ("Unregistered event: " ++ e ++ " in\n" ++ prettyCallStack callStack) (return ())
        Just (SomeTypeRep t)  -> case eqTypeRep t (typeOf a) of
            Nothing -> error $ "Argument type " ++ show (typeOf a) ++ " does not match expected type " ++ show t ++ " for event " ++ e ++ " while running!"
            Just HRefl -> lift $ tell [mkEvent e a]

effect :: IO () -> Consequence s ()
effect = lift . lift . tell . (:[]) . Effect

registerEvent :: String -> SomeTypeRep -> Game s -> Game s
registerEvent e t (es , rs) = (es & at e ?~ t , rs)

registerRule :: (Typeable a, HasCallStack) => String -> Rule s a -> Game s -> Game s
registerRule e r (es , rs) = let tr' = SomeTypeRep (argType r) in
    case es ^. at e of
    Nothing -> registerRule' e (SomeRule r) (es & at e ?~ tr' , rs)
    Just tr -> if tr == tr' then
            registerRule' e (SomeRule r) (es , rs)
        else
            error $ "Argument type " ++ show tr' ++ " does not match expected type "
                                     ++ show tr ++ " for event " ++ e ++ " while registering"
    where
    registerRule' :: String -> SomeRule s -> Game s -> Game s
    registerRule' e r (es , rs) = (es , rs & at e <>~ Just [r])

registerRules :: HasCallStack => String -> [SomeRule s] -> Game s -> Game s
registerRules e rs' (es , rs) = let trs = map someArgType rs' in
    case es ^. at e of
    Nothing -> case rs' of
        (r : rs'') -> if all ((== someArgType r) . someArgType) rs'' then
                registerRules' e rs' (es & at e ?~ someArgType r , rs)
            else
                error $ "Argument types " ++ show trs ++ " do not match"
        [] -> (es, rs)
    Just tr -> if all (tr ==) trs then
            registerRules' e rs' (es , rs)
        else
            error $ "Argument types " ++ show trs ++ " do not match expected type "
                                     ++ show tr ++ " for event " ++ e ++ " while registering"
    where
    registerRules' :: String -> [SomeRule s] -> Game s -> Game s
    registerRules' e rs' (es , rs) = (es , rs & at e <>~ Just rs')

overwriteRule :: (Typeable a) => String -> Rule s a -> Game s -> Game s
overwriteRule e r (es , rs) = let tr' = SomeTypeRep (argType r) in
    registerRule' e (SomeRule r) (es & at e ?~ tr' , rs)
    where
    registerRule' :: String -> SomeRule s -> Game s -> Game s
    registerRule' e r (es , rs) = (es , rs & at e ?~ [r])

spliceRule :: (Typeable a, HasCallStack) => String -> String -> Rule s a -> Game s -> Game s
spliceRule old new r game@(_, rs) = game
    & registerRules new (M.findWithDefault [] old rs)
    & overwriteRule old r

runGame :: Game s -> Runner s
runGame g@(events, rules) acts = do
    forM_ acts $ \case
        Event e (Dynamic ta a) -> do
            forM_ (M.findWithDefault [] e rules) $ \ (SomeRule @tr rule) -> do
                case eqTypeRep ta (TypeRep @tr) of
                    Nothing -> do
                        _ <- error $ "Actual event type " ++ show ta ++ " of " ++ e ++ " does not match expected type " ++ show (TypeRep @tr) ++ " while running"
                        return ()
                    Just HRefl -> do
                        s_ <- get
                        let ((_ , s_') , acts') = runWriter $ flip runReaderT events $ flip runStateT s_ $ rule a
                        put s_'
                        runGame g acts'
        Effect ef -> liftIO ef

runGame' :: Game s -> Runner' s
runGame' g s acts = execStateT (runGame g acts) s

type Simulator s = [Action] -> s -> s

simGameUntil :: (Action -> Bool) -> Game s -> [Action] -> s -> Either (s , Action) s
simGameUntil _ _                 []           s = Right s
simGameUntil p g@(events, rules) (act : acts) s
    | p act = Left (s , act)
    | otherwise = case act of
        Event e (Dynamic ta a) -> do
            s' <- foldrM go s (M.findWithDefault [] e rules)
            simGameUntil p g acts s'
            where
            go (SomeRule @tr rule) s' = case eqTypeRep ta (TypeRep @tr) of
                Nothing -> do
                    error $ "Actual event type " ++ show ta ++ " of " ++ e ++ " does not match expected type " ++ show (TypeRep @tr)
                Just HRefl -> do
                    let ((_ , s'') , acts') = runWriter $ flip runReaderT events $ flip runStateT s' $ rule a
                    simGameUntil p g acts' s''
        Effect _ -> simGameUntil p g acts s

runGameIO :: Runner' s -> IO Action -> s -> IO s
runGameIO runner input s = do
    e <- input
    s' <- runner s [e]
    runGameIO runner input s'


-- * Examples

startRule :: Rule String ()
startRule () = do
    currTxt <- get
    effect $ putStrLn $ "starting on: " ++ currTxt
    put "new state"
    cause "end" "started"

endRule :: Rule String String
endRule txt = do
    currTxt <- get
    effect $ putStrLn $ "ending with: " ++ txt ++ ", on: " ++ currTxt
    cause "ended" ()

myGame :: Game String
myGame = mempty
    & registerEvent "start" (SomeTypeRep $ TypeRep @())
    & registerEvent "end" (SomeTypeRep $ TypeRep @String)
    & registerEvent "ended" (SomeTypeRep $ TypeRep @())
    & registerRule "start" startRule
    & registerRule "end" endRule

myGameTest :: IO String
myGameTest = runGame' myGame "initial state" [mkEvent "start" ()]

{-
This is doomed unless you go all the way and internalize variables, conditionals, etc...

data ActionF s a where
    EventF :: String -> Dynamic -> ActionF s ()
    EffectF :: IO () -> ActionF s ()
    AskF :: ActionF s a
    GetF :: ActionF s s
    BindF :: ActionF s a -> (a -> ActionF s b) -> ActionF s b
    CondF :: ActionF 
-}
{-
deriving instance Functor (ActionF a)
type Rule' s a = FreeT ActionF (ReaderT a (StateT s (Reader Events))) ()


myRule :: Rule' String Int
myRule = do
    _ <- FreeT $ do
        x <- ask
        lift $ do
            y <- get
            lift $ do
                lift $ do
                    return $ Free $ EventF y (toDyn x)
    return ()
-}
