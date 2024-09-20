{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Experiment.Experiment ( module Experiment.Experiment ) where
import Type.Reflection (SomeTypeRep, (:~:))
import Control.Monad.Trans.State (State, get)
import Data.Kind (Type)
import Data.Proxy (Proxy)

type Rule s = State s ()

nop :: Rule s
nop = return ()

constNop :: a -> Rule s
constNop _ = return ()

--type family F a :: Type

class Tag a where
    type C a
    type A a
    type S a

    what :: Proxy a -> (C a -> Rule (S a)) -> A a -> Rule (S a)

data ApplyNum = ApplyNum


instance Tag ApplyNum where
    type C ApplyNum = Int
    type A ApplyNum = ()
    type S ApplyNum = Int

    what _ c _ = applyNum c

data TagAp where
    TagAp :: (Tag a, Tag b) => Proxy a -> Proxy b -> C a :~: A b -> TagAp

type ATag = forall a. Tag a => a

applyNum :: (Int -> Rule Int) -> Rule Int
applyNum f = do
    n <- get
    f n

apply2 :: (Int -> Rule Int) -> (String -> Rule Int) -> Rule Int
apply2 f g = do
    n <- get
    f n
    g "a"

data Ap b where
    Ap :: (a -> b) -> a -> Ap b 

v' = Ap applyNum constNop

v = (applyNum, constNop)

w = (apply2, (constNop, constNop))
