{-# LANGUAGE GADTs, RankNTypes, TypeApplications, ScopedTypeVariables, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Experiment.Experiment3 where

import Data.List.Singletons
import Data.Kind (Type)
import Data.Singletons.Sigma
import Data.Singletons

import Data.Text (Text)
import GHC.TypeLits.Singletons (Symbol, SSymbol)
import Data.Tuple.Singletons (STuple2(..))
import Type.Reflection
import Data.Singletons (Sing)
import GHC.TypeLits (withSomeSSymbol, symbolSing)
import Data.Singletons.Decide (Decision)


type Entry = (Symbol, SomeTypeRep')
type ER = [Entry]

--f :: Entry -> ER -> Type
--f (s , a) er = (a , (s , a) `elem` er)

data SomeTypeRep' where
    SomeRep :: forall (a :: Type). !(TypeRep a) -> SomeTypeRep'

data SSomeTypeRep (r :: SomeTypeRep') where
    SSomeTypeRep :: SSomeTypeRep r

type instance Sing = SSomeTypeRep

type family SomeType sr = t
    where
    SomeType (SomeRep @a ta) = a

data FSym0 :: ER ~> Entry ~> Type
data FSym1 :: ER -> Entry ~> Type
type FSym2 :: ER -> Entry -> Type
type family FSym2 er e where
    FSym2 er '(s , ta) = (SomeType ta , Member '(s , ta) er)

--type instance Apply FSym0 '(s , a) er = (a , Member '(s , a) er)
type instance Apply FSym0 e = FSym1 e
type instance Apply (FSym1 er) e = FSym2 er e
--type instance Apply (FSym2 er '(s , a)) = (a , Member '(s , a) er)


type Event er = Sigma Entry (FSym1 er)

data Lang :: ER -> Type where
    Pure  :: String -> Lang er
    Cause :: Typeable a => SSymbol s -> a -> Member '(s , SomeRep @a typeRep) er -> Lang er

data Member :: Entry -> ER -> Type where
    Here  :: Member e (e : er)
    There :: Member e er -> Member e (e' : er)

data Rules :: ER -> ER -> Type where
    Nil  :: Rules er '[]
    Cons :: (a -> Lang er)
         -> Rules er er'
         -> Rules er ('(s , SomeRep @a typeRep) : er')

runLang :: Lang er -> ([String], [Event er])
runLang (Pure s) = ([s], [])
runLang (Cause s a m) = ([], [STuple2 s SSomeTypeRep :&: (a , m)])

runOne :: Rules er' er -> Event er -> ([String], [Event er'])
runOne (Cons r rs) (STuple2 s rep :&: (a, Here))    = runLang $ r a
runOne (Cons r rs) (STuple2 s rep :&: (a, There m)) = runOne rs (STuple2 s rep :&: (a , m))
runOne Nil (STuple2 s rep :&: (a, Here))    = error "impossible"
runOne Nil (STuple2 s rep :&: (a, There m)) = error "impossible"

run :: Rules er er -> [Event er] -> [String]
run rs [] = []
run rs (e : es) = let (a , b) = runOne rs e in a ++ run rs (b ++ es)

myER :: ER
myER = [("start" , SomeRep @() typeRep), ("end" , SomeRep @String typeRep)]

startRule :: Lang ('("start" , SomeRep @() a) : '("end" , SomeRep @String b) : '[])
startRule = Cause (symbolSing @"end") "started" (There Here)

endRule :: String -> Lang ('("start" , SomeRep @() a) : '("end" , SomeRep @String b) : '[])
endRule s = Pure ("argument: " ++ s)


myRules :: Rules ('("start" , SomeRep @() a) : '("end" , SomeRep @String b) : '[]) ('("start" , SomeRep @() a) : '("end" , SomeRep @String b) : '[])
myRules = Cons (const startRule)
        $ Cons endRule
          Nil

test :: [String]
test = run myRules [STuple2 (symbolSing @"start") SSomeTypeRep :&: (() , Here)]

memberD :: Sing e -> SList es -> Decision (Member e es)
memberD e es = _

class MemberD a as where
    member :: Member a as

-- ??