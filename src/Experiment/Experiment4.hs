{-# LANGUAGE GADTs, RankNTypes, TypeApplications, ScopedTypeVariables, OverloadedStrings, DataKinds, PolyKinds, TypeOperators, TypeFamilyDependencies, MultiParamTypeClasses, UndecidableInstances #-}

module Experiment.Experiment4 (module Experiment.Experiment4) where

import Data.Kind (Type)
import Data.Singletons.Sigma ( Sigma(..) )
import Data.Singletons ( type (~>), Apply, sing )
import GHC.TypeLits.Singletons (Symbol, SSymbol)
import GHC.TypeLits (symbolSing)
import Data.Void (Void)
import Data.Bool.Singletons (If, SBool(..))
import Prelude.Singletons (PEq(..))
import Data.Eq.Singletons (SEq ((%==)))
import GHC.TypeNats (Nat)
import Data.List.Singletons


--data Rep = RepInt | RepString | RepS Rep Rep | RepP Rep Rep | RepT | RepB
-- replace Type with Rep using CustomStar

-- now you can Sigma ER Rules'
-- making extending Rules without writing down everything easier

type Entry = (Symbol, Type)
type ER = [Entry]

data UpLook0 :: ER ~> Symbol ~> Type
type instance Apply UpLook0 e = UpLook1 e

data UpLook1 :: ER -> Symbol ~> Type
type instance Apply (UpLook1 er) e = UpLook2 er e

type family UpLook2 (er :: ER) (s :: Symbol) :: Type
type instance UpLook2 '[] s = Void
type instance UpLook2 ('(s' , a) : er) s = If (s' == s) a (UpLook2 er s)

type Event er = Sigma Symbol (UpLook1 er)

type MyER = ('("start", Nat) : '("end", String) : '[])

testEventStart :: UpLook2 er "start" ~ Nat => Event er
testEventStart = symbolSing @"start" :&: 0

testEventEnd :: UpLook2 er "end" ~ String => Event er
testEventEnd = symbolSing @"end" :&: "baa"

data Lang :: ER -> Type where
    Pure  :: String -> Lang er
    Cause :: UpLook2 er s ~ a => SSymbol s -> a -> Lang er

data Rules :: ER -> ER -> Type where
    Nil  :: Rules er '[]
    Cons :: SSymbol s
         -> (a -> Lang er)
         -> Rules er er'
         -> Rules er ('(s , a) : er')

runLang :: Lang er -> ([String], [Event er])
runLang (Pure s) = ([s], [])
runLang (Cause s a) = ([], [s :&: a])

startRule :: UpLook2 er "end" ~ String => Lang er
startRule = Cause (symbolSing @"end") "started"

endRule :: String -> Lang er
endRule s = Pure ("argument: " ++ s)

type Rules' er = Rules er er

{-
data SomeRules :: Type where
    SomeRules :: Rules' er -> SomeRules
-}

myRules :: Rules' MyER
myRules = Cons sing (const startRule)
        $ Cons sing endRule
          Nil

myRulesCont :: Rules (MyER ++ er) er' -> Rules (MyER ++ er) (MyER ++ er') 
myRulesCont rs = Cons (symbolSing @"start") (const startRule)
               $ Cons (symbolSing @"end") endRule
                 rs

someLongerRules :: Rules' (MyER ++ '("extra", Int) : '[])
someLongerRules = myRulesCont
                $ Cons (symbolSing @"extra") (const $ Pure "extra")
                  Nil
                  
runOne :: Rules er' er -> Event er -> ([String], [Event er'])
runOne Nil _ = ([], [])
runOne (Cons s r rs) (s' :&: a) = case s %== s' of
    STrue  -> runLang (r a)
    SFalse -> runOne rs (s' :&: a)

run :: Rules er er -> [Event er] -> [String]
run _ [] = []
run rs (e : es) = let (a , b) = runOne rs e in a ++ run rs (b ++ es)

test :: [String]
test = run myRules [testEventStart]
