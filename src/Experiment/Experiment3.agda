open import Agda.Primitive renaming (Set to Type)
open import Agda.Builtin.Sigma
open import Agda.Builtin.String
open import Agda.Builtin.List
open import Agda.Builtin.Maybe
open import Agda.Builtin.Nat
open import Agda.Builtin.Equality
open import Agda.Builtin.Unit
open import Agda.Builtin.Bool

module Experiment3 where

private variable
  a b c p q r : Level
  A B C K : Type a
  V : K → Type b

data ⊥ : Type where

_×_ : (A : Type a) (B : Type b) → Type (a ⊔ b)
A × B = Σ A λ _ → B

module Attempt2 where
  Entry = String × Type
  ER = List Entry

  typ : ER → String → Type
  typ [] s = ⊥
  typ ((s' , a) ∷ er) s with primStringEquality s' s
  ... | false = {!!}
  ... | true = {!!}

  -- ...

  -- the first (constant) er in Rules is kind of annoying

  -- disallowing self-reference (i.e., making `er` shrink) makes things easier
  -- but the order in which you have to write rules is weird

  -- Lang er should probably be something like forall er'. er' > er -> Lang' er'
  -- and then just cast the entire thing every time you register a new event

  -- you cannot make a card game in which you draw cards until you can play one without self-reference

  SomeRules = Σ ER Rules
  
module Attempt1 where

  Entry = String × Type
  EventRegistry = List Entry
  ER = EventRegistry

  data Any {A : Set a} (P : A → Type p) : List A → Type (a ⊔ p) where
    here  : ∀ {x xs} →     P x  → Any P (x ∷ xs)
    there : ∀ {x xs} → Any P xs → Any P (x ∷ xs)

  _∈_ : {A : Set a} → A → List A → Type a
  x ∈ xs = Any (x ≡_) xs

  Event : ER → Type (lsuc lzero)
  Event er = Σ Entry λ (s , A) → A × ((s , A) ∈ er)

  data Lang (er : ER) : Type₁ where
    pure  :           String           → Lang er
    _>>_  : Lang er → Lang er          → Lang er
    cause : ∀ {B} s → B → (s , B) ∈ er → Lang er

  {-
  NB: making `er` shrink as you go down _∷_ is a great way of forcing termination

  data Rules : ER → Type (lsuc lzero) where
    []  : Rules []
    _∷_ : ∀ {s A er}
        → (A → Lang er)
        → Rules er 
        → Rules ((s , A) ∷ er)
  -}

  _++_ : {A : Set a} →  List A → List A → List A
  []       ++ ys = ys
  (x ∷ xs) ++ ys = x ∷ (xs ++ ys)


  data Rules (er : ER) : ER → Type (lsuc lzero) where
    []  : Rules er []
    _∷_ : ∀ {s A er'}
        → (A → Lang er)
        → Rules er er'
        → Rules er ((s , A) ∷ er')

  runLang : ∀ {er} → Lang er → List String × List (Event er)
  runLang (pure x) = x ∷ [] , []
  runLang (r >> s) = let (rs , re) = runLang r in
                     let (ss , se) = runLang s in rs ++ ss , re ++ se
  runLang (cause s b p) = [] , ((s , _) , b , p) ∷ []

  {-
  data _⊂_ {A : Set a} : List A → List A → Type a where
    ⊂-refl : ∀ {xs}     → xs ⊂ xs
    ⊂-∷    : ∀ {xs y ys} → xs ⊂ ys → xs ⊂ (y ∷ ys)

  map : {A : Set a} {B : Set b} → (A → B) → List A → List B
  map f [] = []
  map f (x ∷ xs) = f x ∷ map f xs

  weaken : ∀ {xs ys} → xs ⊂ ys → Event xs → Event ys
  weaken ⊂-refl (e , a , q) = e , a , q
  weaken (⊂-∷ p) e = let (e' , a , q) = weaken p e in e' , a , there q
  -}

  lookup : ∀ {er er'} → Event er → Rules er' er → List String × List (Event er')
  lookup ((s , A) , a , here refl) (x ∷ xs) = runLang (x a)
  lookup ((s , A) , a , there p) (x ∷ xs) = lookup ((s , A) , a , p) xs

  {-# TERMINATING #-}
  run : ∀ {er} → Rules er er → List (Event er) → List String
  run xs [] = []
  run xs (e ∷ es) = let (a , b) = lookup e xs in
                    a ++ run xs (b ++ es)

  -- make ∈ compute by setting Entry = String × TypeRep and then make
  -- startRule : ∀ {er} {{_ : ("end", String) ∈ er}} → Lang er

  myER : ER
  myER = ("start" , ⊤) ∷ ("end" , String) ∷ []

  startRule : ∀ {er} → ("end", String) ∈ er → Lang er
  startRule p = pure "start" >> cause "end" "started" p

  endRule : ∀ {er} → String → Lang er
  endRule s = pure (primStringAppend "argument: " s)

  myRules : Rules myER myER
  myRules = (λ _ → startRule (there (here refl)))
          ∷ endRule
          ∷ []

  test : List String
  test = run myRules ((("start" , ⊤) , tt , here refl) ∷ [])

