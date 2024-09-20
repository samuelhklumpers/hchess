open import Agda.Primitive
open import Agda.Builtin.Sigma

module IdealizedChess where

private variable
  a b c : Level
  K : Set a
  V : K → Set b

postulate
  String : Set
  
  Map : (K : Set a) → (K → Set b) → Set (a ⊔ b)
  Keys : {K : Set a} {V : K → Set b} → Map K V → Set a
  Keys-fst : {m : Map K V} → Keys m → K

  empty : {K : Set a} {V : K → Set b} → Map K V 
  lookup : (m : Map K V) (k∈m : Keys m) → V (Keys-fst k∈m)
  insert : (k : K) → Map K V → V k → Map K V

data List (A : Set a) : Set a where
  []  : List A
  _∷_ : A → List A → List A
  
_∘_ : {A : Set a} {B : Set b} {C : Set c} → (B → C) → (A → B) → A → C
(g ∘ f) x = g (f x)

types : Map String (λ _ → Set)
types = empty

Event : Set
Event = Σ (Keys types) (lookup types)

Rule : Keys types → Set
Rule k∈m = lookup _ k∈m → List Event

rules : Map (Keys types) (List ∘ Rule)
rules = insert {!!} empty ({!!} ∷ [])

-- would one survive this but with singletons-base in Haskell?
