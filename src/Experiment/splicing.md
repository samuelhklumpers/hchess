For rules to be extensible and useful you need to be able to rewrite all inputs (alternatively, all outputs) of other rules.

That is, rather than having


>     type Rule = String -> MyMonad ()
>     
>     myRule :: Rule
>     myRule "myTrigger" = do
>         ...
>     
>         cause "e0" args0
>         cause "e1" args1
>         cause "e2" args1

you have

>     type Rule = ([String], [String], MyMonad ())
>     type HalfRule = ([String], MyMonad ())
>     
>     myRule :: Rule
>     myRule = (triggers, effects, go)
>         where
>         go = do
>             ...
>             
>             cause 0 args0
>             cause 1 args1
>             cause 2 args2

and

>     intercept :: String -> String -> Map String HalfRule -> Map String HalfRule
>     intercept t1 t2 = mapKeys (\ t -> if t == t1 then t2 else t)
>     
>     interceptRule t1 t2 m r = insert t1 (intercept t1 t2 m) r

so 

myNewRules = interceptRule "validMove" "validMoveAfterMyCheck" rules myCheck

// Best would be if intercept/insert could somehow retrieve what the type at some tag is (maybe at runtime is also fine, init is basically static so just test it once)
// (just store SomeTypeRep for the arguments of the rules at each tag)

//// Alternatively, turn every rule into a CPS thing, e.g. myRule becomes
////   myRule c0 c1 c2 = do ... ; call c0 args0 ; call c1 args1 ; call c2 args2
//// and you just build the entire call tree in the form
////   [(myRule, (c0, c1, c2)), (c0, (...)), (c1, (...)), ...]
//// You could now instead substitute c0 -> c0' and add (c0', (c0,))


As for extending the game state, you need to be able to take 

>     board :: Board

and replace it with

>     andSomeMore board :: MyBoard

without anyone noticing.

In python this means MyBoard is a subclass of Board







--------------------------------------



Notes on writing a stupid chess framework

-* Events

1. A fixed set of events

>     data ChessEvent = UncheckedMove Square Square | UncheckedMovePiece Piece Square Square | UncheckedMoveType Piece PieceType (Maybe Piece) Square Square

....No


2. Dynamically typed events

>     type Events = M.Map String SomeTypeRep
>     type Rules s = M.Map String [SomeRule s]

This is what Python looks like to a Haskell main

3. Dependently typed events

>     Events = Map String (\ _ -> Type)
>     events : Events
>     
>     myRule : (M : Events) {{_ : Is Ev B M}} -> A -> Consequence s
>     myRule e = do
>         cause ev (x :: B)

>     -- MapOver instead

>     Rules M s = Map String (\ ev -> lookup ev M -> Consequence s)

Agda, but maybe singletons if Singletons.Base.Text

4. Callbacks

>    myRule : Rule s a -> Rule s b -> Rule s c
>    myRule ra rb c = do
>        ...
>        ra _
>        rb _

This does not play well with simulation


-* Polymorphism

Polymorphism is cool until you need to stick polymorphic things into containers.
Subtypes are neat for this because you can ignore whatever your user is doing without even thinking.

On the other hand:

>     data MySubType = forall a. MyConstr a => MySubType a


-* Flexible states

1.
Like above you could try to use typeclasses

>     myRule :: HasBoard s => Consequence s ()

This is bad: what if you had two boards? (See bughouse chess)


2. 
Alternatively, just hand around lenses as appropriate

>     myRule :: Lens s a -> Lens s b -> Consequence s ()
>     myRule sa sb = do
>         sa .= _
>         sb .= _

Maybe this works.

3. 
Alternatively, more uncomfortably and accurately, restrict the state

>     myRule :: Consequence a ()
>     myRule = ...

and then zooom https://hackage.haskell.org/package/lens-5.3.2/docs/Control-Lens-Zoom.html#t:Zoomed
e.g. 

>     sa :: Lens s a and 
>     zoom sa $ myRule