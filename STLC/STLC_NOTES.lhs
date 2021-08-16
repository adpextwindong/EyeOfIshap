https://wwtch?v=j2xYSxMkXew.youtube.com/waQ

STLC (Simply Typed Lambda Calculus) has 4 typing rules

- Variables ![Variables](STLC_Rule1.svg)
- Constants ![Constants](STLC_Rule2.svg)
- Lambda Expressions ![Lambda Expressions](STLC_Rule3.svg)
- Applications ![Applications](STLC_Rule4.svg)

\begin{code}
{-# LANGUAGE GADTs #-}
data Ty = IntTy | Ty :=> Ty

data Exp where
    IntE :: Int -> Exp          -- constant int
    VarE :: Idx -> Exp          -- de Bruijn index (Nat)
    LamE :: Ty -> Exp -> Exp    -- "\(x::ty) -> e"
    AppE :: Exp -> Exp -> Exp   -- "e1 e2"

--TO BE DEFINED
--de Bruijn indexes
data Idx = Idx
\end{code}

```
\(x :: t1, y :: t2) -> x y)
```

Curried into
```
\(x :: t1) -> x (\(y :: t2) -> x y)
```

In our Exp SLTC representation it would be:

```
LamE t1 (AppE (VarE 0) (LamE t2 (AppE (VarE 1) (VarE 0))))
```

Using de Bruijn indices we can reference which binder we're refering to if theres multiple occurances of x in the expression.

NOTE: This representation does not enforce well typed terms.

-- Small-step reduction, closed terms

We can write a function to reduce using small step operation semmantics.

If there is a beta reduction in this term, lets find the left most value. If the term is a value then return it.

NOTE: Closed terms, the type system doesn't know about it. We should only call this fn on closed terms.
      If we hit an unbound variable we will throw an error.


\begin{code}
step :: Exp -> Maybe Exp
step (IntE x) = Nothing
step (VarE n) = error "Unbound variable"
step (LamE t e) = Nothing

-- This is where we'll find out beta-reductions
-- If we find an application of a closed term there will be some beta-reduction. PROPERTY OF WELL TYPED CLOSED LAMBDA EXPRESSIONS
step (AppE e1 e2) = Just $ stepApp e1 e2 where

stepApp :: Exp -> Exp -> Exp
stepApp (IntE x) e2 = error "Type error"
stepApp (VarE n) e2 = error "Unbound variable"
stepApp (LamE t e1) e2 = undefined -- ??? Do a beta redex

--If we find another application, it could be a lambda expression so we must recurse
stepApp (AppE e1' e2') e2 = AppE (stepApp e1' e2') e2
\end{code}

Using de Bruijn indices how do we perform substitution??

How are we going to substitute e2 into the body of e1?
```
stepApp (LamE t e1) e2 = ???? -- substitute e2 into e1
```

Cases to think about while substituting e2 into e1

Simple case:

No lambda expressions in e1 to bind to
Replace all occurances of "Var 0" with e2 as its the closest

E1 could be open so
If e1 is not closed, decrement all other variables (we've removed a binder so everything gets "closer")

General case:

What if we need to traverse under a binder.

We need to avoid capturing variables in e2 while going under a lambda.

So given a Var 0, e2, we need to go into e1 looking for Var 1 beccause its under a binder.

Replace occurences of var 1 under the binder with e2.

If e1 is not closed, decrement variables >= 2 in e1, but leave 0 alone. --Those are bound.

If e2 is not closed, increment free variables in e2 by 1.


---

For traversal under n binders this becomes:

Replace occurrences of "Var n" under the binder with e2

If e1 is not closed, decrement variables >= n + 1 in e1, but leave 0..n-1 alone

If e2 is not closed, increment free variables in e2 by n.

---

This algorithmn for binding is common to all languages with binding. Nothing special to SLTC.

- Substitution Library

```haskell

data Sub = Sub                        -- abstract type

applySub :: Sub -> Idx -> Exp   -- lookup index
applySub = undefined

lift :: Sub -> Sub              -- go under binder
lift = undefined

subst :: Sub -> Exp -> Exp
subst s (IntE x)      = IntE x
subst s (VarE x)     = applySub s x
subst s (LamE ty e)  = LamE ty (subst (lift s) e)
subst s (AppE e1 e2) = AppE (subst s e1) (subst s e2)
```

Using Sub, applySub, lift and subst we can neatly handle substituion on variables, lifting while traversing under binders and substituing in an application for e1 and e2 cleanly.

Because Sub in this version depends on Exp its not generalized totally yet. So lets parameterize on that type

```haskell
type Sub a -- 'a' is the AST type, like Exp

applySub :: SubstDB a => Sub a -> Idx -> a
lift     :: SubstDB a => Sub a -> Sub a

class SubstDB a where
    var     :: Idx -> a         -- Var constructor
    subst   :: Sub a -> a -> a  -- AST traversal

singleSub :: a -> Sub a         -- create a substitution
```

- Using the library

```haskell
import Subst

instance SubstDB Exp where
    var = VarE

    subst s (IntE x)        = IntE x
    subst s (VarE x)        = applySub s x
    subst s (LamE ty e)     = LamE ty (subst (lift s) e)
    subst s (AppE e1 e2)    = AppE (subst s e1) (subst s e2)
```

Then coming back to stepApp

```haskell

stepApp :: Exp -> Exp -> Exp
stepApp (IntE x) e2 = error "Type error"
stepApp (VarE n) e2 = error "Unbound variable"
stepApp (LamE t e1)     e2 = subst (singleSub e2) e1
stepApp (AppE e1' e2')  e2 = AppE (stepApp e1' e2') e2
```

Now we can neatly do a beta redex by creating a single substition with e2 then applying the substituion to e1.

NOTE: Doing this naively can be expensive. Will be talked about later.

- Open vs Closed terms in STLC

Closed term: All identifiers bound by closest containing abstraction

```
\x.y.yx
```

Open term: Some identifiers not bound

```
\x.yx
```

Legal lambda calculus programs: all closed terms


- A strongly-typed AST

Lets use types to rule out errors

The usage of error inside the previous code along with Maybe was confusing. It didn't really signal unwanted usage that well.
Nothing represented when the user was stepping a well typed closed lambda expression that was a value. No further steps should be taken.
Error on the otherhand was thrown when it shouldn't be called in the first place because the lambda expression wasn't closed or well typed.

To fix this:

We add two type parameters to Exp. This will represent the typing context and the type of the expression.

Exp g t, will type check in context g and has type t.

[Ty] is the typing context, nth Ty in list is type of nth bound variable.

So now we can talk about the types of the free variables in the expression and the entire expression.

```haskell

data Ty = IntTy | Ty :-> Ty

data Exp :: [Ty] -> Ty -> Type where
    IntE :: Int -> Exp g IntTy          -- Always has the Int base type because its an IntE
    VarE :: Idx g t -> Exp g t          -- Its also an Index into a typing context

    --We modify the type context in the LamE, we're adding a new bound variable, so we cons it on.
    LamE :: Sing (t1 :: Ty) -> Exp (t1:g) t2 -> Exp g (t1 :-> t2)
    AppE :: Exp g (t1 :-> t2) -> Exp g t1 -> Exp g t2

--Sing is a singleton, revisited later
```

Strong typing is then enforced at the Haskell level.

We'll need a Strongly-typed substituion library then


```haskell
-- A reference to a specific element in the list

data Idx :: [k] -> k -> Type where
    Z :: Idx (t:g) t
    S :: Idx g t -> Idx (u:g) t

-- Idx has a polymorphic kind
Idx :: forall k. [k] -> k -> Type
Exp :: [Ty] -> Ty -> Type

```

Small-step reduction on closed terms with this more strongly typed representation

```haskell
-- Empty list type context to make sure we only give closed terms to the step function
-- Result type is also closed and the same type t, so types are preserved.
step :: Exp '[] t -> Maybe (Exp '[] t)
step (IntE x)       = Nothing
step (VarE n)       = case n of {} -- impossible Haskell's pattern matching can handle this
step (LamE t e)     = Nothing
step (AppE e1 e2)   = Just $ stepApp e1 e2 where

stepApp :: Exp '[] (t1 :-> t2) -> Exp '[] t1 -> Exp '[] t2
--stepApp (IntE x) e2 = error "Type Error" --inaccesible case, can't even compile this
--This is infered by (t1 :-> t2) not being IntTy in the signature

stepApp (VarE n) e2     = case n of {}
stepApp (LamE t e1) e2  = subst (singleSub e2) e1 --Type checking this line will require the subst library knowing about type checking
stepApp (AppE e1' e2') e2 = AppE (stepApp e1' e2') e2
```

Strongly-typed substituion library

```haskell

-- Substitution applies to indices in context g1
-- and produces terms in context g2

type Sub (a:: [k] -> k -> Type) (g1 :: [k]) (g2 :: [k])

class SubstDB (a :: [k] -> k -> Type) where
    Var   :: Idx g t -> a g t
    subst :: Sub a g1 g2 -> a g1 t -> a g2 t   --Type is preservered and context is changed

singleSub :: a g t -> Sub a (t:g) g

instance SubstDB Exp where
    var :: Idx g t -> Exp g t
    var = VarE

    --Virtually the same but the type information prevents lack of lifting/applySub/subst
    subst :: Sub Exp g1 g2 -> Exp g1 t -> Exp g2 t
    subst s (IntE x)        = IntE x
    subst s (VarE x)        = applySub s x
    subst s (LamE ty e)     = LamE ty (subst (lift s) e)
    subst s (AppE e1 e2)    = AppE (subst s e1) (subst s e2)
```

From STLC to System F

SLTC + Two typing rules
