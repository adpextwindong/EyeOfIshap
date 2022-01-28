{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
import Data.Void
import Data.Bifunctor
data BTree a = Leaf a | Fork ((BTree a),(BTree a))
  deriving Show

foldB :: (a -> b) -> ((b,b) -> b) -> BTree a -> b
foldB l _ (Leaf x) = l x
foldB l f (Fork p) = (f . (bimap (foldB l f) (foldB l f))) p

--See Bird and de Moor 97

joinB :: BTree (BTree a) -> BTree a
joinB = foldB id Fork

--Peano Nat
type Pair a = (a,a)
data Incr v = Zero | Succ v
  deriving (Show ,Functor)

data Term v = Var v | App (Pair (Term v)) | Lam (Term (Incr v))
  deriving Show

mapT :: (a -> b) -> Term a -> Term b
mapT f (Var x) = (Var . f) x
mapT f (App p) = (App . bimap (mapT f) (mapT f)) p
mapT f (Lam t) = (Lam . mapT (fmap f)) t

mapP :: (a -> b) -> Pair a -> Pair b
mapP f (x,y) = (f x, f y)
{- Page 6
 - Note the change of argument of mapT in the Lam case: the required mapping function
 - for Term (Incr a) is mapT (mapI f). As a result, mapT leaves bound variables unchanged,
 - and replaces only the free variables. In the nested definition, bound variables have become
 - part of the shape of the term.
 -
 -
 - Page 2:
 - In the body of a lambda abstraction, the set of variables is augmented with an extra element, the variable bound by the lambda. This variable is denoted by Zero; Each free variable x is renamed Succ x inside the Lambda. For example, the terms λx.x and λx.λy.x are represented as
 -
 - Lam (Var Zero) and Lam(Lam(Var (Succ Zero)))
 - λ.0                λ.λ.1
 -
 -}

tz :: Term Void --Could be Term v as there are no terms depending on v thanks to Zero not taking v
tz = Lam (Lam (Var (Succ Zero)))

-- λx.λy.x y z, containing a free variable z, may be represented as the following element of Term Char
-- λ.λ.1 0 Z
tx :: Term Char
tx = Lam (Lam (App (App (Var (Succ Zero), Var Zero), Var (Succ (Succ 'z')))))

--In other words this scheme hinges on Zero NOT taking an argument to denote its bound.
--
--An illtyped example like such fails to type check because the Succ's don't line up.
{-
BirdDebruijn.hs:47:54: error:
    * Couldn't match type `Char' with `Incr Char'
      Expected type: Term (Incr (Incr Char))
        Actual type: Term (Incr Char)
    * In the expression: Var (Succ 'z')
      In the first argument of `App', namely
        `(App (Var (Succ Zero), Var Zero), Var (Succ 'z'))'
      In the first argument of `Lam', namely
        `(App (App (Var (Succ Zero), Var Zero), Var (Succ 'z')))'
   |
47 | tx = Lam (Lam (App (App (Var (Succ Zero), Var Zero), Var (Succ 'z')))) :: Term Char
   |                                                      ^^^^^^^^^^^^^^
-}

abstract :: Eq a => a -> Term a -> Term a
abstract x = Lam . mapT (match x)

match :: Eq a => a -> a -> Incr a
match x y | x == y = Zero
          | otherwise = Succ y

subst :: a -> Incr a -> a
subst x Zero = x
subst _ (Succ y) = y

apply :: Term a -> Term (Incr a) -> Term a
apply t = joinT . mapT (subst t . fmap Var)

--The Rank2Types stuff
--Weird, without the Rank2 types stuff it really cant do it

--Generalized Fold (Bird and Paterson, 1998)
gfoldT :: (forall a. m a -> n a) ->
      (forall a. Pair (n a) -> n a) ->
      (forall a. n (Incr a) -> n a) ->
      (forall a. Incr (m a) -> m (Incr a)) ->
      Term (m b) -> n b
gfoldT v _ _ _ (Var x) = v x
gfoldT v a l k (App p) = (a . mapP (gfoldT v a l k)) p
gfoldT v a l k (Lam t) = (l . gfoldT v a l k . mapT k) t

joinT :: Term (Term a) -> Term a
joinT = gfoldT id App Lam distT

distT :: Incr (Term a) -> Term (Incr a)
distT Zero = Var Zero
distT (Succ x) = mapT Succ x
