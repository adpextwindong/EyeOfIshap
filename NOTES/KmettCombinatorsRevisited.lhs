\begin{code}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

import Control.Monad (guard, ap)

data LC a
  = Var a
  | App (LC a) (LC a)
  | Lam (LC (Maybe a)) --Binding an extra variable which is what the maybe is for.
  deriving (Functor, Foldable, Traversable)

instance Applicative LC where
  pure = Var
  (<*>) = ap

--Capture avoiding substitution
--Avoids the bound variables
--See Kmett's bound library for more advanced version
instance Monad LC where
  Var a >>= f = f a
  App l r >>= f = App (l >>= f) (r >>= f)
  Lam k >>= f = Lam $ k >>= \case
    Nothing -> pure Nothing
    Just a -> Just <$> f a

lam :: Eq a => a -> LC a -> LC a
lam v b = Lam $ bind v <$> b where
  bind v u = u <$ guard (u /= v)
\end{code}

History

Schonfinkel 1924
Haskell Curry 1930
Turner 1979
Smullyan "To Mock a Mockingbird" 1985
Sabine Broda and Luis Damas "Bracket abstraction in the combinator system CI(K) 1987

\begin{code}
--Combinatory Logic
data CL a
  = V a             --Variables
  | A (CL a) (CL a) --Applications
  | S
  | K
  | I
  | B
  | C
  deriving (Functor, Foldable, Traversable)

instance Applicative CL where
  pure = V;
  (<*>) = ap

instance Monad CL where
  V a >>= f = f a
  A l r >>= f = A (l >>= f) (r >>= f)
  S >>= _ = S
  K >>= _ = K
  I >>= _ = I
  B >>= _ = B
  C >>= _ = C

\end{code}

https://youtu.be/zhj_tUMwTe0?t=306

S x y z = (x y) (y z) -- (<*>)
K x y - x -- const
I x = x
B x y z = x (y z) -- (.)
C x y z = x z y -- flip
Y f = f (Y f) = let x = f x in x -- fix
