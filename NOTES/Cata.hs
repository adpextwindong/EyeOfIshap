{-# LANGUAGE DeriveFunctor #-}
--Notes from https://bartoszmilewski.com/2013/06/10/understanding-f-algebras/
type Algebra f a = f a -> a

newtype Fix f = Fx (f (Fix f))

data ExprF a
  = Const Int
  | Add a a
  | Mul a a
  deriving (Functor)

{- As opposed to

data Expr
  = Const Int
  | Add Expr Expr
  | Mul Expr Expr
-}
{-
alg :: Algebra ExprF Int
alg (Const i) = i
alg (Add x y) = x + y
alg (Mul x y) = x * y
-}

alg :: f a -> a
alg = undefined --This depends on the catamorphism we're implementing

ex_init_alg :: Algebra ExprF (Fix ExprF)
ex_init_alg = Fx

g :: Functor f => Fix f -> a
g = alg . (fmap g) . unFix

unFix :: Fix f -> f (Fix f)
unFix (Fx x) = x

cata :: Functor f => (f a -> a) -> (Fix f -> a)
cata alg = alg . fmap (cata alg). unFix

-- Takes an aribtrary algebra, which is non-recursive, and returns an evaluator function Fix f -> a
