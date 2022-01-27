{-# LANGUAGE GADTs #-}
import Data.Function (fix)
--import Data.Void
--import Debug.Trace (trace)

fac :: Int -> Int
fac = fix (\f -> \n -> case n of
                         0 -> 1
                         _ -> n * f (n - 1))

{-

A New Implementation Technique for Applicative Languages

data Expr
type Name = String
type SECDAssocList = [(Name, Expr)]


Given a modest number of extra constants, called _combinators_, we can systematically translate whatever we have to say into a notion in which bound variables do not occur. This process of removing variables can be thought of as a kind of compilation and the resulting variable-free notation as a kind of object code.

Curry Feys Combinatory Logic is the standard text.

Basic Algorithm:

def f x = ...

Replace all the operators by their curried versions, giving

def f x = E

where E is an expression in which functional application is the only operation (such an expression is called a cimbination). We can now write the solution as

def f = [x] E

-- https://github.com/Superstar64/LambdaSki/blob/master/ski.hs
-- A good reference. Uses Void to eliminate non SK terms. Then uses a debruijn indice???

-}

{- Another Algoirthm For Bracket Abstraction
 - http://www.jstor.org/stable/2273733

-}

type Name = String

-- NOT DOING FOR NOW l is used with Void to denote only SKI terms, otherwise might have nonSKI terms
data Term
  = V Name
  | Lam Name Term
  | A Term Term -- Term Application
  | S -- Substitution Combinator
  | K -- Const combinator
  | I -- Id combinator
  | B -- Curry's (.) composition combinator
  | C -- Curry's flip
  deriving (Show, Eq)

infixl 1 %
(%) :: Term -> Term -> Term
(%) = A

bracketAbstract :: Name -> Term -> Term
bracketAbstract v (Lam a (V b)) | (v == a) && (a == b) = I
                                      | otherwise = K % (V b)
bracketAbstract v (A a b) = S % bracketAbstract v a % bracketAbstract v b
bracketAbstract v t@(V v') | v == v' = I
                         | otherwise = interpret t
bracketAbstract _ t = t
--bracketAbstract v x = trace ("TRACE " <> show x <> "!!") undefined

interpret :: Term -> Term
interpret (A (A (A S x) y) z) = (x % z) % (y % z) -- S
interpret (A (A K x) _) = x                       -- K
interpret (A I x) = x                             -- I
interpret (A (A (A B x) y) z) = x % (y % z)       -- B
interpret (A (A (A C x) y) z) = (x % z) % y       -- C

--Improved S reductions

interpret (A (A S (A K a)) (A K b)) = A K (a % b)
interpret (A (A S (A K a)) I) = a
interpret (A (A S (A K a)) b) = B % a % b
interpret (A (A S a) (A K b)) = C % a % b
interpret (A x y) = A (interpret x) (interpret y)
interpret t = t
--interpret x = trace (show x) undefined

pretty :: Term -> String
pretty (V n) = n
pretty (Lam n t) = "\\" <> n <> " -> " <> pretty t
pretty (A l r) = "(" <> pretty l <> " " <> pretty r <> ")"
pretty (S) = "S"
pretty (K) = "K"
pretty (I) = "I"
pretty (C) = "C"
pretty (B) = "B"

x :: Term
x = V "x"

y :: Term
y = V "y"

z :: Term
z = V "z"

f :: Term
f = V "f"

tx :: Bool
tx = bracketAbstract "x" (Lam "x" (V "x")) == I

ty :: Bool
ty = bracketAbstract "x" (Lam "x" (V "y")) == (K % y)

ts :: Bool
ts = interpret (S % x % y % z) == ((x % z) % (y % z))

tb :: Bool
tb = interpret (B % x % y % z) == (x % (y % z))

tc :: Bool
tc = interpret (C % x % y % z) == ((x % z) % y)

--Improveds

ts1 :: Bool
ts1 = interpret (S % (K % x) % (K % y)) == (K % (x % y))

ts2 :: Bool
ts2 = interpret (S % (K % x) % I) == x

ts3 :: Bool
ts3 = interpret (S % (K % x) % y) == (B % x % y)

ts4 :: Bool
ts4 = interpret (S % x % (K % y)) == (C % x % y)

tbx :: Term
tbx = bracketAbstract "x" (f % x % y)
