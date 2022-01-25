{-# LANGUAGE GADTs #-}
import Data.Function (fix)

fac :: Int -> Int
fac = fix (\f -> \n -> case n of
                         0 -> 1
                         _ -> n * f (n - 1))

data Expr
type Name = String
type SECDAssocList = [(Name, Expr)]

{-

Given a modest number of extra constants, called _combinators_, we can systematically translate whatever we have to say into a notion in which bound variables do not occur. This process of removing variables can be thought of as a kind of compilation and the resulting variable-free notation as a kind of object code.

Curry Feys Combinatory Logic is the standard text.

Basic Algorithm:

def f x = ...

Replace all the operators by their curried versions, giving

def f x = E

where E is an expression in which functional application is the only operation (such an expression is called a cimbination). We can now write the solution as

def f = [x] E

-}

-- https://github.com/Superstar64/LambdaSki/blob/master/ski.hs
-- A good reference. Uses Void to eliminate non SK terms. Then uses a debruijn indice???
