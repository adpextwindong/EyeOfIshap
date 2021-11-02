{-# LANGUAGE GADTs #-}

{-
https://www.brics.dk/RS/02/34/BRICS-RS-02-34.pdf

From "Morten Rhiger 2002 - A Foundation for Embedded Languages"

Lambda Calculus example using higher order abstract syntax.

See section 2.1

-}

--TODO NAME SUPPLY

data Expr a where
    Val :: Int -> Expr Int
    Add :: Expr Int -> Expr Int -> Expr Int
    Lam :: (Expr a -> Expr b) -> Expr (a -> b)
    App :: Expr (a -> b) -> Expr a -> Expr b

foo :: Expr Int -> Expr Int
foo x = Add x
            (Add (Val 7) (Val 8))

baz :: Expr (Int -> Int)
baz = Lam foo

quux = App baz (Val 3)

--shouldNotTypeCheck = App baz App
