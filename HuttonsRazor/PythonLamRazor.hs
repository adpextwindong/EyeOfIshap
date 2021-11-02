{-# LANGUAGE GADTs #-} --For Expr GADT
{-# LANGUAGE FlexibleInstances #-} --For partial Num instance

{-
https://www.brics.dk/RS/02/34/BRICS-RS-02-34.pdf

From "Morten Rhiger 2002 - A Foundation for Embedded Languages"

Lambda Calculus example using higher order abstract syntax.

See section 2.1

-}

--TODO NAME SUPPLY

type Name = String

data Expr a where
    Val :: Int -> Expr Int
    Add :: Expr Int -> Expr Int -> Expr Int
    Lam :: (Expr a -> Expr b) -> Expr (a -> b)
    NLam :: Name -> (Expr a -> Expr b) -> Expr (a -> b) --Named Lam for easier naming. Probably throwaway
    App :: Expr (a -> b) -> Expr a -> Expr b

instance Num (Expr Int) where
    (+) = Add

fooN :: Expr Int -> Expr Int
fooN x = x + ((Val 7) + (Val 8))

foo :: Expr Int -> Expr Int
foo x = Add x
            (Add (Val 7) (Val 8))

baz :: Expr (Int -> Int)
baz = Lam foo

bazNamed = NLam "foo" foo

quux = App bazNamed (Val 3)

--TODO we need a slightly more complicated example where baz is used multiple time. This could be a seperate razor for sharing. Probably use Gil's typesafe observable sharing bit.
--TODO version where we handle uncurrying a function into a Lam'able state

--TODO transpile foo, baz, quux to Python. Ideally have it define a function for baz so we don't have to do SSA form stuff.
--TODO SSA form version
--TODO Name supply handling
--TODO test out `language-python`

wrap :: String -> String
wrap es = "(" ++ es ++ ")"

compile :: Expr a -> String
compile (Val i) = show i
compile (Add e1 e2) = wrap $ (compile e1) ++ " + " ++ (compile e2)
--compile (Lam e) = "lambda x: " ++ compile e --TODO fix name supply issue with Lam. This doesn't work
--THIS NEEDS A NAME SUPPLY?
compile (App (NLam name _) e) = wrap $ name ++ wrap (compile e) -- I don't like this currently, we nee to figure out how to sequentially do bindings and stuff
