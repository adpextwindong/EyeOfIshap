# Feldspar

```haskell
data Expr a where
    Value       :: Storable a => a -> Expr a
    Function    :: String -> (a -> b) -> Expr (a -> b)
    Application :: Expr (a -> b) -> Data a -> Expr b

    Variable    :: Expr a
    IfThenElse  :: Data Bool -> (a :-> b) -> (a :-> b) -> (Data a -> Expr a)

    While       :: (a :-> Bool) -> (a :-> a) -> (Data a -> Expr a)

    Parallel    :: Storable a => Data Int -> (Int :-> a) -> Expr [a]

data Data a = Typeable a => Data (Ref (Expr a))
```

Optimization in Front End

[Elliot, Finne, Moor. Compiling embedded languages. JFP 2003.](compile-dsel.pdf)

Constant folding on the fly
Variable hoisting
