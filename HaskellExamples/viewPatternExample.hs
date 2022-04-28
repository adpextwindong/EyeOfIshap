{-# LANGUAGE ViewPatterns #-}

data Expr = Konst Int
          | Var Char
          | Fn
          deriving (Show, Eq)

nonVar :: Expr -> Maybe Expr
nonVar (Var _) = Nothing
nonVar x = Just x

foo :: Expr -> Expr
foo (nonVar -> Just x) = x
foo y                  = Konst 420

baz ((nonVar -> Just x), t) = undefined

eqVars :: (Expr, Expr) -> Maybe (Expr, Expr)
eqVars (x,y) | x == y = Just (x,x)
             | otherwise = Nothing

quux :: (Expr, Expr) -> Maybe (Expr, Expr)
quux (eqVars -> Just _) = Nothing
quux x = Just x

occElseNotEq :: Int -> Expr -> Maybe Expr
occElseNotEq = undefined

testFoo :: Int -> Expr -> Expr
testFoo i (occElseNotEq i -> Just x) = x
