{-# LANGUAGE ViewPatterns #-}

data Expr = Konst Int
          | Var Char
          | Fn
          deriving Show

nonVar :: Expr -> Maybe Expr
nonVar (Var _) = Nothing
nonVar x = Just x

foo :: Expr -> Expr
foo (nonVar -> Just x) = x
foo y                  = Konst 420
