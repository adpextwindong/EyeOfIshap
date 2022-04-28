{-# LANGUAGE ViewPatterns #-}

import Control.Monad.Logic ()
import Data.Set
import Data.List (intercalate)
import Debug.Trace

--Denotes an equation between first order logic expressions
type Eqn = (Expr, Expr)

data Expr = Var String
          | Fn Char [Expr]
          deriving (Ord, Eq)

instance Show Expr where
  show (Var name) = show name
  show (Fn sym expr) = sym : "(" ++ intercalate "," (fmap show expr) ++ ")"

--View Patterns
notVar :: Expr -> Maybe Expr
notVar (Var _) = Nothing
notVar x = Just x

eqVars :: Eqn -> Maybe Eqn
eqVars (x,y) | x == y    = Just (x,x)
             | otherwise = Nothing

--occElseNotEq

--Example from page 262 of MM

s0 = [(Fn 'g' [Var "x2"], Var "x1")
     ,(Fn 'f' [Var "x1", Fn 'h' [Var "x1"], Var "x2"], Fn 'f' [Fn 'g' [Var "x3"], Var "x4", Var "x3"])]

--Note: the Set ordering constraint will make the answers look a bit weird.
termReduction :: Set Eqn -> Eqn -> Set Eqn
termReduction s e@((Fn l xs),(Fn r ys)) | l == r = (s \\ singleton e) `union` subEqns
  where subEqns = fromList $ zip xs ys
termReduction _ _ = error "Term reduction only on matching root fns"

ex1 = termReduction (fromList s0) $ s0 !! 1
tx1 = fromList [(Fn 'g' [Var "x2"], Var "x1")
               ,(Var "x1", Fn 'g' [Var "x3"])
               ,(Fn 'h' [Var "x1"], Var "x4")
               ,(Var "x2", Var "x3")]
tt1 = ex1 == tx1
