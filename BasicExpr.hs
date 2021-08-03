module BasicExpr where

--TODO basic a+b expr that isn't too type safe for the sake of visualizing such an AST in a browser frontend
--Next TODO, a simple lambda calculus for similar purposes

--TODO We can just take the example from leijen.pdf
--TODO we need to get this frontend up and running for a POC to show lack of type safety in a visualization frontend biting us in the ass

data PrimExpr a = Lit a |
                  BinExpr BinOp (PrimExpr a) (PrimExpr a) |
                  UnaryExpr UnaryOp (PrimExpr a)
                  deriving Show

            --OpEq | OpAnd |
data BinOp = OpPlus | OpMinus
    deriving Show

data UnaryOp = OpNeg
    deriving Show

--We can push the Num constraint onto the interpreter handling the eval type for now I guess
eval :: (Num a) => PrimExpr a -> a
eval (Lit a) = a

--TODO GADT to handle Bools
--eval (BinExpr OpEq x y) = (==) (eval x) (eval y)
--eval (BinExpr OpAnd x y) = (&&) (eval x) (eval y)
eval (BinExpr OpPlus x y) = (+) (eval x) (eval y)
eval (BinExpr OpMinus x y) = (-) (eval x) (eval y)
eval (UnaryExpr OpNeg x) = -(eval x)

equ :: (Eq a, Num a) => PrimExpr a -> PrimExpr a -> Bool
equ lhs rhs = eval lhs == eval rhs

--------------------------------------------------------------------------------
--Test expressions
--------------------------------------------------------------------------------
ts :: PrimExpr Integer
ts = (UnaryExpr OpNeg (Lit 5))

ta :: PrimExpr Integer
ta = (UnaryExpr OpNeg (BinExpr OpPlus (Lit 2) (Lit 3)))
