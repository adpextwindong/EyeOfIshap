module BasicExpr where

--TODO basic a+b expr that isn't too type safe for the sake of visualizing such an AST in a browser frontend
--Next TODO, a simple lambda calculus for similar purposes

--TODO We can just take the example from leijen.pdf
--TODO we need to get this frontend up and running for a POC to show lack of type safety in a visualization frontend biting us in the ass

data PrimExpr a = Lit a |
                  BinExpr BinOp (PrimExpr a) (PrimExpr a) |
                  UnaryExpr UnaryOp (PrimExpr a)

data BinOp = OpEq | OpAnd | OpPlus | OpMinus

data UnaryOp = OpNeg
--TODO show instance?
eval :: PrimExpr a -> a
eval (Lit a) = a
--eval (BinExpr OpEq x y) = (==) (eval x) (eval y)
--eval (BinExpr OpAnd x y) = (&&) (eval x) (eval y)
--eval (BinExpr OpPlus x y) = (+) (eval x) (eval y)
--eval (UnaryExpr OpNeg x) = -(eval x)

ts = (UnaryExpr OpNeg (Lit 5))
