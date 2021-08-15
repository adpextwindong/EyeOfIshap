https://wwtch?v=j2xYSxMkXew.youtube.com/waQ

STLC (Simply Typed Lambda Calculus) has 4 typing rules

- Variables
- Constants
- Lambda Expressions
- Applications

\begin{code}
{-# LANGUAGE GADTs #-}
data Ty = IntTy | Ty :=> Ty

data Exp where
    IntE :: Int -> Exp          -- constant int
    VarE :: Idx -> Exp          -- de Bruijn index (Nat)
    LamE :: Ty -> Exp -> Exp    -- "\(x::ty) -> e"
    AppE :: Exp -> Exp -> Exp   -- "e1 e2"

--TO BE DEFINED
--de Bruijn indexes
data Idx = Idx
\end{code}


\(x :: t1, y :: t2) -> x y)

Curried into
\(x :: t1) -> x (\(y :: t2) -> x y)

In our Exp SLTC representation it would be:

LamE t1 (AppE (VarE 0) (LamE t2 (AppE (VarE 1) (VarE 0))))

Using de Bruijn indices we can reference which binder we're refering to if theres multiple occurances of x in the expression.

NOTE: This representation does not enforce well typed terms.
