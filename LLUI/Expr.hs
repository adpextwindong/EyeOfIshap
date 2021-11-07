{-# LANGUAGE GADTs #-}

import qualified Data.Map as M
import qualified Data.Set as S

type Name = String

--Idea from https://twitter.com/FarbsMcFarbs/status/1456830625617432576
--Unityped for now to make playing around easier.
data Expr a where
    Lit :: a -> Expr a
    Var :: Name -> Expr a
    Add :: (Num a) => Expr a -> Expr a -> Expr a
    Mul :: (Num a) => Expr a -> Expr a -> Expr a
    Let :: Name -> Expr a -> Expr a -> Expr a --Maybe worth lifting to a different type

--Ghosts of departed proofs here would be cool to assert the invariants of closed/open expressions

closed e = S.empty == unboundSet e
open e   = S.empty /= unboundSet e

--foo = let x = 3 in True
--(2a + 3) * x
tx = Mul (Add (Mul (Lit 2) (Var "a"))
              (Lit 3))
         (Var "x")

--"a" and "x" are free.

tz = Let "a" (Lit 3) tx
-- "a" is no longer free.

foo = let a = 3 in (let a = 4 in a)
tfoo = Let "a" (Lit 3) $ Let "a" (Lit 4) (Var "a")
--Hmmmm todo this and eval
--it would be nice if we could port this to elm for graphing

type Env a = M.Map Name [Expr a]

pushEnv :: Env a -> Name -> Expr a -> Env a
pushEnv env name e = M.insertWith (++) name [e] env

--Assumes all names reference the same variable.
unbound :: Expr a -> [Name]
unbound = S.elems . unboundSet

unboundSet :: Expr a -> S.Set Name
unboundSet = unbound' M.empty

unbound' :: Env a -> Expr a -> S.Set Name
--Traverse with new binding
unbound' env (Lit _) = S.empty
unbound' env (Var name) = case M.lookup name env of
                            Just _ -> S.empty
                            Nothing -> S.singleton name

unbound' env (Add e1 e2) = S.union (unbound' env e1) (unbound' env e2)
unbound' env (Mul e1 e2) = S.union (unbound' env e1) (unbound' env e2)
unbound' env (Let n e t) = unbound' (pushEnv env n e) t
