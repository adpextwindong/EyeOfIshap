{-# LANGUAGE GADTs #-}
import Data.Void (Void)

data Ty
  = TInt
  | TBool
  deriving (Show, Eq)

type Identifier = String
type Name = Identifier

data Expr
  = ConstInt Int
  | ConstBool Bool
  | Add Expr Expr
  | Equal Expr Expr
  | IFE Expr Expr Expr
  deriving Show
--  | Var Identifier

--TODO typecheck stmts, typing environment, typecheck program
data Statement
  = Let Identifier Ty Expr       -- let x : Ty = Expr  NOTE: Type Annotations must be added
  | Sequence Statement Statement -- gamma1;gamma2
  -- | VarDecl Identifier Ty (Maybe Expr)
  -- | FunDecl Name [Identifier] Statement
  -- | Assignment Identifier Expr
  | Block [Statement]

type Program = [Statement]

data TyError
  = TypeMismatch String -- Expected vs Given
  deriving Show

data TExpr where
  TConstInt :: Int -> TExpr
  TConstBool :: Bool -> TExpr
  TAdd :: TExpr -> TExpr -> TExpr
  TEqual :: TExpr -> TExpr -> TExpr --EQ is only between prims
  TIFE :: Ty -> TExpr -> TExpr -> TExpr
  deriving Show

tyOf :: TExpr -> Ty
tyOf (TConstInt _)  = TInt
tyOf (TConstBool _) = TBool
tyOf (TAdd _ _)     = TInt
tyOf (TEqual _ _)   = TBool
tyOf (TIFE ty _ _)  = ty

sameTy :: TExpr -> TExpr -> Bool
sameTy e1 e2 = tyOf e1 == tyOf e2

typeCheck :: Expr -> Either TyError TExpr
typeCheck (ConstInt v) = Right $ TConstInt v
typeCheck (ConstBool v) = Right $ TConstBool v
typeCheck (Add e1 e2) = do
  t1 <- typeCheck e1
  t2 <- typeCheck e2

  case t1 of
    (TConstInt _) -> case t2 of
              (TConstInt _) -> Right $ TAdd t1 t2
              _ -> Left $ TypeMismatch "Add e2 expects Int"
    _ -> Left $ TypeMismatch "Add e1 expects Int"

{-
Γ|- e1 : Int Γ|- e2: Int
------------------------
   Γ|- e1 + e2 : Int
-}

typeCheck (Equal e1 e2) = do
  t1 <- typeCheck e1
  t2 <- typeCheck e2

  if sameTy t1 t2
  then Right $ TEqual t1 t2
  else Left $ TypeMismatch "Equal expects equal types"

{-
Γ|- e1 : t    Γ|- e2 : t
------------------------
    Γ|- e1 == e2 : Bool
-}

typeCheck (IFE conde e1 e2) = do
  tcond <- typeCheck conde
  case tyOf tcond of
    TBool -> do
      t1 <- typeCheck e1
      t2 <- typeCheck e2

      if sameTy t1 t2
      then Right $ TIFE (tyOf t1) t1 t2
      else Left $ TypeMismatch "IFE branch type mismatch"
    _ -> Left $ TypeMismatch "IFE expects bool condition expression"

{-
Γ|- e1 : Bool  Γ|- e2 : t  Γ|- e3 : t
-------------------------------------
      Γ|- IFE e1 e2 e3 : t
-}

tpx = Add (Equal (ConstInt 5) (ConstInt 6)) (ConstBool False)
tpy = Equal (Add (ConstInt 1) (ConstInt 2)) (ConstInt 3)
tpz = Add (IFE (Equal (ConstInt 1) (ConstInt 2)) (ConstInt 10) (ConstInt 20)) (ConstInt 5)
tpw = IFE (ConstBool False) (ConstInt 5) (ConstBool True)
