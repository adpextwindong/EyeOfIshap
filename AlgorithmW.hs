{-
 - Damas 82 - Principal type-schemes for functional programs
 - Grabmuller 06 - Algorithm W Step by Step
 - https://cseweb.ucsd.edu/classes/wi14/cse230-a/lectures/lec-inference.html
 - TODO https://okmij.org/ftp/ML/generalization.html
 -}

import qualified Data.Map as M
import qualified Data.Set as S

type Identifier = String
type TyName = String

data Expr
  = Var Identifier
  | App Expr Expr
  | Lam Identifier Expr
  | LetE Identifier Expr Expr -- let x = e in e'

data Ty
  = TyVar TyName
  | TyPrim -- This is just a placeholder for prim types which we would add in more later
  | Ty :-> Ty
  -- TyPrimInt
  -- TyPrimBool

-- Damas82 expresses it as this: σ ::= τ | ∀ασ
data Scheme = Scheme [TyName] Ty

--Substitutions from type vars to types
type Subst = M.Map TyName Scheme

--Type Environments
newtype TypeEnv = TypeEnv (M.Map TyName Scheme)


--TODO
--MGU, substitution combinators, name supply and type inference monad
