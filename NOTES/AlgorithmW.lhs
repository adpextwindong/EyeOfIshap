- [Algorithm W Step by Step Notes](https://web.archive.org/web/20210526061654/http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.65.7733&rep=rep1&type=pdf)

-- Introduction

Type inference algorithm W proposed by [Milner in 1978](https://homepages.inf.ed.ac.uk/wadler/papers/papers-we-love/milner-type-polymorphism.pdf) (of the Hindley-Milner polymorphic type inference).

[Principal type-schemes for functional programs - Luis Damas and Robin Milner POPL'82](https://web.cs.wpi.edu/~cs4536/c12/milner-damas_principal_types.pdf)

Another resource to look at is [Phil Freeman's gist](https://gist.github.com/paf31/a49a54d7ea5ede43422f).

[Top Quality Type Error Messages - Bastiaan Johannes Heeren](https://web.archive.org/web/20210507003349/http://www.open.ou.nl/bhr/TopQuality.pdf)

-- Algorithm W
\begin{code}
module AlgorithmW ( Expr (..),
                    Type (..),
                    ti          -- ti :: TypeEnv -> Expr -> (Subst, Type)
                                -- Type Inference
                  ) where
\end{code}

The literature calls environments "contexts".

We'll be using Data.Map for representing environments and substitutions.

Data.Set for sets of type variables etc.

One thing to note is that Martin here uses "type schemes"


-- Background

--- Milner

"...the polymorphism in Exp is the natural outgrowth of a single primitive polymorphic operator, function application, together with variable binding."

```
Type :
T ::= α           -- Type Variable
   |  T           -- Type Constant
   |  τ1 τ2       -- Type Application
```

From [Heeren](https://www.open.ou.nl/bhr/TopQuality.pdf#page=17) "Polymorphic types or type schemes (as opposed to the types above, which are monomorphic). In a type scheme, some type variables are universally quantified.

```
Type scheme:
σ ::= ∀a.σ       -- Polymorphic Type Scheme
    | τ          -- Monomorphic Type
```

//He also says read TAPL

From [Top](https://hackage.haskell.org/package/Top-1.7/docs/Top-Types-Schemes.html) "A type scheme consists of a list of quantified type variables, a finite map that partially maps these type variables to their original identifier, and a qualified type."


\begin{code}

import qualified Data.Map as Map
import qualified Data.Set as Set

--Control.Monad.Error is deprecated, use Control.Monad.Except instead
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State

import qualified Text.PrettyPrint as PP

--Phil Freeman uses De Bruijn indices, Martin uses Strings. For now this will be strings.
type Binder = String

data Expr = EVar Binder
          | ELit Lit
          | EApp Expr Expr
          | EAbs Binder Expr
          | ELet Binder Expr Expr
          deriving (Eq, Ord)

data Lit = LInt Integer
         | LBool Bool
         deriving (Eq, Ord)

data Type = TVar Binder       --Type Variable
          | TInt
          | TBool
          | Type :~> Type    -- a -> b
          deriving (Eq, Ord)

--Freeman directly includes this type in Type. Martin seperates it
data Scheme = Scheme [Binder] Type

\end{code}

We will need to determine the free type variables of a type. We will also need to apply substitutions on types and type schemes, so we can just make a typeclass _Types_ for this. It will also be used for type environments later on.

\begin{code}

--Substitutions are finite mappings from type variables to types.
type Subst = Map.Map Binder Type

class Types a where
    ftv :: a -> Set.Set Binder --Free Type Variables
    apply :: Subst -> a -> a   --Apply Substitution

instance Types Type where
    ftv (TVar name) = Set.singleton name
    ftv TInt     = Set.empty
    ftv TBool    = Set.empty
    ftv (t1 :~> t2) = Set.union (ftv t1) (ftv t2)

    apply subMap (TVar name) = case Map.lookup name subMap of
                                    Nothing -> TVar name    --Remains a type variable, no subsitution found
                                    Just t -> t             --Substituion found

    apply subMap (t1 :~> t2) = (apply subMap t1) :~> (apply subMap t2) --Recurse for type variables
    --Prim types aren't applied to
    apply _ t = t

instance Types Scheme where
    ftv (Scheme vars t) = Set.difference (ftv t) (Set.fromList vars)

    --Substitute any non quantified types
    apply subMap (Scheme vars t) = Scheme vars (apply (foldr Map.delete subMap vars) t)


ti = undefined

\end{code}
