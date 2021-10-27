The code for Conal Elliott's ["Compiling to Categories"](http://conal.net/papers/compiling-to-categories/compiling-to-categories.pdf) can be found on [his github](https://github.com/conal/concat)

This is a reading through ConCat's source (as of commit [a185ee5](https://github.com/conal/concat/tree/a185ee5bf2f950a39d6d985aef281ad2607e955f)), that should hopefully compile, presented in the order of the paper. Primarily to see how the sausage is made for such a library and peek at the GHC plugin.

NOTE: Language Extensions will be declared in order of addition (to get things compiled) with notes on what is using them.

\begin{code}
--PART1
{-# LANGUAGE RankNTypes #-}        -- explicit-forall in Category (.)
{-# LANGUAGE PolyKinds #-}         -- kind variable u in con constraint
{-# LANGUAGE TypeOperators #-}     -- `k` TypeOperator in Category
{-# LANGUAGE FlexibleInstances #-} -- Yes1 instance
{-# LANGUAGE ConstraintKinds #-}   -- con ConstraintKind
{-# LANGUAGE TypeFamilies #-}      -- Indexed type families for Ok k in Category class declaration

import Prelude hiding (id, (.))    -- Category defines these so we need to avoid import clashing
import qualified Prelude as P

import GHC.Types (Type,Constraint) -- con and C3

--src/Concat/Misc.hs:145
class    Yes1 a
instance Yes1 a

--Yes0 is another class with no parameters and Yes2 is another class with 2 parameters

--src/Concat/Category.hs:372
type C3 (con :: u -> Constraint) a b c = (con a, con b, con c)

--src/ConCat/Category.hs:301
type Ok3 k a b c = C3 (Ok k) a b c --Auxilary type alias to a data constructor that counts the typeparams

--Associated Type
class Category k where
    type Ok k :: Type -> Constraint
    type Ok k = Yes1
    id :: Ok  k a => a `k` a
    infixr 9 .
    (.) :: forall b c a. Ok3 k a b c => (b `k` c) -> (a `k` b) -> (a `k` c)

instance Category (->) where
    id = P.id
    (.) = (P..)
\end{code}
