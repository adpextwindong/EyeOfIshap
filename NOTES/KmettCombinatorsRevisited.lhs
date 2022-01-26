\begin{code}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

import Control.Monad (guard, ap, join)

data LC a
  = Var a
  | App (LC a) (LC a)
  | Lam (LC (Maybe a)) --Binding an extra variable which is what the maybe is for.
  deriving (Functor, Foldable, Traversable)

instance Applicative LC where
  pure = Var
  (<*>) = ap

--Capture avoiding substitution
--Avoids the bound variables
--See Kmett's bound library for more advanced version
instance Monad LC where
  Var a >>= f = f a
  App l r >>= f = App (l >>= f) (r >>= f)
  Lam k >>= f = Lam $ k >>= \case
    Nothing -> pure Nothing
    Just a -> Just <$> f a

lam :: Eq a => a -> LC a -> LC a
lam v b = Lam $ bind v <$> b where
  bind v u = u <$ guard (u /= v)
\end{code}

History

Schonfinkel 1924
Haskell Curry 1930
Turner 1979
Smullyan "To Mock a Mockingbird" 1985
Sabine Broda and Luis Damas "Bracket abstraction in the combinator system CI(K) 1987

\begin{code}
--Combinatory Logic
data CL a
  = V a             --Variables
  | A (CL a) (CL a) --Applications
  | S
  | K
  | I
  | B
  | C
  deriving (Functor, Foldable, Traversable)

instance Applicative CL where
  pure = V;
  (<*>) = ap

instance Monad CL where
  V a >>= f = f a
  A l r >>= f = A (l >>= f) (r >>= f)
  S >>= _ = S
  K >>= _ = K
  I >>= _ = I
  B >>= _ = B
  C >>= _ = C

\end{code}

https://youtu.be/zhj_tUMwTe0?t=306

S x y z = (x y) (y z) -- (<*>)
K x y - x -- const
I x = x
B x y z = x (y z) -- (.)
C x y z = x z y -- flip
Y f = f (Y f) = let x = f x in x -- fix

Abstraction Elimination a la Schonfinkel

\begin{code}
infixl 1 %
(%) = A

compile :: LC a -> CL a
compile (Var a)   = V a
compile (App l r) = compile l % compile r
compile (Lam b)   = compileLam b

compileLam :: LC (Maybe a) -> CL a
compileLam (Var Nothing) = I
compileLam (Var (Just y)) = K % V y

compileLam (App l r) = case (sequence l, sequence r) of
  (Nothing, Nothing) -> S % compileLam l % compileLam r
  (Nothing, Just r') -> C % compileLam l % compile r'
  (Just l', Nothing) -> B % compile l' % compileLam r
  (Just l', Just r') -> K % compile l' % compile r'

compileLam (Lam b) = join $ compileLam $ dist $ compileLam b where
  dist :: CL (Maybe a) -> LC (Maybe (CL a))
  dist (A l r) = App (dist l) (dist r)
  dist xs = Var (sequence xs)

\end{code}

C B are slight optimizations for writing in terms of SK

I isn't necessary either. Same with Y.

This can blow up a term to $O(n^3)$

Turner's work gets it down to $O(n^2)$

\subsection{Supercombinators}

Hughes Supercombinators, build a set of combinators specific to your program instead of the fixed set SKICBY

ex: Append combinator, if you're doing append a lot

~40% speedup at the time.

Lennart LazyML, bytecode that implemented supercombinators.

Spineless tagless G-machine is an evolution of Lennart's work.

Marlow, Yakushev, SPJ "Faster laziness using dynamic pointer tagging" 2007. LSB of a pointer is used to indicate which constructor it is. Instead of randomly jumping into a thunk to do evaluation.

\subsection{Unknown Thunk Evaluation}

\begin{align*}
jmp %eax
\end{align*}

Eventually returns which it constructor it evaluated out to.

Vulnerable to Spectre V2. Poison branch prediction table and leverage speculation buffer and side-effect observation.

SPMD-on-SIMD Evaluation.

\subsection{Why are combinator terms bigger?}

S x y z = (x z)(y z) -- Application with environment
K x y = x -- Ignores environment
I x -- Uses environment

All work one variable at a time so we need to use K a lot to throw away environments.
B and C, are similar but with branching.

\subsection{Sketching an "improved" abstraction elimination algorithm}

We need to be able to build and manipulate "partial" environments in sublinear time.

- Abadi, Cardelli, Curien, Levy "Explicit Substitutions" 1990
- Lescanne, "From λσ to λv a journey through calculi of explicit substitutions" 1994
- Mazzoli, "A Well-Typed Suspension Calculus" 2017

Locally Nameless Representations of the lambda calculus
De bruijn indices

are particlularly slow, you pay for the size of the term.

See bound library.

\subsection{Evaluation by Collection}

fst (a,b) ~> a
snd (a,b) ~> b

John Hughes in his PHD dissertation noted that it was impossible for any evaluator that did some concurrent evaluation to avoid certain classes of spaceleaks in a lazy language.

- John Hughes, "The design and implementation of programming languages" 1983
- Philip Wadler, "Fixing some space leaks with a garbage collector" 1987
- Christina von Dorrien, "Stingy Evaluation" 1989 (Let the garbage collector shrink the heap)

Pointer to fst (a,b) shouldn't leak b.

GHCJS does this correctly.

Some extra stingy combinator evaluation rules:

Looking for things that would shrink the heap

\begin{align*}
A (A K x) _           -> Just x -- def K
A I x                 -> Just x -- def I
A S K                 -> Just K_ -- Hudak 1
A S K_                -> Just I  -- Hudak 2
A S k@(A K (A K _))   -> Just k -- Hudak 3
A (A S (A K x)) I     -> Just x -- Hudak 4, Turner p35 2
A Y k@(A K _)         -> Just k -- Hudak 9

(Be careful, not all of Hudak's rules are stingy!)

- Paul Hudak and David Kranz, "A Combinator-based Compiler for a Functional Language" 1984

\subsection{One Bit Reference Counting}

SKIM2
Each node is an app cell, we reuse the 3 app cells on the left to rebalance it on the right.

         =>
   /\   (<*>)   /\
  /\ z         /\ /\
 /\ y         x z y z
S  x

z is then copied and a bit in the pointer has to be flaged to denote that it has two references to it.

- W.R. Stoye. T.J.W. Clarke and A.C. Norman "Some Practical Methods for Rapid Combinator Reduction" 1984

If I know I have the only reference to something I can mutate it inplace rather than grab another app cell off the heap.

This allows for extra reductions that require "uniqueness" to be stingy:

A (A (A C f) x) y         -> Just $ A (A f y) x
A (A (A B f) g) x         -> Just $ A f (A g x)
A (A S (A K x)) (A K y)   -> Just $ A K (A x y) -- Hudak 5, Turner p35 1
A (A S (A K x)) y         -> Just $ A (A B x) y -- Turner p35 3
A (A S x) (A K y)         -> Just $ A (A C x) y -- Turner p35 4

Can we reduce fst(x,y) ~> x by stingy evaluation _without_ special casing Wadler's rules?

fst p = p(\xy.x)            fst = CIK
pair x y z = z x y          snd = CI(KI)
snd p = p (\xy.y)           pair = BC(CI)

Turners rules require more combinators for them to fire.

\subsection{Optionally Acyclic Heaps}

S B C K I do not introduce new cycles on the heap. Just Y.

If you start with a directed acyclic graph for a heap you end with a directed acyclic graph for a heap.

Erlang's GC uses this directed acyclic graph invariant to do things in a single pass.

IORef's clash against this but otherwise we could hash cons the entire heap.

Hash Consing

Binary Decision Diagrams

- Eiichi Goto "Monocopy and associative algorithms in extended Lisp" 1974
- Eelco Dolstra, "Maximal Laziness" 2008

"Compilers for dependently typed languages are slow. Hash consing is usually bolted in as an afterthought. I'd like an efficient default evaluator that automatically memoizes and addresses unification and substitution concerns."

Dependently typed languages have a series of competing concerns:
- Substitutions on terms
- Unification on terms
- Evaluation
- Hash cons terms away so common terms in the heap (of intermediate representations) get commoned up

- Daniel Dougherty "Higher-order unification via combinators" 1993
