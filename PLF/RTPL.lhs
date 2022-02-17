\documentclass{article}
\usepackage[utf8]{inputenc}
\usepackage{hyperref}
\usepackage{stmaryrd}

\title{Reynolds TPL}
\begin{document}
\begin{verbatim}
\begin{code}

{-# LANGUAGE TupleSections #-}

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Monad.Reader
import Control.Concurrent.Supply (Supply, newSupply, freshId, splitSupply)
import Data.Bifunctor
import Data.Char

\end{code}
\end{verbatim}
\maketitle
\href{https://github.com/adpextwindong/EyeOfIshap/blob/main/PLF/Reynolds_TPL.pdf}{Theories of Programming Languages - John C. Reynolds}

\href{http://www.cs.yale.edu/homes/hudak/CS430F07/}{Hudak's CS430 coursework}

\section{\href{https://github.com/adpextwindong/EyeOfIshap/blob/main/PLF/Reynolds\_TPL.pdf\#page=14}{Predicate Logic}}

\href{http://www.cs.yale.edu/homes/hudak/CS430F07/LectureSlides/reynolds-ch1.pdf}{Lecture Notes}

\href{http://www.cs.yale.edu/homes/hudak/CS430F07/problem_set_1.htm}{Problem Set 1}

\subsection{Abstract Syntax}

As predicate logic has no concept of nontermination, its denotations can be defined in terms of ordinary sets (postponing the topic of domains and its subtleties until CH3).

Terminology:
\begin{verbatim}
Logician Terms :: Programmer Terms

Terms -> Integer Expressions (intexp)
well-formed formulas (wff) -> Assertions (Assert)
Assignments -> States
\end{verbatim}

\href{https://github.com/adpextwindong/EyeOfIshap/blob/main/PLF/Reynolds\_TPL.pdf\#page=15}{An abstract grammar for predicate logic, for example, would be}
\begin{verbatim}


\begin{code}

-- A predefined nonterminal denoting a
-- countably infinite set of variables (with unspecified representations)
data Var k = V k
    deriving (Eq, Ord, Show)

type State key a = M.Map key a

type Name = Int --We will be using Control.Concurrent.Supply

data IntExp = ILit Int
            | IVar (Var Name)
            | UnaryMinus IntExp
            | Plus IntExp IntExp
            | BinaryMinus IntExp IntExp
            | Mul IntExp IntExp
            | Div IntExp IntExp
            | Rem IntExp IntExp
            deriving Show

data Assert = ATrue
            | AFalse
            | EQ IntExp IntExp
            | NEQ IntExp IntExp
            | LT IntExp IntExp
            | LTE IntExp IntExp
            | GT IntExp IntExp
            | GTE IntExp IntExp
            | Not Assert
            | And Assert Assert
            | Or Assert Assert
            | Implies Assert Assert
            | IFF Assert Assert
            | VForAll (Var Name) Assert
            | VExists (Var Name) Assert
            deriving Show

\end{code}
\end{verbatim}

For each nonterminal of the grammar the must be a set of abstract phrases, called a carrier.

\subsection{Carriers and Constructors}

First. In the case of predicate logic, there will be three carriers named by the nonterminals $<var>$ $<intexp>$ $<assert>$.

Second. For each production of the abstract grammar, there must be a function among the carriers called a constructor. Specifically, a production of the form $L ::= s_0R_0 ... R_{n-1}s_n$ gives rise to a constructor $c \in R_0 \times ... \times R_{n-1}\rightarrow L$.

Referring to our Haskell datatypes here, the constructors (excluding BLit and ILit, see page 7 for the SML version) correspond to figure 1.1.

For example:

$c_{\forall},c_{\exists} \in <var> \times <assert> \rightarrow <assert>.$

\begin{verbatim}
VForAll :: Var -> Assert -> Assert
VExists :: Var -> Assert -> Assert
\end{verbatim}

Carriers and constructors must satisfy the following conditions:

Each constructor must be injective. (\href{http://www.cs.yale.edu/homes/hudak/CS430F07/LectureSlides/MathBackground.pdf#page=14}{At most one})

Any two constructors into the same carrier (both above for example) must have disjoint ranges.

Every member of each carrier that is not predefined (for example, $<intexp>$ and $<assert>$) must be constructible using a finite number of applications of the constructors.

NOTE: Reynolds says this insures that the abstract phrases form a many-sorted initial operator whose operators are the constructors. For those familiar with universal algebra. CATEGORY THEORY

Least Fixed Point Theorem will appear in Section 2.4

\subsection{Denotational Semantics of Predicate Logic}

We must define a "model" of predicate logic, comprised by a pair of semantic functions that map integer expressions and assertions into the meanings that these phrases denote.

The denotations of a phrase are much more complex than values, because the value of a phrase depends on the values of its variables. This depends on a state, which is a function that maps each variable into its integer value.

Given the Ints {Z} and Bools {B}, we can write $\Sigma$ for the set $<var> \rightarrow Z$ of states, the semantic functions that map integer expressions and assertions into their meanings have the following types:\newline
\newline
$\llbracket - \rrbracket_{intexp} \in <intexp> \rightarrow \Sigma \rightarrow Z$
\newline
\newline
$\llbracket - \rrbracket_{assert} \in <assert> \rightarrow \Sigma \rightarrow Z$

\begin{verbatim}
\begin{code}

denoIntExp :: IntExp -> State Name Int -> Int
denoIntExp (ILit i) env             = i
denoIntExp (UnaryMinus e) env       = negate $ denoIntExp e env
denoIntExp (IVar (V k)) env         = env M.! k
denoIntExp (Plus e0 e1) env         = (denoIntExp e0 env) + (denoIntExp e1 env)
denoIntExp (BinaryMinus e0 e1) env  = (denoIntExp e0 env) - (denoIntExp e1 env)
denoIntExp (Mul e0 e1) env          = (denoIntExp e0 env) * (denoIntExp e1 env)
denoIntExp (Div e0 e1) env          = (denoIntExp e0 env) `div` (denoIntExp e1 env)
denoIntExp (Rem e0 e1) env          = (denoIntExp e0 env) `rem` (denoIntExp e1 env)

--TODO deno assert page 8
denoAssert :: Assert -> State Name Int -> Bool
denoAssert (ATrue) _ = True
denoAssert (AFalse) _ = False
denoAssert (Main.EQ e0 e1) env  = (denoIntExp e0 env) == (denoIntExp e1 env)
denoAssert (NEQ e0 e1) env      = (denoIntExp e0 env) /= (denoIntExp e1 env)
denoAssert (Main.LT e0 e1) env  = (denoIntExp e0 env) < (denoIntExp e1 env)
denoAssert (LTE e0 e1) env      = (denoIntExp e0 env) <= (denoIntExp e1 env)
denoAssert (Main.GT e0 e1) env  = (denoIntExp e0 env) > (denoIntExp e1 env)
denoAssert (GTE e0 e1) env      = (denoIntExp e0 env) >= (denoIntExp e1 env)
denoAssert (Not e) env          = not (denoAssert e env)
denoAssert (And e1 e2) env      = denoAssert e1 env && denoAssert e2 env
denoAssert (Or e1 e2) env       = denoAssert e1 env || denoAssert e2 env

denoAssert (Implies e1 e2) env | denoAssert e1 env = denoAssert e2 env
                               | otherwise = True

denoAssert (IFF e1 e2) env = denoAssert (Implies e1 e2) env && denoAssert (Implies e2 e1) env

--Hudak does a "diagonalization" algorithmn.
--We can leverage AND short circuiting on a False. Otherwise it doesnt terminate
denoAssert (VForAll (V k) e) env = and (map f (interleave [0,1..] [-1,-2..]))
    where f i = denoAssert e (M.insert k i env)

--Same idea but with OR
denoAssert (VExists (V k) e) env = or (map f (interleave [0,1..] [-1,-2..]))
    where f i = denoAssert e (M.insert k i env)

interleave :: [a] -> [a] -> [a]
interleave (x:xs) (y:ys) = x : y : interleave xs ys

\end{code}
\end{verbatim}

\subsection{1.3 Validity and Inference}

Terminology:

Valid : When $\llbracket p \rrbracket _assert \sigma = true$ for all states $\sigma in \Sigma$, we say that p is valid.

Unsatisfiable: When $\llbracket p \rrbracket _assert \sigma = false$ for all states $\sigma in \Sigma$, we say that p is unsatisfiable.

Stronger/Weaker: When $p_0 \rightarrow p_1$ is valid or, equivalently, when every state satisfying $\p_0$ also satisfies $p_1$, we say that $p_0$ is stronger than $p_1$ and that $p_1$ is weaker than $p_0$.

Sound: If there is a proof of an assertion p, then p should be valid. (For all assignments its true) This wil occur provided that each inference rule is sound, which means that, for every instance of the rule, if the premisses are all valid, the conclusion is valid.

\begin{align*}

Forall instances of each inference rule

Premise Valid
-------------
Conclusion Valid

\end{align*}

Complete: A set of inference rules is said to be complete if it can be used to prove every valid assertion. (If validity is defined as in this chapter, then no finite set of inference rules is complete. Godels imcpleteness theorem. But if validity is defined to be logical validity, then there are finite sets of inference rules that are known to be complete.)

Logically valid: In the theoretical study of logic, considerable attention is paid to the situation where the syntax and semantics of the operations for constructing assertions are fixed, but the semantics of the operations for constructing expressions is varied over arbitrary functions on an arbitrary set (so that one would no longer speak of integer expressions). When an assertion holds for all such variations (as well as for states), it is said to be logically valid.

\subsection{1.4 Binding and Substitution}

Binding's mistreament is a surprisingly subtle source of language design errors.

Closed: A phrase with no free occurrences of variables is said to be closed.

Example:
forall x. (x /= y \/ forall y. (x = y \/ forall x. x + y /= x))
       1   1   FREE         2   1   2           3  3   2    3

Premise
------
Conclusion


Caution: Each step in a proof must be a valid assertion, not just an assertion that is true in particular states.

\begin{verbatim}
\begin{code}

freeVarsInt :: IntExp -> S.Set (Var Name)
freeVarsInt (ILit _)            = S.empty
freeVarsInt (IVar v)            = S.singleton v
freeVarsInt (UnaryMinus e)      = freeVarsInt e
freeVarsInt (Plus e1 e2)        = S.union (freeVarsInt e1) (freeVarsInt e2)
freeVarsInt (BinaryMinus e1 e2) = S.union (freeVarsInt e1) (freeVarsInt e2)
freeVarsInt (Mul e1 e2)         = S.union (freeVarsInt e1) (freeVarsInt e2)
freeVarsInt (Div e1 e2)         = S.union (freeVarsInt e1) (freeVarsInt e2)
freeVarsInt (Rem e1 e2)         = S.union (freeVarsInt e1) (freeVarsInt e2)

freeVarsAssert :: Assert -> State Name Int -> S.Set (Var Name)

freeVarsAssert (ATrue) _           = S.empty
freeVarsAssert (AFalse) _          = S.empty
freeVarsAssert (Main.EQ e1 e2) env = S.union (freeVarsInt e1) (freeVarsInt e2)
freeVarsAssert (NEQ e1 e2) env     = S.union (freeVarsInt e1) (freeVarsInt e2)
freeVarsAssert (Main.LT e1 e2) env = S.union (freeVarsInt e1) (freeVarsInt e2)
freeVarsAssert (LTE e1 e2) env     = S.union (freeVarsInt e1) (freeVarsInt e2)
freeVarsAssert (Main.GT e1 e2) env = S.union (freeVarsInt e1) (freeVarsInt e2)
freeVarsAssert (GTE e1 e2) env     = S.union (freeVarsInt e1) (freeVarsInt e2)
freeVarsAssert (Not e) env         = freeVarsAssert e env
freeVarsAssert (And e1 e2) env     = S.union (freeVarsAssert e1 env) (freeVarsAssert e2 env)
freeVarsAssert (Or e1 e2) env      = S.union (freeVarsAssert e1 env) (freeVarsAssert e2 env)
freeVarsAssert (Implies e1 e2) env = S.union (freeVarsAssert e1 env) (freeVarsAssert e2 env)
freeVarsAssert (IFF e1 e2) env     = S.union (freeVarsAssert e1 env) (freeVarsAssert e2 env)
freeVarsAssert (VForAll v e) env   = S.difference (freeVarsAssert e env) (S.singleton v)
freeVarsAssert (VExists v e) env   = S.difference (freeVarsAssert e env) (S.singleton v)

\end{code}
\end{verbatim}

\subsection{Capture Avoiding Substitution}

Given the sound axiom schema

---------------------------- (1.13)
(forall v. p) => (p/v -> e)

replace p by exits y. y > x
replace v by x
replace e by y + 1

(forall x. exist y. y > x) => ((exists y. y > x) / x -> y + 1).

Naively carrying this substitution out (without noticing e has a y in it!!) gives us

(forall x. exists y. y > x) => (exists y. y > y + 1)

The left side is true but the right side is false due to the collision of bindings messing things up. Y is already captured by the exists binder and is a free occurance in $y + 1$

To avoid this, we must define substitution so that bound varaibles are renamed before carrying out the replacement whenever such renaming is necessary to avoid capture.

Instead of defining substitution for a single variable, it is simpler to define simulatenous substituion for all variables.


\begin{verbatim}
\begin{code}

data SubMap = SubMap {
                subs :: M.Map (Var Name) IntExp --SubstituionMap from the set of vars to intexp
               ,nameSupply :: Supply
              }

splitSubMapSupply (SubMap subs ns) = let (l,r) = splitSupply ns in ((SubMap subs l),(SubMap subs r))

simulSubstInt :: IntExp -> SubMap -> IntExp
simulSubstInt e@(ILit _) submap           = e
simulSubstInt (IVar v) submap             = case M.lookup v (subs submap) of
                                              Just e -> e
                                              Nothing -> IVar v
simulSubstInt (UnaryMinus e) submap       = UnaryMinus (simulSubstInt e submap)
simulSubstInt (Plus e1 e2) submap         = Plus (simulSubstInt e1 submap) (simulSubstInt e2 submap)
simulSubstInt (BinaryMinus e1 e2) submap  = BinaryMinus (simulSubstInt e1 submap) (simulSubstInt e2 submap)
simulSubstInt (Mul e1 e2) submap          = Mul (simulSubstInt e1 submap) (simulSubstInt e2 submap)
simulSubstInt (Div e1 e2) submap          = Div (simulSubstInt e1 submap) (simulSubstInt e2 submap)
simulSubstInt (Rem e1 e2) submap          = Rem (simulSubstInt e1 submap) (simulSubstInt e2 submap)

simulSubstAssert :: Assert -> SubMap -> Assert
simulSubstAssert (ATrue) submap           = ATrue
simulSubstAssert (AFalse) submap          = AFalse
simulSubstAssert (Main.EQ e1 e2) submap   = Main.EQ (simulSubstInt e1 submap) (simulSubstInt e2 submap)
simulSubstAssert (NEQ e1 e2) submap       = NEQ (simulSubstInt e1 submap) (simulSubstInt e2 submap)
simulSubstAssert (Main.LT e1 e2) submap   = Main.LT (simulSubstInt e1 submap) (simulSubstInt e2 submap)
simulSubstAssert (LTE e1 e2) submap       = LTE (simulSubstInt e1 submap) (simulSubstInt e2 submap)
simulSubstAssert (Main.GT e1 e2) submap   = Main.GT (simulSubstInt e1 submap) (simulSubstInt e2 submap)
simulSubstAssert (GTE e1 e2) submap       = GTE (simulSubstInt e1 submap) (simulSubstInt e2 submap)
simulSubstAssert (Not e) submap           = Not (simulSubstAssert e submap)
simulSubstAssert (And e1 e2) submap       = And (simulSubstAssert e1 submap) (simulSubstAssert e2 submap)
simulSubstAssert (Or e1 e2) submap        = Or (simulSubstAssert e1 submap) (simulSubstAssert e2 submap)
simulSubstAssert (Implies e1 e2) submap   = Implies (simulSubstAssert e1 submap) (simulSubstAssert e2 submap)
simulSubstAssert (IFF e1 e2) submap       = IFF (simulSubstAssert e1 submap) (simulSubstAssert e2 submap)

simulSubstAssert (VForAll vold e) submap = VForAll vnew (simulSubstAssert e extendedSubMap)
  where (vnew, extendedSubMap) = extendSubMapForQualifier vold e submap

simulSubstAssert (VExists vold e) submap = VExists vnew (simulSubstAssert e extendedSubMap)
 where (vnew, extendedSubMap) = extendSubMapForQualifier vold e submap

extendSubMapForQualifier vold e (SubMap submap ns) = (vnew, extendedSubMap)
  where (vnew, nextSupply) = first V $ freshId ns
        withVnew       = M.insert vold (IVar vnew) submap
        extendedSubMap = SubMap withVnew nextSupply

--TODO test how this namesupply works and if we need to split it on e1 e2 recurses
--Example 1.13
x = V $ ord 'x'
y = V $ ord 'y'
z = V $ ord 'z'
t = V $ ord 't'
n = V $ ord 'n'
d = V $ ord 'd'

--original = VForAll x p
p = (VExists y (Main.GT (IVar y) (IVar x)))
e = Plus (IVar y) (ILit 1)

--Performing the (forall v.p) => (p/v -> e)
exampleMap ns = SubMap (M.fromList [(x,e)]) ns

testSubTerm :: Assert -> SubMap -> Assert
testSubTerm p = simulSubstAssert p

test :: Assert -> M.Map (Var Name) IntExp -> IO Assert
test p smap = do
  ns <- newSupply
  return $ testSubTerm p (SubMap smap ns)

ta = Implies (VForAll x (VForAll z (And (Main.LT (IVar x) (IVar t)) (LTE (IVar t) (IVar z))))) (VExists y (And (LTE (IVar x) (IVar y)) (Main.LT (IVar y) (IVar z))))
--ta = Implies (ATrue) (VExists y (And (LTE (IVar x) (IVar y)) (Main.LT (IVar y) (IVar z))))

ae = M.fromList [(t, (Plus (IVar x) (Plus (IVar y) (IVar z))))]

tb = VForAll d (Implies (VExists n (Main.EQ (IVar x) (Mul (IVar n) (IVar d)))) (VExists n (Main.EQ (IVar y) (Mul (IVar n) (IVar d)))))
be = M.fromList [(n,IVar x),(d,IVar y)]

tc = VForAll x (VExists y (Implies (Main.LT (IVar x) (IVar z)) (And (Main.LT (IVar x) (IVar y)) (Main.LT (IVar y) (IVar z)))))
ce = M.fromList [(y, IVar x),(z,IVar y),(x,IVar z)]
{-
--------------------------
(forall v.p) => (p/v -> e)

(forall x. exists y. y > x) => ((exists y. y > x) / x -> y + 1)

simulSubstAssert (VExists y (Main.GT (IVar y) (IVar x))) (SubMap (M.fromList [(x, (Plus (IVar y) (ILit 1)))]) ns)

= VExists (V 0) (GT (IVar (V 0)) (Plus (IVar (V 121)) (ILit 1)))
exists a. a > y + 1
-}

\end{code}
\end{verbatim}

\section{2 - The Simple Imperative Language}

This chapter introduces the notion of bottom $\perp$, nontermination.
Vocab:
DR SEM EQ - Direct Semantic Equation, where "direct" distinguishes the kind of semantics from the continuation semantics that will be introduced in Chapter 5.

Nontermination (bottom) complicates the denotational semantic formulation. Beginning with sequencing of statements, $[\gamma1;\gamma2]$ it is possible $\gamma1$ fails to terminate, therefore $\gamma1;\gamma2$ would fail to terminate. Therefore we extend functions to a domain including $\perp$ by mapping $\perp$ to $\perp$.

If $f$ is a function from $\Sigma$ to $\Sigma_\perp$, we write $f\perp\perp = if \sigma = \perp then \perp else f \sigma$.

Sequential composition's direct semantic equation then looks like

$\llbracket c_0 ; c_1 \rrbracket _comm \sigma = (\llbracket c_1\rrbracket) \perp\perp (\llbracket c_0 \rrbracket _comm \sigma ).$

A naive attempt at a While dr sem eq can lead to

$\llbracket while b do c \rrbracket_commm \sigma = \llbracket if b then (c; while b do c) else skip \rrbracket _comm \sigma$

This unfortunately isn't a semantic equation because it does not describe the meaning of while purely in terms of the meaning of its subprhases b and c.

A pathological example is

$\llbracket while true do skip \rrbracket _comm \sigma = \llbracket while true do skip \rrbracket _comm sigma$

This is satisfied by every function in $\Sigma \rightarrow \Sigma_\perp$ and is a command that never terminates.

Overcoming this will introduct rudiements of domain theory.

\subsection{Domains and Continous Functions}

Page 29 onwards contains a lot of definitions.
Page 463 contains information on the cup/cap notation

$\sqcup X for the least upper bound of X$
$\sqcap X for the greasted lower bound of X$

A chain is a countably infinite increasing sequence, $x_0 \sqsubseteq x_1 \sqsubseteq x_2 \sqsubseteq ...$

A partially ordered set P is called a predomain if every chain of elements of P has a limit in P. A predomain with a least element, which we will denote by $\perp$, is called a domain.

Continuous

A function $f$ from a predomain $P$ to a predomain $P'$ is said to be continuous from $P$ to $P'$ if it preserves the limits of chains, that is, if, for every chain $x_0 \sqsubseteq x_1 \sqsubseteq ...$ of the elements of $P$, the  function $f$ maps the limits of the chain on an lement of $P'$ that is the least upper bound of ${fx_0,fx_1,...}$.

A continuous function is monotone.

A monotone function $f$ will map a chain $x_0 \sqsubseteq x_1 \sqsubseteq ...$ into another chain $fx_0 \sqsubseteq fx_1 \sqsubseteq ...$, which must posses some limit.


Page 35 2.4 - The Least Fixed-Point Theorem

If $D$ is a domain and $f$ is continous function from $D$ to $D$, then

$x = \sqcup_{x=0}^\infty f^n \perp$

is the least fixed-point of f. In other words, $f x = x$ and, whenever $fy = y$, $x \sqsubseteq y$.





\end{document}
