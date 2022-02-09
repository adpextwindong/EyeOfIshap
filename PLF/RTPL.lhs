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
data Var = V Char
    deriving (Eq, Ord)

data IntExp = ILit Int
            | IVar Var
            | UnaryMinus IntExp
            | Plus IntExp IntExp
            | BinaryMinus IntExp IntExp
            | Mul IntExp IntExp
            | Div IntExp IntExp
            | Rem IntExp IntExp

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
            | VForAll Var Assert
            | VExists Var Assert

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

{-
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
            | VForAll Var Assert
            | VExists Var Assert
-}

type State a = M.Map Char a

denoIntExp :: IntExp -> State Int -> Int
denoIntExp (ILit i) env             = i
denoIntExp (UnaryMinus e) env       = negate $ denoIntExp e env
denoIntExp (IVar (V k)) env         = env M.! k
denoIntExp (Plus e0 e1) env         = (denoIntExp e0 env) + (denoIntExp e1 env)
denoIntExp (BinaryMinus e0 e1) env  = (denoIntExp e0 env) - (denoIntExp e1 env)
denoIntExp (Mul e0 e1) env          = (denoIntExp e0 env) * (denoIntExp e1 env)
denoIntExp (Div e0 e1) env          = (denoIntExp e0 env) `div` (denoIntExp e1 env)
denoIntExp (Rem e0 e1) env          = (denoIntExp e0 env) `rem` (denoIntExp e1 env)

--TODO deno assert page 8
denoAssert :: Assert -> State Int -> Bool
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

\end{document}
