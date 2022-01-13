\documentclass{article}
\usepackage[utf8]{inputenc}
\usepackage{hyperref}

\title{Reynolds TPL}
\begin{document}
\maketitle
\href{https://github.com/adpextwindong/EyeOfIshap/blob/main/PLF/Reynolds_TPL.pdf}{Theories of Programming Languages - John C. Reynolds}

\href{http://www.cs.yale.edu/homes/hudak/CS430F07/}{Hudak's CS430 coursework}

\section{\href{https://github.com/adpextwindong/EyeOfIshap/blob/main/PLF/Reynolds\_TPL.pdf\#page=14}{Predicate Logic}}

\href{http://www.cs.yale.edu/homes/hudak/CS430F07/LectureSlides/reynolds-ch1.pdf}{Lecture Notes}

\href{http://www.cs.yale.edu/homes/hudak/CS430F07/problem_set_1.htm}{Problem Set 1}

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
data Var

data IntExp = ILit Int
            | VMinus Var
            | Minus IntExp IntExp
            | Plus IntExp IntExp
            | Mul IntExp IntExp
            | Div IntExp IntExp
            | Rem IntExp IntExp

data Assert = BLit Bool
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

Referring to our Haskell datatypes here, the constructors (excluding BLit and ILit) correspond to figure 1.1.

For example:

$c_{\forall},c_{\exists} \in <var> \times <assert> \rightarrow <assert>.$

\begin{verbatim}
VForAll :: Var -> Assert -> Assert
VExists :: Var -> Assert -> Assert
\end{verbatim}

\end{document}
