\documentclass{article}
\usepackage[utf8]{inputenc}
\usepackage{hyperref}
\usepackage{alltt}
\usepackage{stmaryrd}
\usepackage{amsmath}

\title{Strachey74 Reading}
\author{George Takumi Crary}
\begin{document}
\maketitle

\begin{verbatim}
\begin{code}
import Data.Function (fix)
import Debug.Trace (trace)
import qualified Data.Map.Strict as M
import qualified Data.Map.Lazy as LM
import Data.Bifunctor
\end{code}
\end{verbatim}

\href{https://www.cs.tufts.edu/comp/150FP/archive/christopher-strachey/continuations.pdf}{Continuations: A Mathematical Semantics for Handling Full Jumps}

References in particular:

\href{https://www.cs.ox.ac.uk/files/3228/PRG06.pdf}{Scott, D. and Strachey, C. Toward a mathematical semantics for computer languages. In Proc. of the Symposium
on Computers and Automata, Polytechnic Institute of Brooklyn, 1971. Also as Technical Monograph PRG-6, Oxford University Computing Laboratory, Programming Research Group}

Further reading:

\href{https://www.cs.princeton.edu/~appel/papers/cpcps.pdf}{Continuation-Passing, Closure-Passing Style by Andrew W. Appel and Trevor Jim 1988}

\href{http://www.jimpryor.net/teaching/nasslli/wadler-essence.pdf}{The essence of functional programming by Philip Wadler POPL 92}

\href{https://kseo.github.io/posts/2017-01-09-continuation-passing-style-interpreter.html}{Kwang's Haskell Blog - Continuation Passing Style Interpreter}

\href{https://stackoverflow.com/questions/25365900/trying-to-apply-cps-to-an-interpreter}{Applying CPS to an interpreter StackOverflow}

\href{https://www.cis.upenn.edu/~bcpierce/courses/670Fall04/GreatWorksInPL.shtml}{Great Works in Programming Languages - Collected by Benjamin C. Pierce}

\section{Conventions}

$C$\newline
$\gamma$ Command\newline
$\epsilon$ Expression\newline
$\rho$ Environment\newline
$\sigma$ Store\newline
$\theta$ Store Transformation from $S \rightarrow S$\newline
$S$ Domain of Machine States (Stores)\newline
$\S$ and $\S|$ Statement brackets (equivalent to Algol 60’s begin and end)
$\delta$ The typical element of both E (Domain of Expression Results) and D (Domain of values which can be denoted by Identifiers)
$\kappa$ An individual expression continuation within K (Domain of expression continuations)

Typing of functions is done as such

\begin{align*}
F : [a -> b]
G : [b -> [c -> d]]
g \circ f : [a -> d]
\end{align*}

Braces will be used to delimit continuations for readability.

\section{The problem of jumps}

In the semantics given in [13] the value of a command is a function which transforms the store, so that, in symbolic terms\newline

$C\llbracket\gamma\rrbracket(\rho) = \theta$\newline

where $C$ is the semantic function mapping commands to their meaning, $\gamma$ is a command, $\rho$ the environment which gives the denotations associated with the identifiers in $\gamma$ and $\theta$ is a store transformation, i.e.\newline

$\theta \in C = [S \rightarrow S]$\newline

where $S$ is the domain of machine states (or stores). We use the double brackets $\llbracket \rrbracket$ as an aid to the eye to separate the program text $\gamma$ from the value domain expression which form the rest of the equation.

\subsection{Sequencing}

The normal sequence of commands is then naturally interpreted as performing one store transformation after another, so that the overall effect is that of functional composition.
Thus, if we have two commands $\gamma_0$ and $gamma_1$ with store transformations given by\newline

$\theta_0 = C\llbracket \gamma_0 \rrbracket (\rho)$

$\theta_1 = C\llbracket \gamma_1 \rrbracket (\rho)$\newline

The effect of the sequence of commands $\gamma_0 ; \gamma_1$ on any initial store $\sigma$ will be to produce a store\newline

$\sigma^{'} = \theta_1 ( \theta_2 (\sigma))$

so that the semantic equations take the form (compositionally)\newline

$C\llbracket \gamma_0 ;\gamma_1 \rrbracket (\rho) = (C\llbracket \gamma_1 \rrbracket(\rho))\circ (C\llbracket \gamma_0 \rrbracket(\rho))$\newline

This simple scheme breaks down if $\gamma_0$ contains a jump to some external label.

\subsection{valof-resultis construction}

Handling an error exit from a function (Algol 60: Type-procedure).

A jump may be called in the middle of evaluating an expression, while there may be several partial results.

We can avoid this unnecessary complication by using the \textbf{valof-resultis} construction.

In this we have an expression of the form \textbf{valof}$\gamma$ and a command of the form \textbf{resultis}$\epsilon$.

NOTE: $\gamma$ is a Command and $\epsilon$ is an expression.

\begin{verbatim}
data Expr = ...
          | Valof Command

data Command = ...
             | Resultis Expr
\end{verbatim}

The value of an expression

\begin{alltt}
\textbf{valof}\(\S\gamma_0\)
      \(\gamma_1\)
      ...
      \textbf{resultis} \(\epsilon'\)
      ...
\end{alltt}

is found by obeying the commands $\gamma_0$,$\gamma_1$,... in a sequence until a command \textbf{resultis} $\epsilon'$ is obeyed.

The expression $\epsilon'$ is then evaluated and this value is taken as the value of the whole expression. All programming languages which allow functions to be defined have some construction which is semantically equivalent to this \textbf{resultis} command, but many languages restrict its use so that it gets confused with function definition.

The difficult type of jump is inside the body of a \textbf{valof} block to a label which is outside. The following is a small PL to illustrate the semantics of these jumps.

\section{A small "continuation" language}

\subsection {Syntactic Categories}
$\xi \in Id$ Usual Identifiers\newline
$\gamma \in Cmd$ Commands\newline
$\epsilon \in Exp$ Expressions\newline
$\phi \in Fn$ Some Primitive Commands\newline

\vspace{1.5cm}

\begin{verbatim}
\begin{code}

type Identifier = String
type Label = Identifier

type Store = M.Map Identifier Expr

data Command = Prim
             | Dummy Int
             | Sequence Command Command
             | IFE Expr Command Command
             | While Expr Command
             | CommandBlock [(Label, Command)]
             | ResultIs Expr
             | VarDecl Identifier Expr
             | Incr Identifier
             | Print Identifier
             | Goto Label
             deriving Show

data Expr = ELabel Label
          | ETrue
          | EFalse
          | Cond Expr Expr Expr
          | ValOf Command
          | ELTE Expr Expr
          | Var Identifier
          | Const Int
            deriving Show

tx = Sequence (Dummy 0)
            $ Sequence (Dummy 1)
                  $ Sequence (Dummy 2) $ Dummy 3

ty = CommandBlock $ fmap (\i -> (show i, Dummy i)) [0..5]
\end{code}

\end{verbatim}

There is function, known as an environment which gives the mapping from identifiers to their denotations.

\begin{align*}
\rho \in Env = [Id \rightarrow D].
\end{align*}

\subsection{Continuations}

$\gamma$ as the store transformation $C\llbracket\gamma\rrbracket(\rho)$ crucially depends on the distinction between jump-free and jump-dependent commands.

This means we cannot find a satisfactory semantic equation for $C\llbracket\gamma_0;\gamma_1\rrbracket(\rho)$ because we cannot determine whether $\gamma_0$ is jump-free or not.

Instead we define a semantic function which yields, for every command $\gamma$, the state transformation from there to the end of the program.

This function will be $P$ instead of $C$.

In order to deal with the effect of the program following $\gamma$, we need to supply an extra argument, $\theta$, which is the state transformation corresponding to this part of the program. If $\gamma$ is jump-free, we shall then have for the program including $\gamma$

\begin{align*}
P\llbracket\gamma\rrbracket(\rho)(\theta) = \theta \circ C\llbracket\gamma\rrbracket(\rho)
\end{align*}

In other words the state transformation from $\gamma$ is performed first then the rest of the program, $\theta$.

$\theta$ is called a \emph{continuation} (strictly a \emph{command continuation}) and is of type $C = [\rightarrow]$

or

\begin{verbatim}
\begin{code}
type Cont = Store -> Store
\end{code}
\end{verbatim}

Thus the semantic function $P$ has the functionality.

\begin{align*}
P:[Cmd \rightarrow [Env \rightarrow [C \rightarrow [S \rightarrow S]]]]
\end{align*}

or

\begin{align*}
P:[Cmd \rightarrow [Env \rightarrow [C \rightarrow CONT]]]
\end{align*}

We will discuss $P\llbracket\gamma\rrbracket(\rho)(\theta)$ when $\gamma$ is jump-dependent later.

It is worth nothing that it does not need to depend on the argument $\theta$. It is possible to ignore the normal continuation for a command which is precisely what is needed for jumps.

Now lets take a look at a simple sequence $P\llbracket\gamma_0;\gamma_1\rrbracket\rho\theta$.

We carry out $\gamma_0$ (in the environment $\rho$).

Then follow it with the continuation which arises from carrying out $\gamma_1$ (in $\rho$) with the continuation $\theta$.

$\gamma_0$'s CONT = $P\llbracket\gamma_1\rrbracket\rho\theta$

$\gamma_1$'s CONT = $\theta$

\begin{align*}
P\llbracket\gamma_0;\gamma_1\rrbracket\rho\theta\sigma = P\llbracket\gamma_0\rrbracket\rho\{P\llbracket\gamma_1\rrbracket\rho\theta\}\sigma
\end{align*}

Now considering labels and jumps. The value of a label with be the state transformation from the labelled point to the end of the program (the rest of the program).

$P\llbracket\textbf{goto}\epsilon\rrbracket\rho\theta$ will simply ignore the original continuation $\theta$ and use the value of $\epsilon$ (the label) as the continuation.

\subsection{Expressions}

Expressions can now also yield both a (possibly altered) machine state and a result. The effect of the result however is not determiend by the expression itself but by its context.

This leads us to a new type of continuations, \emph{expression continuation}.

As arguments it takes the expression result, a state, and produces a final state.

$K = [E \rightarrow [S \rightarrow S]]$

$K = [E \rightarrow CONT]$

\begin{verbatim}
\begin{code}
type ECont = Expr -> Cont
\end{code}
\end{verbatim}

NOTE: $E$ is the domain of expression results. In this example it is the same as $D$ (the domain of values which can be denoted by identifiers) but this is a language detail (see ref [14]).

We shall use $\delta$ for the typical element of both $E$ and $D$ and $\kappa \in K$ for an individual expression continuation.

This leads to a semantic function for expressions denoted by $\mathcal{E}$

\begin{align*}
\mathcal{E} : [Exp \rightarrow [Env \rightarrow [K \rightarrow [S \rightarrow S]]]]
\end{align*}

For example

\begin{align*}
\mathcal{E}\llbracket tt \rrbracket\rho\kappa\sigma = \kappa(tt)\sigma\newline
\mathcal{E}\llbracket ff \rrbracket\rho\kappa\sigma = \kappa(ff)\sigma\newline
\end{align*}

For an identifier its almost equally simple

\begin{align*}
\mathcal{E}\llbracket\xi\rrbracket\rho\kappa\sigma = \kappa(\rho\llbracket\xi\rrbracket)\sigma
\end{align*}

The fact that the same store $\sigma$ occurs on both sides of these equations indicates that is has not been altered by the evalution of the exprression -- in other words that true, false and identifiers $\xi$ can be evaluated without side-effects. This is not true of expressions in general (and of \textbf{valof} expressions in particular). If the evaluation of $\epsilon$ in the environment $\rho$ and with a machine state $\sigma$ terminates normaly producing a result $\delta$ and an altered state $\sigma'$, we should get

\begin{align*}
\mathcal{E}\llbracket\epsilon\rrbracket\rho\kappa\sigma = \kappa(\delta)(\sigma')
\end{align*}

\begin{verbatim}
\begin{code}
data Env = Env {
             gotoTable :: LM.Map Label (Store -> IO Store)
           }

--Looks up an identifier
find :: Env -> Store -> Identifier -> Expr
find _ s i = s M.! i
--For now we're checking the store and ignoring env.
--TODO check how jlox handles the enviornment for closures

insert :: Identifier -> Expr -> Store -> Store
insert i e s = M.insert i e s

--Expression Continuation
type K = Expr -> Cont

kTrace :: K
kTrace = \e -> trace (show e) id

--Monadic Expression Continuation (for printing in this example)
--Lox can probably lose MonadState but hold onto MonadFail/MonadIO
type KM m = Expr -> Store -> m Store

evalM :: Expr -> Env -> KM IO -> Store -> IO Store
evalM (ETrue) env k store = k ETrue store
evalM (EFalse) env k store = k EFalse store
evalM (ELabel identifier) env k store = k (find env store identifier) store
evalM (Cond e p q) env k store = condk store
  where
    condk = evalM e env (\e' s' -> case e' of
                                    ETrue -> evalM p env k s'
                                    EFalse -> evalM q env k s'
                                    _ -> error "Type Error: Cond expects tt or ff")

evalM (ELTE e1 e2) env k store = contLTE
  where
    typeError = error "Type Error: LTE expects Const Int"
    contLTE = evalM e1 env (contE1) store
    contE1 = (\e1' s' -> case e1' of
                          (Const e1i) -> evalM e2 env (contE2 e1i) s'
                          _ -> typeError)

    contE2 e1i = (\e2' s''-> case e2' of
                          (Const e2i) -> k (answer e1i e2i) s''
                          _ -> typeError)

    answer x y = case x <= y of
                   True -> ETrue
                   False -> EFalse

evalM (Var i) env k store = k (store M.! i) store
evalM c@(Const i) env k store = k c store

evalM e env k store | trace (show e) False = undefined

interpretM :: Command -> Env -> (Store -> IO Store) -> (Store -> IO Store)
--interpretM gamma env k s | trace ("interpretting " <> show gamma) False = undefined
interpretM w@(While e gamma) env k s = evalM e env whileCont s
  where whileCont e' s' = case e' of
                            ETrue -> interpretM gamma env (\s'' -> interpretM w env k s'') s'
                            EFalse -> k s'
                            _ -> error "Type Error: While expects tt/ff expr"

interpretM (Dummy i) env k s = do
    print ("interpetM Dummy" <> show i)
    k s

interpretM (VarDecl i e) env k s = k (M.insert i e s)
interpretM (Incr i) env k s = case M.lookup i s of
                                Nothing -> error $ "Missing Var Error: Identifier "<> i <> " undeclared"
                                Just e -> case e of
                                            (Const v) -> k (M.insert i (Const (v+1)) s)
                                            _ -> error $ "Type Error: Incr expects Const"
interpretM (Print i) env k s = do
  print ("interpretM Print" <> show (s M.! i))
  k s

interpretM (Sequence g g') env k s = interpretM g env (\s' -> interpretM g' env k s') s
interpretM (CommandBlock []) env k s = k s
interpretM (CommandBlock ((_,g):gs)) env k s = interpretM g env (\s' -> interpretM (CommandBlock gs) env k s') s
interpretM (IFE e gtrue gfalse) env k s =  evalM e env (\e' s' -> case e' of
                                                                 ETrue -> interpretM gtrue env k s'
                                                                 EFalse -> interpretM gfalse env k s'
                                                                 _ -> error "Type Error: IFE Expets tt/ff") s
interpretM (Goto label) env@(Env gotoTable) k s = (gotoTable LM.! label) s

idM :: Store -> IO Store
idM = return

tW = interpretM (While EFalse (Dummy 1)) blankEnv idM emptyStore
tWW = interpretM (While ETrue (Dummy 1)) blankEnv idM emptyStore
tWWW = interpretM (Sequence (VarDecl "x" (Const 1))
                            (While (ELTE (Var "x") (Const 5))
                                   (Sequence (Print "x")
                                             (Incr "x")))) blankEnv idM emptyStore

emptyStore = M.empty
--TODO test While
--TODO figure out the difference between environment and store for Lox


--eval (ELTE (Const 4) (Const 3)) Env kTrace emptyStore

tVD = interpretM (Sequence (VarDecl "x" (Const 0))
                           (Sequence (Incr "x") (Print "x")))
                blankEnv idM emptyStore


--Traverse AST for labels and their continuations.
--Labels must be distinct otherwise an error is thrown when its evaluation is forced
labelPass :: Command -> Env -> (Store -> IO Store) -> LM.Map Label (Store -> IO Store)
labelPass (Sequence g gs) env k     = LM.union (labelPass g env gcont) (labelPass gs env k)
  where gcont = interpretM gs env k

labelPass (IFE _ l r) env k         = LM.union (labelPass l env k ) (labelPass r env k)
labelPass (While _ g) env k         = labelPass g env k
labelPass c@(CommandBlock ls) env k = LM.unionsWith distinctLabelError (topLabels : nestedLabels)
  where
    conts = fmap (second (\c -> interpretM c blankEnv return)) ls
    topLabels = commandBlockConts ls env k
    nestedLabels = fmap ((\g -> labelPass g env k). snd) ls

labelPass _ _ _ = LM.empty
labelPass gamma env k | trace (show gamma) False = undefined

commandBlockConts :: [(Label, Command)] -> Env -> (Store -> IO Store) -> LM.Map Label (Store -> IO Store)
commandBlockConts [] env k = LM.empty
commandBlockConts ((l,g):gs) env k = LM.union (LM.singleton l gke) (commandBlockConts gs env k)
  where gke = interpretM g env (interpretM (CommandBlock gs) env k)


insertDistinct :: (Ord k) => k -> v -> LM.Map k v -> LM.Map k v
insertDistinct = LM.insertWith (\_ _ -> error "Labels must be distinct")

--We could use set intersection to find nondistinct labels alternatively as a check

distinctLabelError = (\_ _ -> error "Labels must be distinct")

--TODO stash a label map into Env and look it up on goto
blankEnv = Env LM.empty

prog = Sequence (VarDecl "x" (Const 0))
       (Sequence (VarDecl "y" (Const 666))
       (Sequence (CommandBlock [("10" ,Incr "x"), ("20", Print "x")])
       (Sequence (IFE (ELTE (Var "x") (Const 5))
                      (Goto "10")
                      (Print "y"))
                 (Print "x"))))

testProg = interpretM prog (fixProgEnv' prog) idM emptyStore

--This is to get labelPass to reference itself, the newly created env.
fixProgEnv prog = env
  where
    env = Env (labelPass prog env kId)

fixProgEnv' prog = fix (\e -> Env (labelPass prog e kId))

kId = return :: Store -> IO Store
\end{code}
\end{verbatim}

\end{document}
