An Implementation of Modular Monadic Semantics using Folds and Monadic Folds
Jose Emilio Labra Gayo

https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.30.7583&rep=rep1&type=pdf



††2 Modular Abstract Syntax

The abstract syntax of a programming language can be defined as the fixed point of a functor that captures the recursive shape of that language.

NOTE: Using Fix as the constructor instead of "In" as in
Chris Penner's post bridged this gap for me. https://chrispenner.ca/posts/asts-with-fix-and-free

\begin{code}

newtype Fix f = Fix (f (Fix f))

\end{code}

For example, the non-recurisve functor N represents the recurisve shape of a simple language of arithmetic expressions:

\begin{code}
data N a = Cons Int | Add a a | Dvd a a
    deriving Show
\end{code}

and the abstract syntax of the language will be:

\begin{code}

type Arith = Fix N

x = Add (Cons 5) (Cons 5) :: N (N a)
tx = Fix (Add (Fix (Cons 5)) (Fix (Cons 5))) :: Fix N
ty = Fix (Add (Fix (Cons 5)) (Fix (Cons 5))) :: Arith

\end{code}

We are interested to compose the abstract syntax in a modular way. L. Duponcheel [3] presents a technique that we call extensible union functors to construct a new functor from two functors:

\begin{code}

data SumF f g x = S (Either (f x) (g x))

unS (S x) = x

instance (Functor f, Functor g) => Functor (SumF f g)
    where fmap g = S . either (Left . fmap g)
                              (Right . fmap g) . unS
\end{code}

Using that technique, it is very simple to compose the abstract syntax of a language from independent componets. We can define, for example, a new language of arithemtic and boolean expressions as:

\begin{code}
data B a = BCons Bool | Not a | Less a a

type ArithBool = Fix (SumF N B)

t_x = Fix $ S $ Right $ BCons True :: ArithBool
t_y = Fix (S (Right (Less (Fix (S (Left (Cons 5)))) (Fix (S (Left (Cons 5))))))) :: ArithBool
\end{code}


NOTE: This is also shown here http://web.archive.org/web/20170705072959/http://www.ittc.ku.edu/publications/documents/Alexander2006_ITTC-FY2006-TR-30150-06.pdf

http://www.ittc.ku.edu/publications/documents/Alexander2006_ITTC-FY2006-TR-30150-06.pdf

TODO look at https://hackage.haskell.org/package/unification-fd-0.11.1

Modular Monadic Semantics https://flint.cs.yale.edu/trifonov/cs629/modular-monadic-semantics.pdf
https://flint.cs.yale.edu/trifonov/cs629/
https://flint.cs.yale.edu/trifonov/cs430/
