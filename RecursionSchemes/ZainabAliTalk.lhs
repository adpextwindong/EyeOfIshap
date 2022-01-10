- [Peeling the Banana: Recursion Schemes from First Principles - Zainab Ali](https://www.youtube.com/watch?v=XZ9nPZbaYfE)

NOTE: Original slides are in Scala.

-- Recurisve Data Types

\begin{code}
data List a = Nil | Cons a (List a)

--Cons 1 $ Cons 2 $ Cons 3 Nil
\end{code}

List termination?

Scala eagerly evaluates the tail however in Haskell it can be infinite.

Finite data structures can be collapsed.

-- Recursive Collapse
```haskell
multiply :: (Num a) => List a -> a
multiply Nil = 1
multiply (Cons i xs) = i * multiply xs

length' :: List a -> Int
length' Nil = 1
length' (Cons _ xs) = 1 + length' xs
```

Can be replaced with a fold

-- Recursive Collapse
\begin{code}
--Right fold as the parens go to the right
foldList :: b -> (a -> b -> b) -> List a -> b
foldList onNil onCons Nil = onNil
foldList onNil onCons (Cons i xs) = onCons i $ foldList onNil onCons xs

\end{code}

\begin{code}
multiply = foldList 1 (*)

length' :: List a -> Int
length' = foldList 0 (\_ len -> len + 1)
\end{code}

`multiply` and `length'` are _catamorphisms_

-- Recursive Collapse

\begin{code}

data Tree a = Leaf a | Node (Tree a) (Tree a)

--foldTree :: (a -> b) -> (b -> b -> b) -> Tree a -> b
foldTree onLeaf onNode (Leaf x) = onLeaf x
foldTree onLeaf onNode (Node l r) = onNode (foldTree onLeaf onNode l)
                                           (foldTree onLeaf onNode r)

sumTree = foldTree id (+)
countLeaves = foldTree (\_ -> 1) (+)
\end{code}

-- Generalized Collapse

List & foldList vs Tree & foldTree

They have different shapes.

-- Category Theory

% https://tikzcd.yichuanshen.de/#N4Igdg9gJgpgziAXAbVABwnAlgFyxMJZABgBpiBdUkANwEMAbAVxiRAEEQBfU9TXfIRQBGclVqMWbAELdeIDNjwEiAJjHV6zVohABhbuJhQA5vCKgAZgCcIAWyRkQOCElEgARjDBQkAZicGOi8GAAV+ZSEQBhhLHBBNSR0QSzkrWwdEdxckdU9vX0QAxO02EwTnOiwGNkgwVh50+zdqHKzqBggINCIyS0Y4GHEgkPClQTYsMGxYCq0pXSwoWQ6p5LgIBiWKgAqG+RtmxCc2wK6elFsYOkKwJgYGVqqa3TUAdj6BoY7gmDCIia6axYEw7eIlBYgJacRopDK5VquRB5TrdXqkfoMQbDX7-cYqIEgsFzJKTKAGWG3TIPJF5Lw+JAAWgALABOCHJSwAAgAdFzyj9RgCCSBgaD4lwKFwgA

\begin{tikzcd}
A \arrow[r, "f", bend left] \arrow["idA"', loop, distance=2em, in=215, out=145] \arrow[rr, "f . g"', bend right=49] & B \arrow[r, "g", bend left] \arrow["idB" d, distance=2em, escription, loopin=305, out=235] & C \arrow["idC"', loop, distance=2em, in=305, out=235]
\end{tikzcd}

```
idA . f = f . idB = f
(f . g) . h = f . (g . h)
```

-- Category Theory

Collapse a recursive data type R to a value A

% https://tikzcd.yichuanshen.de/#N4Igdg9gJgpgziAXAbVABwnAlgFyxMJZABgBpiBdUkANwEMAbAVxiRACUQBfU9TXfIRQAmclVqMWbAILdeIDNjwEiARlKrx9Zq0QgA-N3LZIyIHBCTqQAIxhgEwoAc3hFQAMwBOEAoSAMzO2lJ6AMZ0OHRy1naOiM6uSKJePn6IALSBPDEOSdSJiB7evkiZxFwUXEA
\begin{tikzcd}
R \arrow[rr, "cata", bend left] \arrow[rd, bend right] &                          & A \\
                                                       & ? \arrow[ru, bend right] &
\end{tikzcd}

-- Higher Kinded Types

```
data List a = Nil | Cons a (List a)
```

NOTE the original List in the slides was hard coded to Int.

-- Isomorphism

-- Functors

\begin{code}
instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) $ fmap f xs
\end{code}

FA -alg-> A

type Algebra f a = f a -> a
