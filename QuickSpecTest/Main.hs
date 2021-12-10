module Main where

import Control.Monad
import QuickSpec

-- Quick Specifications for the Busy Programmer
-- https://smallbone.se/papers/quickspec2.pdf

-- QuickSpec: A Lightweight Theory Exploration Tool for Programmers (System Demonstration)
-- https://www.youtube.com/watch?v=eTcq_QqhFfM

monadLaws = quickSpec [
        -- Typeclass Polymorphism isn't understood yet, see page 2. https://smallbone.se/papers/quickspec2.pdf
        --con "(>>=)" ((>>=) :: Monad m => m a -> (a -> m b) -> m b),
        --con "(>>)" ((>>) :: Monad m => m a -> m b -> m b),
        --con "return" (return :: (Monad m) => a -> m a)

        con ">>=" ((>>=) :: [A] -> (A -> [B]) -> [B]),
        con "return" (return :: A -> [A]),
        con ">=>" ((>=>) :: (A -> [B]) -> (B -> [C]) -> A -> [C])


        --Avoid lowercase, you'll get "No instance for (Typeable a0) arising from a use of 'con'" for some reason
        --con "return" (return :: a -> [a])

    ]

{-
== Functions ==
 (>>=) :: [a] -> (a -> [b]) -> [b]
return :: a -> [a]

== Laws ==
  1. xs >>= return = xs
  2. return x >>= f = f x

-- The Monad Laws. https://wiki.haskell.org/Monad_laws
1. being the right identity
2. being the left identity

The associativity law

(m >>= g) >>= h ≡	m >>= (\x -> g x >>= h)

isn't found with just bind and return. Adding the Kliesi composition does give us the associativity law when QuickSpec'd.

7. (f >=> g) >=> h = f >=> (g >=> h)

-}

frob :: [a] -> [a]
frob xs = xs ++ xs

frobFreeTheorem = quickSpec [
            con "frob" (frob :: [A] -> [A]),
            con "map" (map :: (A -> B) -> [A] -> [B])
        ]

{-
== Functions ==
frob :: [a] -> [a]
 map :: (a -> b) -> [a] -> [b]

== Laws ==
  1. map f (frob xs) = frob (map f xs)

This is the frob example from this talk by Lars Hupel https://www.youtube.com/watch?v=sR1tEk5nTFU
Which originally is from Wadler's Theorem for Free "3.5 A result about map" https://home.ttic.edu/~dreyer/course/papers/wadler.pdf

-}

concatWadler = quickSpec [
        con "(++)" ((++) :: [A] -> [A] -> [A])
    ]

-- This gives us append law discussed in Wadler's critique on SICP
-- 1.2 Proving properties of programs
-- https://www.cs.kent.ac.uk/people/staff/dat/miranda/wadler87.pdf

{-

== Laws ==
  1. (xs (++) ys) (++) zs = xs (++) (ys (++) zs)

-}

main = concatWadler
