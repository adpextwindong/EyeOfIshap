{-# LANGUAGE Arrows #-}
module Main where

import Control.Arrow
import Control.Applicative
import QuickSpec

--https://www.youtube.com/watch?v=6Vab1_icBWU
--Arrows In Haskell, Helsinki FRP Meetup 2015 May 06
--https://blog.paulme.ng/posts/2012-04-22-haskell-arrow.html

type Circuit = (->)

wire :: Circuit Bool Bool
wire = id

splittedWire :: Circuit Bool (Bool, Bool)
splittedWire = (wire &&& wire)

nandGate :: Circuit (Bool, Bool) Bool
nandGate = \(x,y) -> not (x && y)

-- Composes arrows for us
-- (>>>) :: cat a b -> cat b c -> cat a c

inverter :: Circuit Bool Bool
inverter = splittedWire >>> nandGate

-- Composes arrows in parallel
-- (***) :: a b c -> a b' c' -> a (b, b') (c, c')

parallelInverters :: Circuit (Bool, Bool) (Bool, Bool)
parallelInverters = inverter *** inverter

orGate :: Circuit (Bool, Bool) Bool
orGate = parallelInverters >>> nandGate

--Proc style orGate
--Automatically translates to the correct composition operators using the Arrows language directive
orGate' :: Circuit (Bool, Bool) Bool
orGate' = proc (a, b) -> do
    m1 <- nandGate -< (a,a)
    m2 <- nandGate -< (b,b)
    nandGate -< (m1, m2)

norGate :: Circuit (Bool, Bool) Bool
norGate = orGate >>> inverter

norGate' :: Circuit (Bool, Bool) Bool
norGate' = proc (a, b) -> do
    m1 <- orGate' -< (a, b)
    inverter -< m1

--NAND construction
--https://en.wikipedia.org/wiki/Logic_gate#/media/File:XNOR_from_NAND.svg
xnorGate' :: Circuit (Bool, Bool) Bool
xnorGate' = proc (a, b) -> do
    m1 <- nandGate -< (a,b)

    m2 <- nandGate -< (a,m1)
    m3 <- nandGate -< (b,m1)

    m4 <- nandGate -< (m2,m3)

    nandGate -< (m4,m4)

inputsPairs = liftA2 (,) vals vals
    where
        vals = [False,True]

xnorTruthTable = xnorGate' <$> inputsPairs

orGateRelatedLaws = quickSpec [
            con "wire" (wire :: Circuit Bool Bool),
            con "splittedWire" (splittedWire :: Circuit Bool (Bool, Bool)),
            con "nandGate" (nandGate :: Circuit (Bool, Bool) Bool),
            con "inverter" (inverter :: Circuit Bool Bool),
            con "parallelInverters" (parallelInverters :: Circuit (Bool, Bool) (Bool, Bool)),
            --con "orGate" (orGate :: Circuit (Bool, Bool) Bool)
            con "orGate'" (orGate' :: Circuit (Bool, Bool) Bool)

            ]

{-
== Functions ==
             wire :: Bool -> Bool
     splittedWire :: Bool -> (Bool, Bool)
         nandGate :: (Bool, Bool) -> Bool
         inverter :: Bool -> Bool
parallelInverters :: (Bool, Bool) -> (Bool, Bool)
           orGate :: (Bool, Bool) -> Bool

== Laws ==
  1. wire x = x
  2. inverter (inverter x) = x
  3. inverter x = nandGate (splittedWire x)
  4. orGate x = nandGate (parallelInverters x)
  5. orGate (splittedWire x) = x
  6. nandGate x = orGate (parallelInverters x)
  7. parallelInverters (splittedWire x) = splittedWire (inverter x)
  8. parallelInverters (parallelInverters x) = x
-}

main = quickSpec [
        con "arr" (arr :: (B -> C) -> Circuit B C),
        con "first" (first :: Circuit B C -> Circuit (B, D) (C, D)),
        con "second" (second :: Circuit B C -> Circuit (D, B) (D, C)),
        con "(***)" ((***) :: Circuit B C -> Circuit D E -> Circuit (B, D) (C, E)),
        con "(&&&)" ((&&&) :: Circuit B C -> Circuit B D -> Circuit B (C, D))
   ]

{-
== Functions ==
    arr :: (a -> b) -> a -> b
  first :: (a -> b) -> (a, c) -> (b, c)
 second :: (a -> b) -> (c, a) -> (c, b)
((***)) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
((&&&)) :: (a -> b) -> (a -> c) -> a -> (b, c)

== Laws ==
  1. arr f = f
  2. first f (second g x) = (f (***) g) x
  3. second f (first g x) = (g (***) f) x
  4. (f (***) g) ((h (&&&) h) x) = (f (&&&) g) (h x)
-}
