# Talks I Like (and need to write about)

## [Replacing functions with data - Bartosz Milewski - Haskell Love! 2020](https://www.youtube.com/watch?v=wppzFzzD4b8)

Bartosz shows the link between recursion and imperative functions in a very direct manner using a mechanical transformation (via CPS'ing and defunctionalization).

Probably the most concrete proof I've seen of the truth that recursion and imperative programs can represent each other. (Once you step past the simple case of tail recursion being iteration)

It also show cases the link between code and data in a nonLisp context.

## Trace Trick

[From this reddit comment.](https://www.reddit.com/r/haskell/comments/py8u24/why_did_haskell_not_succeed/hetrqzv/)

```haskell
import Debug.Trace

fib :: Integer -> Integer
fib n | trace ("fib " ++ show n) False = undefined
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```

## Well Typed Programs and Type Safety

[Alexis King - Parse, don't validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/).

This is probably what openned my eyes to type safety.

TODO READ THIS: [Ghosts of Departed Proofs (Functional Pearl)](https://kataskeue.com/gdp.pdf)

### DSLs
We need a clean representation of the ideas from [Daan Leijen, Meijer Domain Specific Embedded Compilers](https://www.usenix.org/legacy/events/dsl99/full_papers/leijen/leijen.pdf).

#### Simple illtyped Expr
```haskell
data Expr = Lit Int
            Add Expr Expr
            Equals Expr Expr
```

#### TODO Phantom Type Version

TODO read through [Fun with Phantom Types by R. Hinze 2003](https://www.cs.ox.ac.uk/ralf.hinze/publications/With.pdf)

```haskell
data Expr a = Lit a
            | Add (Expr a) (Expr a)
            | Equals (Expr a) (Expr a)
```

#### GADTs version similar to Haskell Wikibook example
```haskell
{-# LANGUAGE GADTs #-}

data Expr a where
    Lit :: a -> Expr a
    Add :: Expr Int -> Expr Int -> Expr Int
    And :: Expr Bool -> Expr Bool -> Expr Bool
    Equals :: Expr Int -> Expr Int -> Expr Bool
```

#### GADTs version with DataKinds,TypeFamilies,ConstraintKinds.

A nice introduction to [ConstraintKinds I read by Kwang Yul Seo](https://kseo.github.io/posts/2017-01-13-constraint-kinds.html).
I originally ran into some of these techniques when I was trying to express ["Tainted and Untainted" types for expressing natural deduction proofs](https://github.com/adpextwindong/NDNotes) while reading [Wadler 2015 Propositions as Types](https://dl.acm.org/doi/10.1145/2699407). [Example1](https://github.com/adpextwindong/NDNotes/blob/main/TaintedExample1.hs) and [Example2](https://github.com/adpextwindong/NDNotes/blob/main/TaintedExample2.hs). This initial stab at expressing proofs in Haskell with typesafety wasn't that successful but the techniques I learned from it were great. I should probably read into how proofs are expressed in other systems.

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}       --For 'Measurable
{-# LANGUAGE TypeFamilies #-}    --For KMetric'
{-# LANGUAGE ConstraintKinds #-} --For KMetric t

data Line = KLine
data Point = KPoint

data Measurable = 'Measurable               --Measurable Types Constraint TypeFamily

type family KMetric' a where                --Measurable Primitives Constraint Type Family
    KMetric' Line = 'Measurable

type KMetric t = (KMetric' t ~ 'Measurable) --Measurable ConstraintKind

data KExpr a where
    K :: a -> KExpr a
    C :: ConstExpr a -> KExpr a
    KLength :: (KMetric a) => KExpr a -> KExpr Float
    KConLine :: KExpr Point -> KExpr Point -> KExpr Line
    KAdd :: (Num a) => KExpr a -> KExpr a -> KExpr a

--Used with KExpr C constructor to provide labeled and typed constants.
--Pattern found from https://stackoverflow.com/a/37359542
data ConstExpr a where
    ConstF :: String -> ConstExpr Float
```

Example error
```
<interactive>:1:1: error:
    • Couldn't match type ‘KMetric' Point’ with ‘'Measurable’
        arising from a use of ‘KLength’
    • In the expression: KLength (K KPoint)
      In an equation for ‘it’: it = KLength (K KPoint)
```

Alternative minimal version without 'Measurable DataKind for improved type error messages.

```haskell
type family KMetric' a where
    KMetric' Line = 'True

type KMetric t = (KMetric' t ~ 'True)
```

```
<interactive>:1:1: error:
    • Couldn't match type ‘KMetric' Point’ with ‘'True’
        arising from a use of ‘KLength’
    • In the expression: KLength (K KPoint)
      In an equation for ‘it’: it = KLength (K KPoint)
```

As you can see this leads to readability issues. Adding a 'Measurable datakind adds readability at the cost of polluting the namespace.

### GADTs

Morrow's post on [Tying your Shoes with GADTs](https://www.morrowm.com/posts/2021-08-02-shoes.html)

### Partial VS Total

From [Calculating Correct Compilers](https://www.cs.nott.ac.uk/~pszgmh/ccc.pdf)

```
Partiality Because the ADD instruction fails if the stack does not contain at least two values, the function exec the implements the virtual machine is partial. As remarked by Ager et al. (2003a), such partiality is “inherent to programming abstract machines in an ML-like language”. If desired, exec could be turned into a total function by using a  dependently typed language to make the stack demands of each machine instruction explicit in its type (McKinna & Wright, 2006). However, we do not require such additional effort  here as we are only interested in the behaviour of exec for well-formed code produced by our compiler, as expressed in specifications (3) and (4).
```

TODO read McKinna & Wright 2006.

## [Continuation Passing Style](https://en.wikipedia.org/wiki/Continuation-passing_style), Normal Order Evaluation, Applicative Order Evaluation

### Continuation Passing Style

From wikipedia:

```
Expressing code in this form makes a number of things explicit which are implicit in direct style. These include: procedure returns, which become apparent as calls to a continuation; intermediate values, which are all given names; order of argument evaluation, which is made explicit; and tail calls, which simply call a procedure with the same continuation, unmodified, that was passed to the caller."
```

The explicit order of argument evaluation is a nice bit to expand on.

### Normal Order vs Applicative Order

This writing should pair with Applicative Order vs Normal Order evaluation in the SLPJ notes.

Aho has some nice lecture notes regarding this. [I](http://www.cs.columbia.edu/~aho/cs3261/Lectures/L23-Lambda_Calculus_I.html), [II](http://www.cs.columbia.edu/~aho/cs3261/Lectures/L24-Lambda_Calculus_II.html).
Here's a really cool open book on [Programming Languages with some exercises related to reduction strategies](https://opendsa-server.cs.vt.edu/OpenDSA/Books/PL/html/ReductionStrategies.html).

From ["Self-replicating Expressions in the Lambda Calculus" by James Larkin Phil Stocks](https://dl.acm.org/doi/pdf/10.5555/979922.979943)

```
Church-Rosser Theorem II states that if an expression has a normal form, then normal order reduction is guaranteed to find it, but applicative order reduction is not necessarily guaranteed to find it.
```

TODO We need to hammer on this guaranteed bit.

SLPJ's book [2.3.1 Normal Order Reduction](SLPJ_READING/slpj-book-1987.pdf#page=36) talks about Church-Rosser Theorem I and II.

- [Continuation Passing Style from AI Memo 349: Scheme - An Interpreter for extended lambda calculus](https://www.researchgate.net/profile/Gerald-Sussman-2/publication/227098423_Scheme_A_Interpreter_for_Extended_Lambda_Calculus/links/53d1413f0cf220632f392bf3/Scheme-A-Intended-Lambda-Calcrpreter-for-Exteulus.pdf)

- [Note: An more original but dirtier looking version of the paper from MIT](https://dspace.mit.edu/bitstream/handle/1721.1/5794/AIM-349.pdf)

- [Reynolds93 - The Discoveries of Continuations](https://homepages.inf.ed.ac.uk/wadler/papee/reynolds-discors/papers-we-lovveries.pdf)

One thing to note is that CPS is orthoginal to normal and applicative evaluation order. Given a CPS'd program with all of its reducible subexpressions reduced, the evaluation order under applicative order and normal form order is the same.

Thanks to Jagen and Codingbuddy this intuition is confirmed to be correct. Apparently a proof exists in [Plotkin's 1975 "Call-By-Name, Call-By-Value and the λ-Calculus"](https://homepages.inf.ed.ac.uk/gdp/publications/cbn_cbv_lambda.pdf)

```
adpx — Today at 12:09 AM
for a continuation passing style program, is applicative order evaluation practically indistinguishable from normal order evaluation? (Assuming reducible subexpressions are already reduced before cps'ing it) (edited)

codingbuddy — Today at 12:14 AM
yeah

Gooby — Today at 12:24 AM
I am not sure what those things are tbhdesu, can you define them uwu

adpx — Today at 12:33 AM
http://www.cs.columbia.edu/~aho/cs3261/Lectures/L24-Lambda_Calculus_II.html

adpx — Today at 9:58 AM
I wonder if theres a proof of this written somewhere.

codingbuddy — Today at 9:59 AM
absolutely
@jagen 's the expert on this

jagen — Today at 10:06 AM
definitely not preserved for eternity in a paper from 1975 called "call by name, call by value, and the lambda calculus"
the proof is that you can use call by name reduction after cpsing and it will be observationally equivalent to call by value lambda calculus
it will be closed as well, so performing beta reduction on a cps'd term will yield a cps'd term
and since you're using call by name beta reduction you can prove more equations
```

From ["(LEFTMOST-OUTERMOST) BETA REDUCTION IS INVARIANT, INDEED" by BENIAMINO ACCATTOLI AND UGO DAL LAGO](https://arxiv.org/pdf/1601.01233.pdf) page 3.
```
Such an evaluation strategy [Normal Order Evaluation] is standard, in the sense of the standardization theorem, one of the central theorems in the theory of λ-calculus, first proved by Curry and Feys [CF58].
```
TODO We should dig up this bit on the Standardization theorem from the [Curry and Feys book on Combinatory Logic](SLPJ_READING/Curry58_CombinatoryLogic.djvu). The intro paragraph on [wikipedia about reduction strategy mentions it.](https://en.wikipedia.org/wiki/Reduction_strategy#Lambda_calculus).

## Other topics

- Boolean blindness and the significance of Sum types (or algebraic data types)

Implementing modes in my [ThreeJS Last Viewer](https://github.com/adpextwindong/Last-Viewer) was painful without sum types. For a long while it was just bools (and too many of them) with associated data lieing around. It could've been possible to emulate them with a tagged union and [object freezing on symbols](https://github.com/adpextwindong/Last-Viewer/blob/abdfd0645f3a10336c046cf16474a99d0949536a/src/engine/scene/landmark_utils.js#L57) but I don't find that particularly safe or fun to do in such a weak language.

## READING BACKLOG

### Defunctionalization

- [Reynolds72 - Definitional interpreters for higher-order programming languages](https://surface.syr.edu/cgi/viewcontent.cgi?article=1012&context=lcsmith_other)
- [Danvy01 - Defunctionalization at Work](https://www.brics.dk/RS/01/23/BRICS-RS-01-23.pdf)

### Other

- [SSA is functional programming](https://www.cs.princeton.edu/~appel/papers/ssafun.pdf)

- [Compiler Design CS4410](https://course.ccs.neu.edu/cs4410sp20)

- [Andrew Appel - Modern Compiler Implementation in ML](https://www.cs.princeton.edu/~appel/modern/ml/)