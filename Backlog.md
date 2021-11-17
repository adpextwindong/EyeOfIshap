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

##[Ghosts of Departed Proofs (Functional Pearl)](https://kataskeue.com/gdp.pdf)

```
But despite this menagerie of powerful type systems, workaday Haskell programmers have already been able to encode suprisingly sophisticated invariants using nothing more than a few well-understood extensions to the Damas-Hindley-Milner type system.

An early success story is the ST monad, which allows pure computations to make use of local, mutable state. A phantom type parameter and a clever use of rank-2 types in the ST monad's API gives a compile-time guarantee that the local mutable state is invisible from the outside, and hence the resulting computation really is pure...
```
From Ghost of Departed Proofs Introduction

--TODO Read [A Foundation for Embedded Languages](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.94.4331&rep=rep1&type=pdf)


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

TODO read through [A Foundation for Embedded Languages](http://web.archive.org/web/20170809090906/https://www.brics.dk/RS/02/34/BRICS-RS-02-34.pdf)

#### GADTs version with DataKinds,TypeFamilies,ConstraintKinds.

A nice introduction to [ConstraintKinds I read by Kwang Yul Seo](https://kseo.github.io/posts/2017-01-13-constraint-kinds.html).
I originally ran into some of these techniques when I was trying to express ["Tainted and Untainted" types for expressing natural deduction proofs](https://github.com/adpextwindong/NDNotes) while reading [Wadler 2015 Propositions as Types](https://dl.acm.org/doi/10.1145/2699407). [Example1](https://github.com/adpextwindong/NDNotes/blob/main/TaintedExample1.hs) and [Example2](https://github.com/adpextwindong/NDNotes/blob/main/TaintedExample2.hs). This initial stab at expressing proofs in Haskell with typesafety wasn't that successful but the techniques I learned from it were great. I should probably read into how proofs are expressed in other systems.

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}       --For 'Primitive
{-# LANGUAGE TypeFamilies #-}    --For KPrim'
{-# LANGUAGE ConstraintKinds #-} --For KPrim t

data Image = Png
data Line = KLine
data Point = KPoint

data Primitive = 'Primitive

type family KPrim' a where
    KPrim' Line = 'Primitive
    KPrim' Point = 'Primitive

type KPrim t = (KPrim' t ~ ' Primitive)

class Measure a
instance Measure Line

data KExpr a where
    K :: a -> KExpr a
    C :: ConstExpr a -> KExpr a
    KLength :: (Measure a) => KExpr a -> KExpr Float
    KRender :: (KPrim a) => KExpr a -> KExpr Image
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
type family KPrim' a where
    KPrim' Line = 'True

type KPrim t = (KPrim' t ~ 'True)
```

```
<interactive>:1:1: error:
    • Couldn't match type ‘KPrim' Point’ with ‘'True’
        arising from a use of ‘KLength’
    • In the expression: KLength (K KPoint)
      In an equation for ‘it’: it = KLength (K KPoint)
```

As you can see this leads to readability issues. Adding a 'Primitive datakind adds readability at the cost of polluting the namespace.

One thing to note is the difference of usage between Type Classes and Type Families in the KExpr example. In this case the type families are closed. [More information here from Serokell](https://seype-families-hasrokell.io/blog/tkell). While type class constraints can express a lot about your types, you're still restricted to talking about a single type a. Introducing a second parameter to KExpr has its own issues. Where type families are nice is when you want to work with types at the type level. For example having a 2ary typefamily accept Tainted kinded types to curse and uncurse a type. Or matrix/vector sizing like in [Hylogen](https://github.com/adpextwindong/hylogen/blob/protofeature/matrix/hylogen/src/Hylogen/Types/Mat.hs). In this example so far they closed type family KPrim' and Measure achieve similar constraints but express different things about the types. KPrim tells us primitives within the KExpr can be rendered to an image while Measure talks about values that have a measure instance outside of the KExpr language.

Generally if you can work with the types you build up in the language then you can use type families to compute a type using those types. This is hte primary mechanism for type safety in the KExpr example.

Additionally the ConstF constructor is a phantom type trick that lets us use strings in place for float constants. The semmantics of this are up to the eval/compile function but in the scheme of things its used to placehold a value while allowing for the rest of the parent expression to be type-safe.

Example from Hylogen.

```haskell
type family MulR a b where
    MulR (Expr (FloatVec 2)) (Expr (FloatMat 2 2)) = Expr (FloatVec 2)
    MulR (Expr (FloatVec 3)) (Expr (FloatMat 3 3)) = Expr (FloatVec 3)
    MulR (Expr (FloatVec 4)) (Expr (FloatMat 4 4)) = Expr (FloatVec 4)

    MulR (Expr (FloatMat 2 2)) (Expr (FloatVec 2)) = Expr (FloatVec 2)
    MulR (Expr (FloatMat 3 3)) (Expr (FloatVec 3)) = Expr (FloatVec 3)
    MulR (Expr (FloatMat 4 4)) (Expr (FloatVec 4)) = Expr (FloatVec 4)

    MulR (Expr (FloatMat 2 2)) (Expr (FloatMat 2 2)) = Expr (FloatMat 2 2)
    MulR (Expr (FloatMat 3 3)) (Expr (FloatMat 3 3)) = Expr (FloatMat 3 3)
    MulR (Expr (FloatMat 4 4)) (Expr (FloatMat 4 4)) = Expr (FloatMat 4 4)

mul :: (OrMatVec a, OrMatVec b, MulR a b ~ Expr c, b ~ Expr b0, a ~ Expr a0, ToGLSLType a0, ToGLSLType b0, ToGLSLType c) => a -> b -> MulR a b
mul = op2 "*"
```

TODO READ [Foundations for Structured Programming with GADTs.](https://strathprints.strath.ac.uk/33726/1/ghani_popl08.pdf)

### GADTs

Morrow's post on [Tying your Shoes with GADTs](https://www.morrowm.com/posts/2021-08-02-shoes.html)

### Partial VS Total

From [Calculating Correct Compilers](https://www.cs.nott.ac.uk/~pszgmh/ccc.pdf)

```
Partiality Because the ADD instruction fails if the stack does not contain at least two values, the function exec the implements the virtual machine is partial. As remarked by Ager et al. (2003a), such partiality is “inherent to programming abstract machines in an ML-like language”. If desired, exec could be turned into a total function by using a  dependently typed language to make the stack demands of each machine instruction explicit in its type (McKinna & Wright, 2006). However, we do not require such additional effort  here as we are only interested in the behaviour of exec for well-formed code produced by our compiler, as expressed in specifications (3) and (4).
```

### Dependent Types

Read [RAE's Dependently typed programming with singletons](https://repository.brynmawr.edu/cgi/viewcontent.cgi?article=1009&context=compsci_pubs)

#### Epigram1

[A type-correct, stack-safe, provably correct, expression compiler in Epigram by James McKinna and Joel Wright 2006](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.94.62&rep=rep1&type=pdf)
TODO figure out how to run Epigram1. [This might require some fixing.](https://github.com/david-christiansen/epigram1). It might be easiest to run under WSL or a Linux VM.

### Other

TODO EXPLORE Phantom Types for Vectors denoting which space its in. This might be a nice application of TypeOperators.
```
adpx — 06/18/2021
Do you guys do any type level programming to handle transformations between coordinate spaces at all?

Rotaerk — 06/18/2021
@adpx what are you imagining that to look like?

adpx — 06/18/2021
Like phantom types for Vectors
denoting which space its in.

Rotaerk — 06/18/2021
ah

adpx — 06/18/2021
I'm curious because I've tried writing raycasting code in Rust before but im' very clumsy at organizing that stuff

Rotaerk — 06/18/2021
maybe just use https://hackage.haskell.org/package/tagged-0.8.6.1/docs/Data-Tagged.html

adpx — 06/18/2021
Oh nice. I'll have to play with this. Man, I feel like theres always a Kmett library for anything useful and challenging to write.

Rotaerk — 06/18/2021
you can basically have a function that transforms from X space to Y space with a type like Tagged X Vector4 -> Tagged Y Vector4
can construct one like Tagged @X someVector4
if that's a bit too repetitive for you, you can make an alias like type SpaceVector4 s = Tagged s Vector4 and then xToY :: SpaceVector4 X -> SpaceVector4 Y
```

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

- [Concept Techniques and Models of Computer Programming](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.102.7366&rep=rep1&type=pdf)

Particularly the relational and constraint programming.

- [Fold and Unfold for Program Semantics](https://www.cs.nott.ac.uk/~pszgmh/semantics.pdf)
- [Jeff Polakow - Embedding a Full Linear Lambda Calculus in Haskell](http://functorial.com/Embedding-a-Full-Linear-Lambda-Calculus-in-Haskell/linearlam.pdf)
- [Haskell MicroKanren impl](https://github.com/seantalts/hasktrip/blob/master/doc/MicroKanren.md)
- [µKanren: A Minimal Functional Core for Relational Programming by Jason Hemann Daniel P. Friedman](http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf)

### UI

- [Comonads for user interfaces](https://arthurxavierx.github.io/ComonadsForUIs.pdf)
- [A Real-World Application with a Comonadic User Interface](https://arthurxavierx.github.io/RealWorldAppComonadicUI.pdf)
- [Comonads as Spaces by Phil Freeman](https://blog.functorial.com/posts/2016-08-07-Comonads-As-Spaces.html)
