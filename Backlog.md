# Talks I Like (and need to write about)

## [Replacing functions with data - Bartosz Milewski - Haskell Love! 2020](https://www.youtube.com/watch?v=wppzFzzD4b8)

Bartosz shows the link between recursion and imperative functions in a very direct manner using a mechanical transformation (via CPS'ing and defunctionalization).

Probably the most concrete proof I've seen of the truth that recursion and imperative programs can represent each other. (Once you step past the simple case of tail recursion being iteration)

It also show cases the link between code and data in a nonLisp context.

## Well Typed Programs and Type Safety

[Alexis King - Parse, don't validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/).

This is probably what openned my eyes to type safety.

TODO READ THIS: [Ghosts of Departed Proofs (Functional Pearl)](https://kataskeue.com/gdp.pdf)

### DSLs
We need a clean representation of the ideas from [Daan Leijen, Meijer Domain Specific Embedded Compilers](https://www.usenix.org/legacy/events/dsl99/full_papers/leijen/leijen.pdf).

```haskell
data Expr = Lit Int
            Add Expr Expr
            Equals Expr Expr
```

Phantom Type Version

```haskell
data Expr a = Lit a
            | Add (Expr a) (Expr a)
            | Equals (Expr a) (Expr a)
```

```haskell
{-# LANGUAGE GADTs #-}

data Expr a where
    Lit :: a -> Expr a
    Add :: Expr Int -> Expr Int -> Expr Int
    And :: Expr Bool -> Expr Bool -> Expr Bool
    Equals :: Expr Int -> Expr Int -> Expr Bool
```

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

One thing to note is that CPS is orthoginal to normal and applicative evaluation order. Given a CPS'd program with all of its reducible subexpressions reduced, the evaluation order under applicative order and normal form order is the same. TODO find a proof for this and write an example.

From ["(LEFTMOST-OUTERMOST) BETA REDUCTION IS INVARIANT, INDEED" by BENIAMINO ACCATTOLI AND UGO DAL LAGO](https://arxiv.org/pdf/1601.01233.pdf) page 3.
```
Such an evaluation strategy [Normal Order Evaluation] is standard, in the sense of the standardization theorem, one of the central theorems in the theory of λ-calculus, first proved by Curry and Feys [CF58].
```
TODO We should dig up this bit on the Standardization theorem from the [Curry and Feys book on Combinatory Logic](SLPJ_READING/Curry58_CombinatoryLogic.djvu). The intro paragraph on [wikipedia about reduction strategy mentions it.](https://en.wikipedia.org/wiki/Reduction_strategy#Lambda_calculus).

## READING BACKLOG

### Defunctionalization

- [Reynolds72 - Definitional interpreters for higher-order programming languages](https://surface.syr.edu/cgi/viewcontent.cgi?article=1012&context=lcsmith_other)
- [Danvy01 - Defunctionalization at Work](https://www.brics.dk/RS/01/23/BRICS-RS-01-23.pdf)

### Other

- [SSA is functional programming](https://www.cs.princeton.edu/~appel/papers/ssafun.pdf)

- [Compiler Design CS4410](https://course.ccs.neu.edu/cs4410sp20)

- [Andrew Appel - Modern Compiler Implementation in ML](https://www.cs.princeton.edu/~appel/modern/ml/)