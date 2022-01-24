# [YOW! Lambda Jam 2020 - Edward Kmett - Cadenza Building Fast Functional Languages Fast](https://www.youtube.com/watch?v=25RmUl88jSw)

[Cadenza](https://github.com/ekmett/cadenza)

In Kotlin, a lambda calculus interpreter with NBE. 1/5th as verbose as the Java, Scala doesn't do the java annotation preprocessing you need.

Why Kotlin? To run in the JVM to use graalvm and use its jit. Which gets to JIT for the evaluator for a dependently typed language. Jit the typelevel problems, which is an evaluator crunching away at substitutions. ("Big slow, often quadratic, often linear in the size of the term. really only care about it when we compare two types at the end")

# [The bit: Observe a couple things about expressions](https://youtu.be/25RmUl88jSw?t=1280)

In order to run an expression, all we needed was eval. What expr really is Expr :: Env -> Value

Moving towards an efficient machine representation it could be dropped into

```haskell
data Value
  = Closure (Env Value) Name (Expr) -- <-- right here
  | Neutral Neutral

data Neutral
  = NVar Name
  | NApp Neutral Value
```

The whole environment doesnt need to be captured either, just WhatDidINeed.

Same with Lam, it could precompute what it needs to just capture.

Expr could be arbitrary assembly that knows how to suck in those values...

SourceTerms -> Term -> Expr manipulate Value -> NF -> Expr

Normal Forms can be compared for equality quickly. Quick alpha-equality checks.
Going from NF to Expr without going through the type-checking process.

## Polymorphic inline cache (notion from the Self/Smalltalk world)

Virtual Function Call in C++

Deref vtable ptr thats at beginning/end of your object. Look in the vtable for what function to do, call it with the reference to the object.

2 derefs. Potentially 2 cache misses in a pathological case.

Replace with branch instead of deref to unknown address.

In a functional setting we need a notion of a polymorhpic inline cache that is

- arity aware (for over/under applying functions)

Truffle helps to write the slow path fallback.

- Trampolining (ex: Monad in ScalaZ)
- TCO on jvm? Kinda hosed
- Truffle can specify trampolines by hand, exception based control flow tricks and escape analysis into tight loop

Functional language requires multiple trampolines that degrade in quality

- Eval/Apply (like GHC under the hood)
- Environment trimming (GHC-style)
