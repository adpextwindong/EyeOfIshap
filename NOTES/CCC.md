# Notes on "[Calculating Correct Compilers](https://www.cs.nott.ac.uk/~pszgmh/ccc.pdf)"

## 1. Introduction

```
The starting point of our calculation process is:

- The semantics for the source language in the form of an `evaluation function`
- Formulating an `equational specification` that captures the correctness of the compiler.
- Definitions of the compiler and the virtual machine are calculated by constructive induction (Backhouse, 2003)

## 2. Arithmetic Expressions

```haskell
data Expr = Val Int | Add Expr Expr
```

We apply a number of transformation steps to the seamnntics of the language. The transformation steps involving the following:

- Continuations
- Defunctionalisation

The process is simplified by combining the seperate transformation steps, resulting in a simple but powerful new approach.

## 2.1 - Define the language semantics

```haskell
eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y
```

Key point to note is that eval is compositional.

// Conal Elliot really harped on this in his [denotational design talk](https://www.youtube.com/watch?v=bmKYiUOEo2A)

### 2.2 - Step 2 - Transform into a stack transformer
```haskell
type Stack = [Int]
--Head is the top element of the stack

--then we seek to derrive a function

evalS :: Expr -> Stack -> Stack
evalS = undefined
```

such that:

```
evalS x s = eval x : s                                  -- (1)
```

"Rather than first defining the function evalS and then seperately proving by induction that it satisfies the above equation, we aim to calculate a deifnition for evalS that satisfies the equation by `constructive induction` (Backhouse, 2003) on the expression x, using the desire to apply the induction hypotheses as the driving force for the calculation process."

#### Transformation on term `evalS x s` gradually by equational reasoning

--TODO Read through Backhouse to look at these Calculational Proofs

Note: The layout of this is weird and I think comes from Backhouse.
Note2: The explicit stack transformation and cps transformation reminds me of Bartosz's talk [Replacing function with data](https://www.youtube.com/watch?v=wppzFzzD4b8)
# Refs

- Backhouse, Roland (2003). Program Construction: Calculating Implementations from Specifications. John Wiley and Sons, Inc.

### 2.6 Reflection
#### Partiality

//This is the exact concrete bit on dependant types I've been looking for ever.

```
Because the ADD instruction fails if the stack does not contain at least two values, the function exec the implements the virtual machine is partial. As remarked by Ager et al. (2003a), is “inherent to such partiality programming abstract machines in an ML-like language”. If desired, exec could be turned into a total function by using a dependently typed language to make the stack demands of each machine instruction explicit in its type (McKinna & Wright, 2006).
```
