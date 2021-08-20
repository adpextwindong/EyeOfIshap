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

## 2.1 Define the language semantics

```haskell
eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y
```

Key point to note is that eval is compositional.

// Conal Elliot really harped on this in his [denotational design talk](https://www.youtube.com/watch?v=bmKYiUOEo2A)


# Refs

- Backhouse, Roland (2003). Program Construction: Calculating Implementations from Specifications. John Wiley and Sons, Inc.
