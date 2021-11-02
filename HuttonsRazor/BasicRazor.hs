{-
https://www.cs.nott.ac.uk/~pszgmh/semantics.pdf

2.1 Arithmetic Expressions

As an example, let us consider a language of simple arithmetic expressions, built up from the set Z of integer values using the addition operator +. The language E of such expressions is defined by the following grammar:

E ::= Z | E + E

-}

data Expr a = Val a | Add (Expr a) (Expr a)
