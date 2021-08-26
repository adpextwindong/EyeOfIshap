# Intro

Implementing functional programming languages using _lazy graph reduction_

## 1.
- Translation from high level functional language into Lambda Calculus
- Pattern Matching
- Type Checking

## 2.
- Refinements and alternatives such as:
- Super Combinators
- Full laziness
- SK combinators

## 3.
- G-machine

The functional programming languages predating 1987:

- SASL [Turner, 1976](salsman.pdf)

- ML [Gorden et al., 1979](Gordon79_LectureNotesInComputerScience.pdf)

- KRC [Turner, 1982](turner82_RecursionEquations.pdf)

- Hope [Burstall et al., 1980](burstall80_Hope.pdf)

- Ponder [Fairbairn, 1985](Fairbairn75DesignAndImplementationOfASimpleTypedLanguageBasedOnTheLambdaCalculus.pdf)

- LML [Augustsson, 1984](augustsson84_ACompilerForLazyML.pdf)

- Miranda [Turner, 1985](turner85_Miranda.pdf)

- Orwell [Wadler, 1985](Wadler85_orwell.pdf)

Those with non strict semmantics being SASL, KRC, Ponder, LML, Miranda and Orwell.

ML and Hope being the strict languages in this group.

A super set of Lambda Calculus called _enriched lambda calculus_ will be used to specifically allow a straightfoward translation of a Miranda program into an expression in the enriched lambda calculus. This will be shown in chapter 3.

## Graph Reduction

```Haskell
f x = (x + 1) * (x - 1)
```

The expression `f 4` can be expressed as a tree of an application root with f and 4 as successors.

```
  @
 / \
f   4
```

Applying f to 4 yields

```
   *
 +    -
4 1  4 1
```

Executing the addition and subtration (in either order) gives

```
 *
5 3
```

Executing the last multiplication gives the result 15.

Evaluation of this tree (or more generally a graph) is done by a sequence of simple steps called reductions. Each reduction performing a local transformation of the graph (hence graphc reduction).

Reductions may safely take place in a variety of orders, even parallel, since they cannot interfere with each other.

When there are no further reducible expressions evaluation is completed.

# CH1 References
--TODO link these

[Abelson, H., and Sussman, G.J. 1985. Structure and Interpretation of Computer Programs. MIT Press.](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book.html)

[Augustsson, L. 1984. A compiler for lazy ML. Proceedings of the ACM Symposium on Lisp and Programming, Austin. August, pp. 218-27.](augustsson84_ACompilerForLazyML.pdf)

[Burstall, R.M., MacQueen, D.B., and Sanella, D.T. 1980. Hope: an experimental applicative language. CSR-62-80. Department of Computer Science, University of Edinburgh. May.](burstall80_Hope.pdf)

Darlington, J. 1984. Functional programming. In Distributed Computing. Duce (Editor). Academic Press. --NOTE Cannot find this online at all

[Fairbairn, J. 1985. Design and implementation of a simple typed language based on the lambda calculus. PhD thesis, TechnicalReport 75. University of Cambridge. May.](Fairbairn75DesignAndImplementationOfASimpleTypedLanguageBasedOnTheLambdaCalculus.pdf)

Glaser, H., Hankin, C., and Till, D. 1984. Principles of Functional Programming. Prentice-Hall. --TODO scan once it arrives stateside.

[Gordon, M.J., Milner, A.J., and Wadsworth, C.P. 1979. Edinburgh LCF. LNCS 78. Springer Verlag.](Gordon79_LectureNotesInComputerScience.pdf)

[Henderson,P. 1980. Functional Programming. Prentice-Hall.](Henderson80_FunctionalProgramming.djvu)

[Turner, D.A. 1976. The SASL language manual. University of St Andrews. December.](saslman.pdf)

[Turner, D.A. 1982. Recursion equations as a programming language. In Functional Programming and Its Applications, Darlington et al. (editors), pp. 1-28. Cambridge University Press.](turner82_RecursionEquations.pdf)

[Turner, D.A. 1985. Miranda — a non-strict functional language with polymorphic types. In Conference on Functional Programming Languages and Computer Architecture, Nancy, pp. |-16. Jouannaud (editor), LNCS 201. Springer Verlag.](turner85_Miranda.pdf)

[Wadler, P. 1985. Introduction to Orwell. Programming Research Group, University of Oxford.](Wadler85_orwell.pdf)