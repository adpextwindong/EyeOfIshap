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

# Chapter 2 - The Lambda Calculus

The lambda calculus as an intermediate language has two things going for it:

Simplicity and Expressiveness

You only need to support a few constructs and it has simple semantics for reasoning about correctness of the implementation. It can also express all functional prgorams (and all computable functions) meaning we can implement other functional languages by translating them down to the lambda calculus.

## 2.1 The Syntax of the Lambda Calculus

A simple expression
```
(+ 4 5)
```

All function applications in the lambda calculus are written in the prefix form. A more complex example.

NOTE: The Vim digraph command for Lambda is <ctrl>kl*
      See :digraphs for more.
```
(+ (* 5 6) (* 8 3))

-- A Haskell version of this being
(\x y -> x + y) ((\x y -> x * y) 5 6) ((\x y -> x * y) 8 3)

-- An even more verbose version being
(\x -> \y -> x + y) ((\x -> \y -> x * y) 5 6) ((\x -> \y -> x * y) 8 3)

-- Traditional
((λx.λy.x + y) ((λx.λy x * y) 5 6) ((λx.λy.x * y) 8 3))
```

Evaluation proceeds by repeatedly selecting a _reducible expression_ (or _redex_) and reducing it.

When there are several avalible redexes we have a choice of which one to reduce first.

## 2.2 Function Application and Currying

In the lambda calculus function application is denoted by simple juxtaposition

```
f x
```

This denotes f applied to the argument x.

When expressing the application of a function to serveral arguments how do we do that?

```
-- This would force (x,y) to be a pair or some C style syntax
(f (x,y))

-- Instead, for example the sum of 3 and 4 can be expressed as
(+ 3 4)

((+ 3) 4)

((λx.λy.x + y) 3) 4)

--In this situation the redex taken are
--β reduction -- Function Application
((λy.3 + y) 4)

--β reduction
(3 + 4)

--δ Reduction -- Calculation with predefined function on predefined data types
```

By expressing `(+ 3 4)` as `((+ 3) 4)` we can reason about lambda calculus abstractions (functions) taking only one argument. This process is known as _currying_, originally introduced by [Schonfinkel in 1924](Schonfinkel24-OnTheBuildingBlocksOfMathematicalLogic.pdf) and extensively used by [Curry and Feys 1958](Curry58_CombinatoryLogic.djvu).

--TODO PAGE 10

### [Summary of reduction rules](Huch_LambdaCalculusNotes.pdf#page=6)

- α-reduction: Renaming of Parameters

- β-reduction: Function Application

- η-reduction: Elimination of redundant λ-abstractions

- δ-reduction: Calculation with predefined function on predefined data types

## CH2 References
[Barendregt, H.P. 1984. The Lambda Calculus—Its Syntax and Semantics, 2nd edition. North-Holland.](BarendregtLambdaCalculus.pdf)

[Church, A. 1941. The Calculi of Lambda Conversion. Princeton University Press.](Church41_TheCalculiOfLambdaConversion.pdf)

[Curry, H.B., and Feys, R. 1958. Combinatory Logic, Vol. 1. North-Holland.](Curry58_CombinatoryLogic.djvu)

Kennaway, J.R. 1984. An Outline of Some Results of Staples on Optimal Reduction Orders in Replacement Systems. CSA/19/1984, School of information Systems, University of East Anglia. March.

[Levy, J.J. 1980. Optimal reductions in the lambda calculus. In Essays on Combinatory Logic, pp.159-92. Hindley and Seldin (editors). Academic Press.](80curry.pdf)

[Rosser, J.B. 1982. Highlights of the history of the lambda calculus. Proceedings of the ACM Symposium on Lisp and Functional Programming, Pittsburgh, pp. 216-25. August.](rosser82.pdf)

[Schonfinkel, M. 1924. Uber die Bausteine der mathematischen Logik. Mathematische Annalen, Vol. 92, pp. 305-16.](Schonfinkel24-OnTheBuildingBlocksOfMathematicalLogic.pdf)

[Scott, D. 1981. Lectures on a Mathematical Theory of Computation. PRG-19. Programming Research Group, Oxford. May.](DanaScott82_LecturesOnAMathematicalTheoryOfComputation.pdf)

[Staples, J. 1980a. Computation on graph-like expressions. Theoretical Computer Science. Vol. 10, pp. 171-85.](staples1980a_ComputationsOnGraphLikeExpressions.pdf)

[Staples, J. 1980b. Optimal evaluations of graph-like expressions. Theoretical Computer Science. Vol. 10, pp. 297-316.](staples1980b_OptimalEvaluationOnGraphLikeExpressions.pdf)

[Staples, J. 1980c. Speeding up subtree replacement systems. Theoretical Computer Science, Vol. 11, pp. 39-47.](staples1980c_SpeedingUpSubtreeReplacementSystems.pdf)

[Stoy, J.E. 1981. Denotational Semantics. MIT Press.](stoy81_DenotationalSemantics.djvu)

Welch,P. 1975. Some Notes on the Martin-Lof Proof of the Church Rosser Theorem as Rediscovered by Park. Computer Lab., University of Kent. October.

# CH3

## References

[Gordon, M.J.C. 1979. The Denotational Description of Programming Languages. Springer Verlag.](gordon79_DenotationalDescriptionOfProgrammingLanguages.djvu)

[Turner, D.A. 1985. Miranda — a non-strict functional language with polymorphic types. In Conference on Functional Programming Languages and Computer Architecture, Nancy, pp. 1-16, Jouannaud(editor). LNCS 201. Springer Verlag.](turner85_Miranda.pdf)

# CH4

## References

[Burstall, R.M. 1969. Proving properties of programs by structural induction. The Computer Journal. Vol. 12, No. 1, pp. 41-8.](Burstall69_ProvingPropertiesOfProgramsByStructuralInduction)

Burstall, R.M. 1977. Design considerations for a functional programming language. In Proceedings Infotech State of the Art Conference, Copenhagen, pp. 54-7.

[Burstall, R.M., and Darlington, J. 1977. A transformation system for developing recursive programs. Journal of the ACM.Vol. 24, No. 1, pp. 44-67.](Burstall77_ATransformationSystemForDevelopingRecursivePrograms.pdf)

[Burstall, R.M., and Gognen, J.A. 1982. Algebras, Theories, and Freeness: An Introduction for Computer Scientists. Report CSR-101-82, Dept of Computer Science, University of Edinburgh. February.](Burstall82_AlgebrasTheoriesAndFreeness_AnIntroductionForComputerScientists.pdf)

[Landin, P.J. 1966. The next 700 programming langnages. Communications of the ACM.Vol. 9, No. 3, pp. 157-64.](Landin66.pdf)

Turner, D.A., 1981.. Aspects of the implementation of programming languages. D.Phil. thesis, University of Oxford. February.

Wadler, P. 1985. A Splitting Headache— and Its Cure. Programming Research Group, Oxford. January.

# CH5

## References

[Augustsson, L. 1985. Compiling pattern matching. In Conference on Functional Programming Languages and Computer Architecture, Nancy, pp. 368-81. Jouannaud(editor), LNCS 201. Springer Verlag.](Augustsson85_CompilingPatternMatching.pdf)

[Barrett, G., and Wadler, P. 1986. Derivation of a Pattern-matching Compiler. Programming Research Group, Oxford.](Barret86_DerivationOfAPatternMatchingCompiler.pdf)

[Burstall, R.M., MacQueen, D.B., and Sanella, D.T. 1980. HOPE: an experimental applicative language. In Proceedings of the ACM Lisp Conference. August.](burstall80_Hope.pdf)

[Hoffmann, C.M., and O’Donnell, M.J. 1983. Implementation of an interpreter for abstract equations. In 10th ACM Symposium on Principles of Programming Languages, pp. 111-21. ACM.](Hoffman83_ImplementationOfAnInterpreterForAbstractEquations.pdf)

Huet, G., and Levy, J.J. 1979. Computations in Non-ambiguous Linear Term Rewriting Systems. INRIA technical report 359.

# CH6

## References

[Aho, A.V., Hopcroft, J.E., and Ullman, D. 1974. The Design and Analysis of Computer Algorithms, pp. 189-95. Addison Wesley.](Aho74_TheDesignAndAnalysisOfComputerAlgorithms.pdf)

[Aho, A.V., Hopcroft, J.E., and Ullman, D. 1983a. Data Structures and Algorithms, pp. 222-6. Addison Wesley.](Aho85_DataStructuresAndAlgorithms.djvu)

[Aho, A.V., Hopcroft, J.E., and Ullman, D. 1983b. Data Structures and Algorithms, pp. 221-2. Addison Wesley.](Aho85_DataStructuresAndAlgorithms.djvu)

[Dijkstra, E.W. 1976. A Discipline of Programming, pp. 192-200. Prentice Hall.](Dijkstra76_ADisciplineOfProgramming.pdf)

# CH7

## References

[Turner, D.A. 1982. Recursion equations as a programming language. In Functional Programming and Its Applications. Darlington et al. (editors). Cambridge University Press](turner82_RecursionEquations.pdf)

# CH8

## References

[Gaeck, P., and Black, M. (editors) 1970. Function and concept. In Translations from the Philosophical Writings of Gottlob Frege. Basil Blackwell.](Gaeck70_TranslationsFromThePhilosophicalWritingsOfGottlobFregeBasilBlackwell.pdf)

[Milner, R. 1978. A theory of type polymorphism in programming. Journal of Computer and System Science. Vol. 17, pp. 348-75.](Milner78_ATheoryOfTypePolymorphismInProgramming.pdf)

[Mycroft, A. 1984. Polymorphic type schemes and recursive definitions. In Proceedings of the International Symposium on Programming, Toulouse, pp. 217-39. LNCS 167. Springer Verlag.](Mosses74.pdf)

[Strachey, C. 1967. Fundamental concepts in programming languages. In Notes for the International Summer School in Computer Programming, Copenhagen.](strachey67_2000_FundamentalConceptsInProgrammingLanguages.pdf)

[Whitehead, A.N., and Russell, B.A.W. 1910-1913. Principia Mathematica, Volume One. Cambridge University Press.](WhiteheadRussell_PrincipaMathematica_Vol1.pdf)

[Whitehead, A.N., and Russell, B.A.W. 1910-1913. Principia Mathematica, Volume Two. Cambridge University Press.](WhiteheadRussell_PrincipaMathematica_Vol2.pdf)

[Whitehead, A.N., and Russell, B.A.W. 1910-1913. Principia Mathematica, Volume Three. Cambridge University Press.](WhiteheadRussell_PrincipaMathematica_Vol3.pdf)