# [THE IMPLEMENTATION OF FUNCTIONAL PROGRAMMING LANGUAGES - Simon L. Peyton Jones](SLPJ_READING/slpj-book-1987.pdf)

## CH1. Introduction

Implementing functional programming languages using _lazy graph reduction_

### 1

- Translation from high level functional language into Lambda Calculus
- Pattern Matching
- Type Checking

### 2

- Refinements and alternatives such as:
- Super Combinators
- Full laziness
- SK combinators

### 3

- G-machine

The functional programming languages predating 1987:

- SASL [Turner, 1976](SLPJ_READING/turner83_saslmanual.pdf)

- ML [Gorden et al., 1979](SLPJ_READING/Gordon79_LectureNotesInComputerScience.pdf)

- KRC [Turner, 1982](SLPJ_READING/turner82_RecursionEquations.pdf)

- Hope [Burstall et al., 1980](SLPJ_READING/burstall80_Hope.pdf)

- Ponder [Fairbairn, 1985](SLPJ_READING/Fairbairn85DesignAndImplementationOfASimpleTypedLanguageBasedOnTheLambdaCalculus.pdf)

- LML [Augustsson, 1984](SLPJ_READING/augustsson84_ACompilerForLazyML.pdf)

- Miranda [Turner, 1985](SLPJ_READING/turner85_Miranda.pdf)

- Orwell [Wadler, 1985](SLPJ_READING/Wadler85_orwell.pdf)

Those with non strict semmantics being SASL, KRC, Ponder, LML, Miranda and Orwell.

ML and Hope being the strict languages in this group.

A super set of Lambda Calculus called _enriched lambda calculus_ will be used to specifically allow a straightfoward translation of a Miranda program into an expression in the enriched lambda calculus. This will be shown in chapter 3.

### Graph Reduction

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

### CH1 References

[Abelson, H., and Sussman, G.J. 1985. Structure and Interpretation of Computer Programs. MIT Press.](SLPJ_READING/https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book.html)

[Augustsson, L. 1984. A compiler for lazy ML. Proceedings of the ACM Symposium on Lisp and Programming, Austin. August, pp. 218-27.](SLPJ_READING/augustsson84_ACompilerForLazyML.pdf)

[Burstall, R.M., MacQueen, D.B., and Sanella, D.T. 1980. Hope: an experimental applicative language. CSR-62-80. Department of Computer Science, University of Edinburgh. May.](SLPJ_READING/burstall80_Hope.pdf)

Darlington, J. 1984. Functional programming. In Distributed Computing. Duce (Editor). Academic Press. --NOTE Cannot find this online at all

[Fairbairn, J. 1985. Design and implementation of a simple typed language based on the lambda calculus. PhD thesis, TechnicalReport 75. University of Cambridge. May.](SLPJ_READING/Fairbairn75DesignAndImplementationOfASimpleTypedLanguageBasedOnTheLambdaCalculus.pdf)

Glaser, H., Hankin, C., and Till, D. 1984. Principles of Functional Programming. Prentice-Hall. --TODO scan once it arrives stateside.

[Gordon, M.J., Milner, A.J., and Wadsworth, C.P. 1979. Edinburgh LCF. LNCS 78. Springer Verlag.](SLPJ_READING/Gordon79_LectureNotesInComputerScience.pdf)

[Henderson,P. 1980. Functional Programming. Prentice-Hall.](SLPJ_READING/Henderson80_FunctionalProgramming.djvu)

[Turner, D.A. 1976. The SASL language manual. University of St Andrews. December.](SLPJ_READING/saslman.pdf)

[Turner, D.A. 1982. Recursion equations as a programming language. In Functional Programming and Its Applications, Darlington et al. (editors), pp. 1-28. Cambridge University Press.](SLPJ_READING/turner82_RecursionEquations.pdf)

[Turner, D.A. 1985. Miranda ??? a non-strict functional language with polymorphic types. In Conference on Functional Programming Languages and Computer Architecture, Nancy, pp. |-16. Jouannaud (editor), LNCS 201. Springer Verlag.](SLPJ_READING/turner85_Miranda.pdf)

[Wadler, P. 1985. Introduction to Orwell. Programming Research Group, University of Oxford.](SLPJ_READING/Wadler85_orwell.pdf)

## Part I - Compiling High Level Functional Languages

## Chapter 2 - The Lambda Calculus

The lambda calculus as an intermediate language has two things going for it:

Simplicity and Expressiveness

You only need to support a few constructs and it has simple semantics for reasoning about correctness of the implementation. It can also express all functional prgorams (and all computable functions) meaning we can implement other functional languages by translating them down to the lambda calculus.

### 2.1 The Syntax of the Lambda Calculus

A simple expression
```
(+ 4 5)
```

All function applications in the lambda calculus are written in the prefix form. A more complex example.

NOTE: The Vim digraph command for Lambda is \<ctrl>kl*
      See :digraphs for more.
```
(+ (* 5 6) (* 8 3))

-- A Haskell version of this being
(\x y -> x + y) ((\x y -> x * y) 5 6) ((\x y -> x * y) 8 3)

-- An even more verbose version being
(\x -> \y -> x + y) ((\x -> \y -> x * y) 5 6) ((\x -> \y -> x * y) 8 3)

-- Traditional
((??x.??y.x + y) ((??x.??y x * y) 5 6) ((??x.??y.x * y) 8 3))
```

Evaluation proceeds by repeatedly selecting a _reducible expression_ (or _redex_) and reducing it.

When there are several avalible redexes we have a choice of which one to reduce first.

### 2.2 Function Application and Currying

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

((??x.??y.x + y) 3) 4)

--In this situation the redex taken are
--?? reduction -- Function Application
((??y.3 + y) 4)

--?? reduction
(3 + 4)

--?? Reduction -- Calculation with predefined function on predefined data types
```

By expressing `(+ 3 4)` as `((+ 3) 4)` we can reason about lambda calculus abstractions (functions) taking only one argument. This process is known as _currying_, originally introduced by [Schonfinkel in 1924](SLPJ_READING/Schonfinkel24-OnTheBuildingBlocksOfMathematicalLogic.pdf) and extensively used by [Curry and Feys 1958](SLPJ_READING/Curry58_CombinatoryLogic.djvu).

#### [Schonfinkel 1924 - On the building blocks of mathematical logic](SLPJ_READING/Schonfinkel24-OnTheBuildingBlocksOfMathematicalLogic.pdf)

```
??a, a ??? b, a ??? b, a ??? b, a ??? b
```

Schonfinkel notes that these three connectives can be defined in terms of negation and any one of them. This was shown briefly by [Henry M. Sheffer in 1913](SLPJ_SUPPLEMENTARY/Sheffer13.pdf). Sheffer is the same person who discovered the Sheffer Stroke, which is also known as NAND, indepdently alongside Charles Sanders Peirce (who did so in 1880 but never published the findings).

Boaz Schuman has a [nice lecture on the functional completeness of the functional completeness of the Sheffer Stroke](https://www.youtube.com/watch?v=PRKXXJF9Mi0).

| Connective |???|???|???|
|  -         |  -   |-   |-    |
|a ??? b       |-|??(??a?????b) |??(a?????b)|
|a ??? b       |??(??a?????b)     |-|??a???b|
|a ??? b       |??(a?????b)    |??a???b|-|

XOR can also be implemented similarly.
| Connective |???|???|???|
| - | - | - | - |
| XOR | ??(??(a ??? ??b) ??? ??(??a ??? b)) | ??(??(??a ??? ??b)??? ??(a ??? b)) | ??((a ??? ??b) ??? ??(??a ??? b)) |

The connectives and logical not implemented with the Sheffer Stroke (denoted as \| ) look as such

| Connective | NAND |
| - | - |
| ??b | b \| b |
| a ??? b | (a \| b) \| (a \| b) |
| a ??? b | (a \| a) \| (b \| b) |
| a ??? b | (a \| (b \| b)) |
| a ??? b | (a \| (a \| b)) |
| a ??? b | ((a \| b) \| ((a \| a) \| (b \| b))) |

```Coq
(* Sheffer 1913 states that any of the logical connectives
  for Boolean algebra can be implemented with the other using not and the logical connective.*)

Require Import Bool.

Example andb_orb : forall a b : bool,
  (* a ??? b = ??(??a?????b) *)
  andb a b = negb(orb (negb a) (negb b)).
Proof.
  intros a b.
  destruct a.
  - simpl. rewrite -> negb_involutive. reflexivity.
  - auto.
Qed.

Search (negb (negb _)).

Example andb_implb : forall a b : bool,
  (* a ??? b = ??(a?????b) *)
  andb a b = negb (implb a (negb b)).
Proof.
  intros a b.
  destruct a.
  - simpl. rewrite -> negb_involutive. reflexivity.
  - auto.
Qed.

Example orb_andb : forall a b: bool,
  (* a ??? b = ??(??a?????b) *)
  orb a b = negb (andb (negb a) (negb b)).
Proof.
  intros a b.
  destruct a.
  destruct b.
  - auto.
  - auto.
  - simpl. rewrite -> negb_involutive. reflexivity.
Qed.

Example orb_implb : forall a b: bool,
  (* a ??? b = ??a???b *)
  orb a b = implb (negb a) b.
Proof.
  intros a b.
  destruct a.
  - auto.
  - auto.
Qed.

Example implb_andb : forall a b: bool,
  (* a ??? b ??(a?????b) *)
  implb a b = negb(andb a (negb b)).
Proof.
  intros a b.
  unfold implb.
  destruct a.
  - destruct b. auto. auto.
  - auto.
Qed.

Example implb_orb : forall a b : bool,
  (* a ??? b = ??a???b *)
  implb a b = orb (negb a) b.
Proof.
  intros a b.
  unfold implb.
  destruct a.
  - destruct b. auto. auto.
  - auto.
Qed.

Definition nandb (a: bool) (b: bool) : bool:=
  negb (andb a b).

Example nandb_tt : nandb true  true   = false. auto. Qed.
Example nandb_tf : nandb false true   = true. auto. Qed.
Example nandb_ft : nandb true  false  = true. auto. Qed.
Example nandb_ff : nandb false false  = true. auto. Qed.

Example xorb_andb : forall a b : bool,
(* (a ??? b) = ??(??(a ??? ??b) ??? ??(??a ??? b)) *)
  xorb a b = negb(andb (negb (andb a (negb b)))
                       (negb (andb (negb a) b))).
Proof.
  intros a b.
  unfold negb, xorb.
  destruct a.
  - destruct b. auto. auto.
  - destruct b. auto. auto.
Qed.

Example xorb_orb : forall a b : bool,
(* (a ??? b) = ??(??(??a ??? ??b)??? ??(a???b)) *)
  xorb a b = negb(orb (negb (orb (negb a) (negb b)))
                      (negb (orb a b))).
Proof.
  intros a b.
  unfold negb, xorb.
  destruct a.
  - destruct b. auto. auto.
  - destruct b. auto. auto.
Qed.

Example xorb_implb : forall a b : bool,
(* (a ??? b) = ??((a ??? ??b) ??? ??(??a ??? b)) *)
  xorb a b = negb ( implb (implb a (negb b))
                          (negb (implb (negb a) (b)))).
Proof.
  intros a b.
  unfold negb, implb, xorb.
  destruct a.
  - destruct b. auto. auto.
  - destruct b. auto. auto.
Qed.

Example nandb_negb : forall b : bool,
(* ??b = b | b *)
  negb b = nandb b b.
Proof.
  unfold negb.
  destruct b.
  auto. auto.
Qed.

Example nandb_andb : forall a b : bool,
(* a ??? b = (a | b) | (a | b) *)
  andb a b = nandb (nandb a b) (nandb a b).
Proof.
  intros a b.
  unfold nandb, andb, negb.
  destruct a.
  - destruct b. auto. auto.
  - auto.
Qed.

Example nandb_orb : forall a b : bool,
(* a ??? b = (a | a) | (b | b) *)
  orb a b = nandb (nandb a a) (nandb b b).
Proof.
  intros a b.
  unfold nandb, andb, negb, orb.
  destruct a.
  - auto.
  - destruct b. auto. auto.
Qed.

Example nandb_implb1 : forall a b : bool,
(* a ??? b = (a | (b | b)) *)
  implb a b = nandb a (nandb b b).
Proof.
  intros a b.
  unfold nandb, implb, negb.
  destruct a.
  - destruct b. auto. auto.
  - auto.
Qed.

Example nandb_implb2 : forall a b : bool,
(* a ??? b = (a | (a | b)) *)
  implb a b = nandb a (nandb a b).
Proof.
  intros a b.
  unfold nandb, implb, negb.
  destruct a.
  - destruct b. auto. auto.
  - auto.
Qed.

Example nandb_eqb : forall a b : bool,
(*a <-> b = ((a | b) | ((a | a) | (b | b))) *)
  eqb a b = nandb (nandb a b) (nandb (nandb a a) (nandb b b)).
Proof.
  intros a b.
  unfold eqb, nandb, negb, andb.
  destruct a.
  - destruct b. auto. auto.
  - destruct b. auto. auto.
Qed.
```

--TODO SLPJ PAGE 10

### [Summary of reduction rules](Huch_LambdaCalculusNotes.pdf#page=6)

- ??-reduction: Renaming of Parameters

- ??-reduction: Function Application

- ??-reduction: Elimination of redundant ??-abstractions

- ??-reduction: Calculation with predefined function on predefined data types

### CH2 References

[Barendregt, H.P. 1984. The Lambda Calculus???Its Syntax and Semantics, 2nd edition. North-Holland.](SLPJ_READING/Barendregt80_LambdaCalculus.pdf)

[Church, A. 1941. The Calculi of Lambda Conversion. Princeton University Press.](SLPJ_READING/Church41_TheCalculiOfLambdaConversion.pdf)

[Curry, H.B., and Feys, R. 1958. Combinatory Logic, Vol. 1. North-Holland.](SLPJ_READING/Curry58_CombinatoryLogic.djvu)

Kennaway, J.R. 1984. An Outline of Some Results of Staples on Optimal Reduction Orders in Replacement Systems. CSA/19/1984, School of information Systems, University of East Anglia. March.

[Levy, J.J. 1980. Optimal reductions in the lambda calculus. In Essays on Combinatory Logic, pp.159-92. Hindley and Seldin (editors). Academic Press.](SLPJ_READING/80curry.pdf)

[Rosser, J.B. 1982. Highlights of the history of the lambda calculus. Proceedings of the ACM Symposium on Lisp and Functional Programming, Pittsburgh, pp. 216-25. August.](SLPJ_READING/rosser82.pdf)

[Schonfinkel, M. 1924. Uber die Bausteine der mathematischen Logik. Mathematische Annalen, Vol. 92, pp. 305-16.](SLPJ_READING/Schonfinkel24-OnTheBuildingBlocksOfMathematicalLogic.pdf)

[Scott, D. 1981. Lectures on a Mathematical Theory of Computation. PRG-19. Programming Research Group, Oxford. May.](SLPJ_READING/DanaScott82_LecturesOnAMathematicalTheoryOfComputation.pdf)

[Staples, J. 1980a. Computation on graph-like expressions. Theoretical Computer Science. Vol. 10, pp. 171-85.](SLPJ_READING/staples1980a_ComputationsOnGraphLikeExpressions.pdf)

[Staples, J. 1980b. Optimal evaluations of graph-like expressions. Theoretical Computer Science. Vol. 10, pp. 297-316.](SLPJ_READING/staples1980b_OptimalEvaluationOnGraphLikeExpressions.pdf)

[Staples, J. 1980c. Speeding up subtree replacement systems. Theoretical Computer Science, Vol. 11, pp. 39-47.](SLPJ_READING/staples1980c_SpeedingUpSubtreeReplacementSystems.pdf)

[Stoy, J.E. 1981. Denotational Semantics. MIT Press.](SLPJ_READING/stoy81_DenotationalSemantics.djvu)

Welch,P. 1975. Some Notes on the Martin-Lof Proof of the Church Rosser Theorem as Rediscovered by Park. Computer Lab., University of Kent. October.

## CH3

### CH3 References

[Gordon, M.J.C. 1979. The Denotational Description of Programming Languages. Springer Verlag.](SLPJ_READING/gordon79_DenotationalDescriptionOfProgrammingLanguages.djvu)

[Turner, D.A. 1985. Miranda ??? a non-strict functional language with polymorphic types. In Conference on Functional Programming Languages and Computer Architecture, Nancy, pp. 1-16, Jouannaud(editor). LNCS 201. Springer Verlag.](SLPJ_READING/turner85_Miranda.pdf)

## CH4

### CH4 References

[Burstall, R.M. 1969. Proving properties of programs by structural induction. The Computer Journal. Vol. 12, No. 1, pp. 41-8.](SLPJ_READING/Burstall69_ProvingPropertiesOfProgramsByStructuralInduction)

Burstall, R.M. 1977. Design considerations for a functional programming language. In Proceedings Infotech State of the Art Conference, Copenhagen, pp. 54-7.

[Burstall, R.M., and Darlington, J. 1977. A transformation system for developing recursive programs. Journal of the ACM.Vol. 24, No. 1, pp. 44-67.](SLPJ_READING/Burstall77_ATransformationSystemForDevelopingRecursivePrograms.pdf)

[Burstall, R.M., and Gognen, J.A. 1982. Algebras, Theories, and Freeness: An Introduction for Computer Scientists. Report CSR-101-82, Dept of Computer Science, University of Edinburgh. February.](SLPJ_READING/Burstall82_AlgebrasTheoriesAndFreeness_AnIntroductionForComputerScientists.pdf)

[Landin, P.J. 1966. The next 700 programming langnages. Communications of the ACM.Vol. 9, No. 3, pp. 157-64.](SLPJ_READING/Landin66.pdf)

Turner, D.A., 1981.. Aspects of the implementation of programming languages. D.Phil. thesis, University of Oxford. February.

Wadler, P. 1985. A Splitting Headache??? and Its Cure. Programming Research Group, Oxford. January.

## CH5

### References

[Augustsson, L. 1985. Compiling pattern matching. In Conference on Functional Programming Languages and Computer Architecture, Nancy, pp. 368-81. Jouannaud(editor), LNCS 201. Springer Verlag.](SLPJ_READING/Augustsson85_CompilingPatternMatching.pdf)

[Barrett, G., and Wadler, P. 1986. Derivation of a Pattern-matching Compiler. Programming Research Group, Oxford.](SLPJ_READING/Barret86_DerivationOfAPatternMatchingCompiler.pdf)

[Burstall, R.M., MacQueen, D.B., and Sanella, D.T. 1980. HOPE: an experimental applicative language. In Proceedings of the ACM Lisp Conference. August.](SLPJ_READING/burstall80_Hope.pdf)

[Hoffmann, C.M., and O???Donnell, M.J. 1983. Implementation of an interpreter for abstract equations. In 10th ACM Symposium on Principles of Programming Languages, pp. 111-21. ACM.](SLPJ_READING/Hoffman83_ImplementationOfAnInterpreterForAbstractEquations.pdf)

Huet, G., and Levy, J.J. 1979. Computations in Non-ambiguous Linear Term Rewriting Systems. INRIA technical report 359.

## CH6

### CH6 References

[Aho, A.V., Hopcroft, J.E., and Ullman, D. 1974. The Design and Analysis of Computer Algorithms, pp. 189-95. Addison Wesley.](SLPJ_READING/Aho74_TheDesignAndAnalysisOfComputerAlgorithms.pdf)

[Aho, A.V., Hopcroft, J.E., and Ullman, D. 1983a. Data Structures and Algorithms, pp. 222-6. Addison Wesley.](SLPJ_READING/Aho85_DataStructuresAndAlgorithms.djvu)

[Aho, A.V., Hopcroft, J.E., and Ullman, D. 1983b. Data Structures and Algorithms, pp. 221-2. Addison Wesley.](SLPJ_READING/Aho85_DataStructuresAndAlgorithms.djvu)

[Dijkstra, E.W. 1976. A Discipline of Programming, pp. 192-200. Prentice Hall.](SLPJ_READING/Dijkstra76_ADisciplineOfProgramming.pdf)

## CH7

### CH7 References

[Turner, D.A. 1982. Recursion equations as a programming language. In Functional Programming and Its Applications. Darlington et al. (editors). Cambridge University Press](SLPJ_READING/turner82_RecursionEquations.pdf)

## CH8

### CH8 References

[Gaeck, P., and Black, M. (editors) 1970. Function and concept. In Translations from the Philosophical Writings of Gottlob Frege. Basil Blackwell.](SLPJ_READING/Gaeck70_TranslationsFromThePhilosophicalWritingsOfGottlobFregeBasilBlackwell.pdf)

[Milner, R. 1978. A theory of type polymorphism in programming. Journal of Computer and System Science. Vol. 17, pp. 348-75.](SLPJ_READING/Milner78_ATheoryOfTypePolymorphismInProgramming.pdf)

[Mycroft, A. 1984. Polymorphic type schemes and recursive definitions. In Proceedings of the International Symposium on Programming, Toulouse, pp. 217-39. LNCS 167. Springer Verlag.](SLPJ_READING/Mosses74.pdf)

[Strachey, C. 1967. Fundamental concepts in programming languages. In Notes for the International Summer School in Computer Programming, Copenhagen.](SLPJ_READING/strachey67_2000_FundamentalConceptsInProgrammingLanguages.pdf)

[Whitehead, A.N., and Russell, B.A.W. 1910-1913. Principia Mathematica, Volume One. Cambridge University Press.](SLPJ_READING/WhiteheadRussell_PrincipaMathematica_Vol1.pdf)

[Whitehead, A.N., and Russell, B.A.W. 1910-1913. Principia Mathematica, Volume Two. Cambridge University Press.](SLPJ_READING/WhiteheadRussell_PrincipaMathematica_Vol2.pdf)

[Whitehead, A.N., and Russell, B.A.W. 1910-1913. Principia Mathematica, Volume Three. Cambridge University Press.](SLPJ_READING/WhiteheadRussell_PrincipaMathematica_Vol3.pdf)

## CH9

[Damas-Hindley-Milner](SLPJ_READING/http://dev.stephendiehl.com/fun/006_hindley_milner.html)

### CH9 References

[Damas,L. 1985.Type Assignment in Programming Languages. CST-33-35. Department of Computer Science, University of Edinburgh. April.](SLPJ_READING/Damas84_TypeAssignmentInProgrammingLanguages.pdf)

[Robinson, J.A. 1965. A machine-oriented logic based on the resolution principle. JournaloftheACM.Vol. 12,no. 1, pp. 23-41.](SLPJ_READING/robinson65_AMachineOrientedLogicBasedOnTheResolutionPrinciple.pdf)

[Wadler, P. 1985. How to replace failure by a list of successes. In Conference on Functional Programming Languages and Computer Architecture, Nancy. Jouannaud(editor). LNCS 201. Springer Verlag.](SLPJ_READING/Jouannaud85_ConferenceOnFunctionalProgrammingLanguagesAndComputerArchitecture.djvu)

## Part II - Graph Reduction

## CH10

### CH10 References

[Clarke, T.J.W., Gladstone, P.J.S., Maclean, C., and Norman, A.C. 1980. SKIM ??? The SKI reduction machine. Proceedings of the ACM Lisp Conference, Stanford, Calif. 95044.](SLPJ_READING/clarke80_TheSKIReductionMachine.pdf)

[Richards, H. 1985. An Overview of Burroughs NORMA. Austin Research Center, Burroughs Corp., Austin, Texas. January](SLPJ_READING/scheevel86_NORMA_AGraphReductionProcessor.pdf)

## CH11

### CH11 References

[Abelson, H., and Sussman, G.J. 1985. Structure and Interpretation of Computer Programs. MITPress.](SLPJ_READING/sussman85_SICP.pdf)

Arvind, Kathail, V., and Pingali, K. 1984. Sharing of Computation in Functional Language Implementations. Laboratory for Computer Science, MIT.July.

[Barendregt, H.P., Kennaway, J.R., Klop, J.W., and Sleep, M.R. 1986. Needed reduction and spine strategies for the lambda calculus. Report CS-R8621. Centre for Mathematics and Computer Science, Amsterdam. May.](SLPJ_READING/barendregt86_NeededReductionAndSpineStrategiesForTheLambdaCalculus.pdf)

[Henderson, P. 1980. Functional Programming ??? Application and Implementation. Prentice-Hall.](SLPJ_READING/Henderson80_FunctionalProgramming.djvu)

[Peyton Jones, S.L. 1986. Functional programming languages as a software engineering tool. In Software Engineering - The Critical Decade. Ince (editor). Peter Peregrinus.](SLPJ_READING/spj86_FunctionalProgrammingLanguagesAsASoftwareEngineeringTool.pdf)

[Scheevel, M. 1986. Norma: a graph reduction processor. In Proceedings of the ACM Conference on Lisp and Functional Programming, Cambridge, Mass., pp. 212-19. August.](SLPJ_READING/scheevel86_NORMA_AGraphReductionProcessor.pdf)

[Schorr, H., and Waite, W. 1967. An efficient machine independent procedure for garbage collection. Communications ofthe ACM. Vol.10, no. 8, pp. 501-6.](SLPJ_READING/schorr67_AnEfficientMachineIndependentProcedureForGarbageCollection.pdf)

[Stoye, W.R., Clarke, T.J.W., and Norman, A.C. 1984. Some practical methods for rapid combinator reduction. In Proceedings ofthe ACM Symposium on Lisp and Functional Programming, Austin, pp. 159-66. August.](SLPJ_READING/stoye84_SomePracticalMethodsForRapidCombinatorReduction.pdf)

[Turner, D.A. 1983. The SASL Language Manual. University of Kent. November.](SLPJ_READING/turner83_saslmanual.pdf)

Watson, P., Watson, I., and Woods, V. 1986. A Model of Computation for the Parallel Evaluation of Functional Languages. PMP/MU/PW/000001. Department of ComputerScience, University of Manchester. February.

## CH12

### CH12 References

[Hughes, R.J.M. 1985. Lazy memo functions. In Proceedings of the IFIP Conference on Functional Programming Languages and Computer Architecture, Nancy, pp. 129-46, Jouannaud(editor). LNCS 201. Springer Verlag. September.](SLPJ_READING/Hughes85_LazyMemoFunctions.pdf)

Mago, G.A. 1980. A cellular computer architecture for functional programming. IEEE Computer Society COMPCON,pp.179-87.

Wadsworth, C.P. 1971. Semantics and pragmatics of the lambda calculus, Chapter4. PhD thesis, Oxford.

## CH13

### CH13 References

[Barendregt, H.P. 1984. The Lambda Calculus: Its Syntax and Semantics, 2nd edition,p.24. North-Holland.](SLPJ_READING/SLPJ_READING\Barendregt80_LambdaCalculus.pdf)

[Curry, H.B., and Feys, R. 1958. Combinatory Logic, Vol. 1. North-Holland.](SLPJ_READING/Curry58_CombinatoryLogic.djvu)

[De Bruijn, N.G. 1972. Lambda calculus notation with nameless dummies. Indagationes Mathematicae. Vol. 34, pp. 381-92.](SLPJ_READING/SLPJ_READING\DeBruijn72_LambdaCalculusNotationWithNamelessDummies.pdf)

[Henderson, P. 1980. Functional Programming: application and implementation. Prentice-Hall.](SLPJ_READING/SLPJ_READING\Henderson80_FunctionalProgramming.djvu)

[Hoffman, C.M., and O'Donnell, M.J. 1982. Programming with equations. ???ACM TOPLAS.Vol. 4, no. 1, pp. 83-112.](SLPJ_READING/hoffman1982_ProgrammingWithEquations.pdf)

[Hughes, R.J.M. 1984. The design and implementation of programming languages. PhDthesis, PRG-40, Programming Research Group, Oxford. September.](SLPJ_READING/Hughes84_TheDesignAndImplementationOfProgrammingLanguages.pdf)

[Johnsson, T. 1984. Efficient compilation of lazy evaluation. In Proceeding of the ACM Symposium on Compiler Construction, Montreal, pp. 58-69. June.](SLPJ_READING/johnsson2004_84_EfficientCompilationOfLazyEvaluation.pdf)

[Johnsson, T. 1985. Lambdalifting: transforming programs to recursive equations. In Conference on Functional Programming Languages and Computer Architecture, Nancy. Jouannaud (editor). LNCS 201. Springer Verlag.](SLPJ_READING/johnsson85_LambdaLiftingTransformingProgramsToRecursiveEquations.pdf)

[Keller, R.M. 1985. Distributed graph reduction from first principles. Department of Computer Science, University of Utah.' Klop, J.W.1980. Combinatory reduction systems. PhD thesis, Mathematisch Centrum, Amsterdam.](SLPJ_READING/Keller85_DistributedComputationByGraphReduction.pdf)

[Landin, P.J. 1964. The mechanical evaluation of expressions. Computer Journal. Vol. 6, pp. 308-20.](SLPJ_READING/Landin64.pdf)

[O???Donnell, M.J. 1977. Computing in Systems Described by Equations. LNCS 58, Springer Verlag.](SLPJ_READING/ODonnell77_ComputingInSystesmDescribedByEquations.djvu)

## CH14

### CH14 References

[Hudak, P., and Kranz, D. 1984. A combinator based compiler for a functional language. In Proceedings of the 11th ACM Symposium on  Principles of Programming Languages, pp. 122-32. January.](SLPJ_READING/Hudak84_ACombinatorBasedCompilerForAFunctionalLanguage.pdf)

[Johnsson, T. 1985. Lambda-lifting??? transforming programsto recursive equations. In Conference on Functional Programming and Computer Architecture, Nancy, pp. 190-203. Jouannaud (editor). LNCS 201. Springer Verlag](SLPJ_READING/johnsson85_LambdaLiftingTransformingProgramsToRecursiveEquations.pdf)

## CH15

### CH15 References

[Fairbairn, J. 1985. The design and implementation of a simple typed language based on the lambda calculus, pp. 59-60. PhD thesis, Technical  Report 75. University of Cambridge. May.](SLPJ_READING/Fairbairn85DesignAndImplementationOfASimpleTypedLanguageBasedOnTheLambdaCalculus.pdf)

[Hudak, P., and Goldberg, B. 1985. Serial combinators. In Conference on Functional Programming Languages and Computer Architecture, Nancy, pp. 382-99. Jouannaud(editor). LNCS 201. Springer Verlag.](SLPJ_READING/Hudak85_SerialCombinators.pdf)

[Hughes, R.J.M. 1984. The design and implementation of programming languages. PhD thesis, PRG-40. Programming Research Group, Oxford. September.](SLPJ_READING/Hughes84_TheDesignAndImplementationOfProgrammingLanguages.pdf)

Wadsworth, C.P. 1971. Semantics and pragmatics of the lambda calculus, Chapter4. PhD thesis, Oxford.

## CH16

### CH16 References

[Burton, F.W. 1982. A linear space translation of functional programs to Turner combinators. /nformation Processing Letters. Vol. 14, no. 5, pp. 202-4.](SLPJ_READING/burton82_ALinearSpaceTranslationOfFunctionalProgramsToTurnerCOmbinators.pdf)

[Curry, H.B., and Feys, R. 1958. Combinatory Logic, Vol. 1. North-Holland. Hughes, R.J.M. 1984. The design and implementation of programming languages. PhD thesis, PRG-40. Programming Research Group, Oxford.](SLPJ_READING/Curry58_CombinatoryLogic.djvu)

[Joy, M.S., Rayward-Smith, V.J., and Burton, F.W. 1985. Efficient combinator code. Computer Languages. Vol. 10, no. 3/4, pp. 211-24.](SLPJ_READING/joy85_EfficientCombinatorCode.pdf)

Kennaway, J.R. 1982. The Complexity of a Translation of Lambda Calculus to Combinators. Department of Computer Science, University of East Anglia.

[Kennaway, J.R., and Sleep, M.R. 1982a. Director Strings as Combinators. Department ofComputer Science, University of East Anglia.](SLPJ_READING/kennaway88_DirectorStringsAsCombinators.pdf)

Kennaway, J.R., and Sleep, M.R. 1982b. Counting Director Strings. Department of Computer Science, University ofEast Anglia.

[Scheevel, M., 1986. Norma: a graph reduction processor. In Proceedings ofthe ACM Conference on Lisp and Functional Programming, Cambridge, Mass., pp. 212-19. August.](SLPJ_READING/scheevel86_NORMA_AGraphReductionProcessor.pdf)

[Stoye, W.R. 1983. The SKIM microprogrammer???s guide. Technical Report 31. University of Cambridge. October.](SLPJ_READING/stoy83_TheSKIMMicroprogrammersGuide.pdf)

[Stoye, W.R. 1985. The implementation of functional languages using custom hardware. PhD thesis, Computer Lab., University ofCambridge. May.](SLPJ_READING/stoye85_TheImplementationOfFunctionalLanguagesUsingCustomHardware.pdf)

[Turner, D.A. 1976. SASL Reference Manual. University of St Andrews.](SLPJ_READING/turner83_saslmanual.pdf)

[Turner, D.A. 1979a. A new implementation technique for applicative languages. Software??? Practice and Experience. Vol. 9, pp. 31-49.](SLPJ_READING/turner79_ANewImplementationTechniqueForApplicativeLanguages.pdf)

[Turner, D.A. 1979b. Another algorithm for bracket abstraction. Journal of Symbolic Logic. Vol. 44, no. 2, pp. 67-270](SLPJ_READING/turner79_AnotherAlgorithmForBracketAbstraction.pdf)

## CH17

### CH17 References

[Aerts, J.P.H. 1981. Implementing SASL without Garbage Collection. EUT-Report 81-WSK-05. Eindhoven Univ. of Technology. November.](SLPJ_READING/Aerts81_ImplementingSASLWithoutGarbageCollection.pdf)

[Baker, H. 1978. List processing in real time on a serial computer. Communications of the ACM.Vol. 21, no.4, pp. 280-94.](SLPJ_READING/Baker78_ListProcessingInRealTimeOnASerialComputer.pdf)

[Ben-Ari, M. 1984. Algorithms for on-the-fly garbage collection. ACM TOPLAS.Vol. 6, no. 3, pp. 333-44.](SLPJ_READING/ben-ari84_AlgorithmsForOnTheFlyGarbageCollection.pdf)

[Bobrow, D.G. 1980. Managing reentrant structures using reference-counts. ACM TOPLAS.Vol.2,no. 3, pp. 269-73.](SLPJ_READING/bobrow80_ManagingReentraceStructuresUsingReferenceCounts.pdf)

[Brownbridge, D. 1985. Cyclic reference-counting for combinator machines. in Proceedings ofthe IFIP Conference on Functional Programming and Computer Architecture, Nancy. September.](SLPJ_READING/Jouannaud85_ConferenceOnFunctionalProgrammingLanguagesAndComputerArchitecture.djvu)

[Cohen,J. 1981. Garbage collection of linked datastructures. ACMComputing Surveys. Vol. 13, no. 3, pp. 341-67.](SLPJ_READING/cohen81_GarbageCollectionOfLinkedDataStructures.pdf)

[Dijkstra, E.W., Lamport, L., Martin, A.J., Scholten, C.S., and Steffens, E.F.M. 1978. On-the-fly garbage collection???an exercise in cooperation. Communications of the ACM.Vol. 21, no. 11, pp. 966-75.](SLPJ_READING/Dijkstra78_OnTheFlyGarbageCollection.pdf)

[Hudak, P. 1983a. Distributed Graph Marking. Research report 268, Computer Science Dept, Yale. January.](SLPJ_READING/Hudak83_DistributedGraphMarking.pdf)

[Hudak, P. 1983b. Distributed task and memory management. in Proceedings of the ACM Symposium on Principles of Distributed Computing, pp. 277-89. August.](SLPJ_READING/Hudak83_DistributedTaskAndMemoryManagement.pdf)

Hughes, R.J.M. 1982. Reference-counting with Circular Structures in Virtual Memory Applicative Systems. Programming Research Group, Oxford.

[Hughes, R.J.M. 1985. A distributed garbage collection algorithm. in Proceedings of the IFIP Conf. on Functional Programming and Computer Architecture, Nancy, pp 256-72. Jouannaud(editor). LNCS 201, Springer Verlag. September.](SLPJ_READING/Jouannaud85_ConferenceOnFunctionalProgrammingLanguagesAndComputerArchitecture.djvu)

[Knuth, D. 1976. The Art of Computer Programming. Vol. 1, Section 2.5. Addison Wesley.](SLPJ_READING/TAOCP_Vol1_FundamentalAlgorithms.pdf)

[Kung, H.T., and Wong, S.W. 1977. An Efficient Parallel Garbage Collection System, and its Correctness Proof. Dept of Comp. Sci., Carnegie-Mellon Univ. September.](SLPJ_READING/Kung77_AnEfficientParallelGarbageCollectionSystemAndItsCorrectnessProof.pdf)

[Lieberman, H., and Hewitt, C. 1983. A real time garbage collector based on the lifetimes of objects. Communications of the ACM.Vol. 26, no. 6, pp. 419-29.](SLPJ_READING/Lieberman83_ARealTimeGarbageCollectorBasedOnTheLifetimesOfObjects.pdf)

Mohamed-Ali, K.A. 1984. Object oriented storage management and garbage collection in distributed processing systems. PhD Thesis, report TRITA-CS-8406. Royal Institute of Technology, Stockholm. December.

[Schorr, H., and Waite, W. 1967. An efficient machine independent procedure for garbage collection. Communications of the ACM.Vol. 10, no. 8, pp. 501-6.](SLPJ_READING/schorr67_AnEfficientMachineIndependentProcedureForGarbageCollection.pdf)

[Steele, G.L. 1975. Multiprocessing compactifying garbage collection. Communications of the ACM.Vol.18, no. 9, pp. 495-508.](SLPJ_READING/steele75_MultiprocessingCompactifyingGarbageCollection.pdf)

[Stoye, W.R., Clarke, T.J.W., and Norman, A.C. 1984. Some practical methods for rapid combinator reduction. In Proceedings of the ACM Symposium on Lisp and Functional Programming, Austin, pp. 159-66. August.](SLPJ_READING/stoye84_SomePracticalMethodsForRapidCombinatorReduction.pdf)

[Wadler, P. 1984. Listlessness is better than laziness: lazy evaluation and garbage collection at compile-time. In Proceedings ofthe ACM Symposium on Lisp and Functional Programming, Austin, pp. 45-52. August.](SLPJ_READING/Wadler84_ListlessnessIsBetterThanLaziness.pdf)

[Wise, D. 1985. Design for a multiprocessing heap with on-board reference-counting. In Functional Programming and Computer Architecture, Nancy, pp. 289-304. Jouannaud (editor). LNCS 201. Springer Verlag.](SLPJ_READING/Jouannaud85_ConferenceOnFunctionalProgrammingLanguagesAndComputerArchitecture.djvu)

[Wise, D.S., and Friedman, D.P. 1977. The one-bit reference-count. B/T. Vol. 17, no. 3, pp. 351-9.](SLPJ_READING/Wise77_The_one_bit_reference_count.pdf)

## CH18

### CH18 References

[Augustsson, L. 1984. A compiler for lazy ML. In Proceedings of the ACM Symposium on Lisp and Functional Programming, Austin, pp. 218-27, August.](SLPJ_READING/augustsson84_ACompilerForLazyML.pdf)

[Burstall, R.M., MacQueen, D.B., and Sanella, D.T. 1980. HOPE: an experimental applicative language. In Proceedings of the ACM Lisp Conference, pp. 136-43, August.](SLPJ_READING/burstall80_Hope.pdf)

[Clark, R. (editor) 1981. UCSD P-system and UCSD Pascal Users??? Manual, 2nd edition. Softech Microsystems, San Diego.](SLPJ_READING/Clark81_UCSD_PSystemAndUCSDPascalUsersManual.pdf)

Elworthy, D. 1985. Implementing a Ponder cross compiler for the SKIM processor. Dip. Comp.Sci. Dissertation, Computer Lab., Cambridge. July.

[Fairbairn, J. 1982. Ponder and its type system. Technical Report 31. Computer Lab., Cambridge. November.](SLPJ_READING/Fairbairn82_PonderAndItsTypeSystem.pdf)

[Fairbairn,J. 1985. Design and implementationofa simple typed language based on the lambda calculus. Technical Report 75. Computer Lab., Cambridge. May.](SLPJ_READING/Fairbairn85DesignAndImplementationOfASimpleTypedLanguageBasedOnTheLambdaCalculus.pdf)

[Fairbairn, J., and Wray, S.C. 1986. Code generation techniques for functional languages. In Proceedings of the ACM Conference on Lisp and Functional Programming, Boston, pp. 94-104, August.](SLPJ_READING/fairbairn86_CodeGenerationTechniquesForFunctionalLanguages.pdf)

Field, A. 1985. The Compilation of FP/M Programs into Conventional Machine Code. Dept Comp.Sci., Imperial College. June.

[Griss, M.L., and Hearn, A.C. 1981. A portable Lisp compiler. Software??? Practice and Experience. Vol. 11, pp. 541-605.](SLPJ_READING/griss81_APortableLispCompiler.pdf)

[Hudak, P., and Kranz, D. 1984. A combinator based compiler for a functional language. In Proceedings of the llth ACM Symposium on Principles of Programming Languages, pp. 122-32, January.](SLPJ_READING/Hudak84_ACombinatorBasedCompilerForAFunctionalLanguage.pdf)

[Johnsson, T. 1984. Efficient compilation of lazy evaluation. In Proceedings of the ACM Conference on Compiler Construction, Montreal, pp. 58-69, June.](SLPJ_READING/johnsson2004_84_EfficientCompilationOfLazyEvaluation.pdf)

Lester, D. 1985. The correctness of a G-machine compiler. MSc dissertation, Programming Research Group, Oxford. December.

[Rees, J.A., and Adams, N.1. 1982. T ???a dialect of LISP. In Proceedings of the ACM Symposium on Lisp and Functional Programming, pp. 114-22, August.](SLPJ_READING/rees82_T_ADialectOfLispOrLambdaTheUltimateSoftwareTool.pdf)

[Richards, M. 1971. The portability of the BCPL compiler. Software ??? Practice and Experience. Vol. 1, no. 2, pp. 135-46.](SLPJ_READING/richards71_ThePortabilityOfTheBCPLCompiler.pdf)

[Steele, G.L., and Sussman, G.J. 1978. The Revised Report on Scheme. Al Memo452, MIT. January.](SLPJ_READING/steele78_TheRevisedReportOnScheme.pdf)

## CH19

### CH19 References

Aho, A.V., and Ullman, J.D. 1977. Principles of Compiler Design. Addison Wesley. Bauer, F.L., and Eickel, J. 1976. Compiler Construction. Springer Verlag.

[Landin, P.J. 1964. The mechanical evaluation of expressions. ComputerJournal. Vol. 6, pp. 308-20.](SLPJ_READING/Landin64_TheMechanicalEvaluationOfExpressions.pdf)

[Wulf, W., Johnsson, R.K., Weinstock, C.B., Hobbs, S.O., and Geschke, C.M. 1975.The Design of an Optimising Compiler. Elsevier.](SLPJ_READING/Wulf77_TheDesignOfAnOptimizingCompiler.djvu)

## CH20

### CH20 References

[Augustsson, L. 1985. Compiling pattern matching. In Conference on Functional Programming Languages and Computer Architecture, Nancy. Jouannaud (editor). LNCS 201. Springer Verlag.](SLPJ_READING/Augustsson85_CompilingPatternMatching.pdf)

## CH21

### CH21 References

[Cardelli, L. 1983. The functional abstract machine. Polymorphism. Vol. 1, no.1.](SLPJ_READING/cardelli83_functional-abstract-machine.pdf)

[Cardelli, L. 1984. Compiling a functional language. In Proceedings of the ACM Symposium on Lisp and Functional Programming, Austin, pp. 208-17. August.](SLPJ_READING/cardelli84_CompilingAFunctionalLanguage.pdf)

[Steele, G.L. 1977. Lambda ??? the ultimate goto. Al Memo 443. MIT Artificial Intelligence Lab. October.](SLPJ_READING/steele77_LambdaTheUltimateGoto_AIM-443.pdf)

[D.A. 1979. A new implementation technique for applicative languages. Software??? Practice and Experience. Vol. 9, pp. 31-49.](SLPJ_READING/turner79_ANewImplementationTechniqueForApplicativeLanguages.pdf)

## CH22

### CH22 References

[Burn, G., Hankin, C.L., and Abramsky, S. 1985. Strictness analysis of higher order functions. Science of Computer Programming (to appear); also DoC 85/6, Dept Comp.Sci., Imperial College, London. April.](SLPJ_READING/Burn85_StrictnessAnalysisOfHigherOrderFunctions.pdf)

[Clack, C.D., and PeytonJones, S.L. 1985. Strictness Analysis ???a practical approach.In Conference on Functional Programming and Computer Architecture, Nancy. pp. 35-49. Jouannaud (editor). LNCS 201. Springer Verlag.](SLPJ_READING/Clack85_StrictnessAnalysisAPracticalApproach.pdf)

[Cousot, P., and Cousot, R. 1977. Abstract interpretation: a unified lattice model for static analysis of programs by construction or approximation of fixed points. In Proceedings of the 4th ACM Symposium on Principles of Programming Languages, Los Angeles, pp. 238-52.](SLPJ_READING/cousot77_AbstractInterpretation.pdf)

[Hudak, P., and Young, J. 1986. Higher order strictness analysis in untyped lambda calculus. In Proceedings of the 12th ACM Symposium on Principles of Programming Languages, pp. 97-109, January.](SLPJ_READING/Hudak86_HigherOrderStrictnessAnalysisInUntypedLambdaCalculus.pdf)

[Hughes, R.J.M. 1985. Strictness Detection in Non-flat Domains. Programming Research Group, Oxford. August.](SLPJ_READING/Hughes85_StrictnessDectionInNonFlatDomains.pdf)

[Mycroft, A. 1981. Abstract interpretation and optimising transformations for applicative  programs. PhD thesis, Dept Computer Science, University of Edinburgh.](SLPJ_READING/Mycroft81_AbstractInterpretationAndOptimisingTransformationsForApplicativePrograms.pdf)

[Stoy, J.E. 1981. Denotational Semantics. MIT Press.](SLPJ_READING/stoy81_DenotationalSemantics.djvu)

[Wadler, P. 1984. Listlessness is better than laziness: lazy evaluation and garbage collection at compile-time. In Proceedings ofthe ACM Symposium on Lisp and Functional Programming, Austin, Texas. August.](SLPJ_READING/Wadler84_ListlessnessIsBetterThanLaziness.pdf)

[Wadler, P. 1985a. Strictness Analysis on Non-flat Domains. Programming Research Group, Oxford. November.](SLPJ_READING/Wadler85_StrictNonFlat.pdf)

[Wadler, P. 1985b. Listiessness is better than laziness 11: composing listless functions. In Programs as Data Objects, Ganzinger, H., and Jones, N.D. (editors). LNCS 217. Springer Verlag.](SLPJ_READING/Wadler86_ListlessnessII.pdf)

[Wray, S.C. 1986. Implementation and programming techniques for functional languages. PhD thesis, University of Cambridge. January.](SLPJ_READING/Wray86_ImplementationAndProgrammingTechniquesForFunctionalLanguages.pdf)

## CH23

### CH23 References

[Backus, J. 1978. Can programming be liberated from the von Neumann style? A functional style  and its algebra of programs. Communications ofthe ACM. Vol. 21, no. 8, pp. 613-41.](SLPJ_READING/Backus78_CanProgrammingBeLiberatedFromTheVonNeumannStyle.pdf)

Hughes, R.J.M. 1984. Parallel Functional Programs use Less Space. Programming
Research Group, Oxford.

Meira, S.R.L. 1985. On the efficiency of applicative algorithms. PhD thesis, Computer
Laboratory, University of Kent, p. 36. March.

[Stoye, W. 1985. The implementation of functional languages using custom hardware. PhD thesis, Computer Lab., University of Cambridge. May.](SLPJ_READING/stoye85_TheImplementationOfFunctionalLanguagesUsingCustomHardware.pdf)

[Turner, D.A. 1981. The semantic elegance of applicative languages. In Proceedings of the ACM Conference on Functional Languages and Computer Architecture, pp. 85-92. ACM.](SLPJ_READING/turner81_TheSemanticEleganceOfApplicativeLanguages.pdf)

## CH24

### CH24 References

Clack, C.D., and Peyton Jones, S.L. 1985. Generating Parallelism from Strictness Analysis. Internal Note 1679, Dept Comp. Sci., University College London. February.

[Clack, C.D., and Peyton Jones, S.L. 1986. The four-stroke reduction engine. In ACM Conference on Lisp and Functional Programming, Boston. pp. 220-32, August.](SLPJ_READING/clack86_TheFourStrokeReductionEngine.pdf)

Cripps, M.D., and Field, A.J. 1983, An Asynchronous Structure-independent Switching System with System-level Fault Tolerance. Dept Comp. Sci., Imperial College, London.

[Darlington, J., and Reeve, M. 1981. ALICE???a multiprocessor reduction machine for the parallel evaluation of applicative languages. In Proceedings of the ACM Symposium on Functional Languages and Computer Architecture, Portsmouth. pp. 65~???76, October.](SLPJ_READING/darlington81_ALICE_MultiprocessorReductionMachine.pdf)

Denning, P.J. 1972. On modeling program behavior. In Proceedings of the Spring Joint Computer Conference, 40, pp. 937-44. AFIPS Press.

[Goldberg, B., and Hudak,P. 1985. Serial combinators??? optimal grains of parallelism. In Functional Programming Languages and Computer Architecture. pp. 382-99. LNCS 201. Springer Verlag.](SLPJ_READING/Hudak85_SerialCombinators.pdf)

[Hankin, C.L., Burn, G.L., and Peyton Jones, S.L. 1986. An approach to safe parallel combinator reduction. European Symposium on Programming. Robinet, B., and Wilhelm,R.(editors), pp. 99-110. LNCS 213. Springer Verlag. March.](SLPJ_READING/Hankin86_A_Safe_Approach_to_Parallel_Combinator_Reduction_E.pdf)

[Hudak,P. 1984. Distributed Applicative Processing Systems. YALEU/DCS/TR-317. Dept Comp.Sci., Yale. May.](SLPJ_READING/Hudak84_DistributedApplicativeProcessingSystems.pdf)

Hudak,P, 1985. Functional Programming on Multiprocessor Architectures ??? Research in Progress, Dept Comp.Sci., Yale University. November.

[Hudak,P., and Goldberg, B. 1985a. Distributed execution of functional programs using serial combinators. IEEE Transactions on Computers. Vol. C-34, no. 10.](SLPJ_READING/hudak1985_DistributedExecutionOfFunctionalProgramsUsingSerialCombinators.pdf)

[Hudak,P., and Goldberg, B. 1985b. Serial combinators. In Conference on Functional Programming and Computer Architecture, Nancy. Jouannaud (editor). LNCS 201. Springer Verlag.](SLPJ_READING/Hudak85_SerialCombinators.pdf)

[Hudak, P., and Smith, L. 1985. Para-functional Programming ??? a Paradigm for Programming MultiprocessorSystems. YALEU/DCS/RR-390. Dept Comp.Sci., Yale University. June. Intel 1985. iPSC User???s Guide. Intel Corporation, Order Number 175455-003. October.](SLPJ_READING/hudak86_ParaFunctionalPrograming.pdf)

[Keller, R.M. 1985. Rediflow Architecture Prospectus, UUCS-85-105. Dept Comp. Sci., University of Utah. August.](SLPJ_READING/Keller85_Rediflow_architecture_prospectus.pdf)

[Keller, R.M., and Lin, F.C.H. 1984. Simulated performance of a reduction based multiprocessor. IEEE Computer. Vol. 17, no. 7, pp. 70-82.](SLPJ_READING/Keller84_SimulatedPerformanceOfAReductionBasedMultiprocessingSystem.pdf)

[Keller, R.M., Lindstrom, G., and Patil, S. 1979. A loosely-coupled applicative multi-processing system. In AFIPS Conference Proceedings, pp. 613-22, June.](SLPJ_READING/Keller85_DistributedComputationByGraphReduction.pdf)

[Peyton Jones, S.L. 1986. Using Futurebus in a fifth generation computer. Micro-processors and Microsystems. Vol. 10, no. 2.](SLPJ_READING/spj86_Futurebus.pdf)

[Peyton Jones, S.L., Clack, C.D., and Salkild, J. 1985. GRIP ??? a Parallel Graph Reduction Machine, Dept Comp.Sci., University College London. November.](SLPJ_READING/spj85_GRIP.pdf)

[Smith, A.J. 1982. Cache memories. ACM Computing Surveys. Vol. 14, no. 3, pp.473-530.](SLPJ_READING/smith82_CacheMemories.pdf)

Tighe, S. 1985. A Study ofthe Parallelism Inherent in Combinator Reduction. Parallel processing program, MCC, Austin, Texas. August.

## Appendix

### Appendix References

[Gordon, M.J., Milner, A.J., and Wadsworth, C.P. 1979. Edinburgh LCF. Springer Lecture Notes in Computer Science. Vol. 78.](SLPJ_READING/Gordon79_LectureNotesInComputerScience.pdf)

[Landin, P.J. 1966. The next 700 programming languages. Communications of the ACM.Vol.9, no. 3.](SLPJ_READING/Landin66.pdf)

[Milner, A.J. 1978. A theory of type polymorphism in programming. Journal of Computer and System Sciences. Vol. 17.](SLPJ_READING/Milner78_ATheoryOfTypePolymorphismInProgramming.pdf)

Richards, H. 1984. An overview of ARC SASL. SIGPLAN Notices. October.

[Thompson, S.J. 1986. Laws in Miranda. Proceedings of the 4th ACM International Conference on LISPand Functional Programming, Boston, Mass. August.](SLPJ_READING/thompson86_LawsInMiranda.pdf)

[Turner, D.A. 1976. SASL language manual. StAndrews University Technical Report. December.](SLPJ_READING/turner83_saslmanual.pdf)

[Turner, D.A. 1982. Recursion equations as a programming language. In Functional Programming and its Applications, Darlington et al. (editors). Cambridge University Press.](SLPJ_READING/turner82_RecursionEquations.pdf)

[Turner, D.A. 1985. Miranda: a non-strict functional language with polymorphic types. In Proceedings of the IFIP International Conference on Functional Programming Languages and Computer Architecture, Nancy. Springer Lecture Notes in ComputerScience. LNCS 201.](SLPJ_READING/turner85_Miranda.pdf)
