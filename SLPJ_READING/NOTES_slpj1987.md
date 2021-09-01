# THE IMPLEMENTATION OF FUNCTIONAL PROGRAMMING LANGUAGES - Simon L. Peyton Jones

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

- SASL [Turner, 1976](turner83_saslmanual.pdf)

- ML [Gorden et al., 1979](Gordon79_LectureNotesInComputerScience.pdf)

- KRC [Turner, 1982](turner82_RecursionEquations.pdf)

- Hope [Burstall et al., 1980](burstall80_Hope.pdf)

- Ponder [Fairbairn, 1985](Fairbairn85DesignAndImplementationOfASimpleTypedLanguageBasedOnTheLambdaCalculus.pdf)

- LML [Augustsson, 1984](augustsson84_ACompilerForLazyML.pdf)

- Miranda [Turner, 1985](turner85_Miranda.pdf)

- Orwell [Wadler, 1985](Wadler85_orwell.pdf)

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
((λx.λy.x + y) ((λx.λy x * y) 5 6) ((λx.λy.x * y) 8 3))
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

### CH2 References

[Barendregt, H.P. 1984. The Lambda Calculus—Its Syntax and Semantics, 2nd edition. North-Holland.](Barendregt80_LambdaCalculus.pdf)

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

## CH3

### CH3 References

[Gordon, M.J.C. 1979. The Denotational Description of Programming Languages. Springer Verlag.](gordon79_DenotationalDescriptionOfProgrammingLanguages.djvu)

[Turner, D.A. 1985. Miranda — a non-strict functional language with polymorphic types. In Conference on Functional Programming Languages and Computer Architecture, Nancy, pp. 1-16, Jouannaud(editor). LNCS 201. Springer Verlag.](turner85_Miranda.pdf)

## CH4

### CH4 References

[Burstall, R.M. 1969. Proving properties of programs by structural induction. The Computer Journal. Vol. 12, No. 1, pp. 41-8.](Burstall69_ProvingPropertiesOfProgramsByStructuralInduction)

Burstall, R.M. 1977. Design considerations for a functional programming language. In Proceedings Infotech State of the Art Conference, Copenhagen, pp. 54-7.

[Burstall, R.M., and Darlington, J. 1977. A transformation system for developing recursive programs. Journal of the ACM.Vol. 24, No. 1, pp. 44-67.](Burstall77_ATransformationSystemForDevelopingRecursivePrograms.pdf)

[Burstall, R.M., and Gognen, J.A. 1982. Algebras, Theories, and Freeness: An Introduction for Computer Scientists. Report CSR-101-82, Dept of Computer Science, University of Edinburgh. February.](Burstall82_AlgebrasTheoriesAndFreeness_AnIntroductionForComputerScientists.pdf)

[Landin, P.J. 1966. The next 700 programming langnages. Communications of the ACM.Vol. 9, No. 3, pp. 157-64.](Landin66.pdf)

Turner, D.A., 1981.. Aspects of the implementation of programming languages. D.Phil. thesis, University of Oxford. February.

Wadler, P. 1985. A Splitting Headache— and Its Cure. Programming Research Group, Oxford. January.

## CH5

### References

[Augustsson, L. 1985. Compiling pattern matching. In Conference on Functional Programming Languages and Computer Architecture, Nancy, pp. 368-81. Jouannaud(editor), LNCS 201. Springer Verlag.](Augustsson85_CompilingPatternMatching.pdf)

[Barrett, G., and Wadler, P. 1986. Derivation of a Pattern-matching Compiler. Programming Research Group, Oxford.](Barret86_DerivationOfAPatternMatchingCompiler.pdf)

[Burstall, R.M., MacQueen, D.B., and Sanella, D.T. 1980. HOPE: an experimental applicative language. In Proceedings of the ACM Lisp Conference. August.](burstall80_Hope.pdf)

[Hoffmann, C.M., and O’Donnell, M.J. 1983. Implementation of an interpreter for abstract equations. In 10th ACM Symposium on Principles of Programming Languages, pp. 111-21. ACM.](Hoffman83_ImplementationOfAnInterpreterForAbstractEquations.pdf)

Huet, G., and Levy, J.J. 1979. Computations in Non-ambiguous Linear Term Rewriting Systems. INRIA technical report 359.

## CH6

### CH6 References

[Aho, A.V., Hopcroft, J.E., and Ullman, D. 1974. The Design and Analysis of Computer Algorithms, pp. 189-95. Addison Wesley.](Aho74_TheDesignAndAnalysisOfComputerAlgorithms.pdf)

[Aho, A.V., Hopcroft, J.E., and Ullman, D. 1983a. Data Structures and Algorithms, pp. 222-6. Addison Wesley.](Aho85_DataStructuresAndAlgorithms.djvu)

[Aho, A.V., Hopcroft, J.E., and Ullman, D. 1983b. Data Structures and Algorithms, pp. 221-2. Addison Wesley.](Aho85_DataStructuresAndAlgorithms.djvu)

[Dijkstra, E.W. 1976. A Discipline of Programming, pp. 192-200. Prentice Hall.](Dijkstra76_ADisciplineOfProgramming.pdf)

## CH7

### CH7 References

[Turner, D.A. 1982. Recursion equations as a programming language. In Functional Programming and Its Applications. Darlington et al. (editors). Cambridge University Press](turner82_RecursionEquations.pdf)

## CH8

### CH8 References

[Gaeck, P., and Black, M. (editors) 1970. Function and concept. In Translations from the Philosophical Writings of Gottlob Frege. Basil Blackwell.](Gaeck70_TranslationsFromThePhilosophicalWritingsOfGottlobFregeBasilBlackwell.pdf)

[Milner, R. 1978. A theory of type polymorphism in programming. Journal of Computer and System Science. Vol. 17, pp. 348-75.](Milner78_ATheoryOfTypePolymorphismInProgramming.pdf)

[Mycroft, A. 1984. Polymorphic type schemes and recursive definitions. In Proceedings of the International Symposium on Programming, Toulouse, pp. 217-39. LNCS 167. Springer Verlag.](Mosses74.pdf)

[Strachey, C. 1967. Fundamental concepts in programming languages. In Notes for the International Summer School in Computer Programming, Copenhagen.](strachey67_2000_FundamentalConceptsInProgrammingLanguages.pdf)

[Whitehead, A.N., and Russell, B.A.W. 1910-1913. Principia Mathematica, Volume One. Cambridge University Press.](WhiteheadRussell_PrincipaMathematica_Vol1.pdf)

[Whitehead, A.N., and Russell, B.A.W. 1910-1913. Principia Mathematica, Volume Two. Cambridge University Press.](WhiteheadRussell_PrincipaMathematica_Vol2.pdf)

[Whitehead, A.N., and Russell, B.A.W. 1910-1913. Principia Mathematica, Volume Three. Cambridge University Press.](WhiteheadRussell_PrincipaMathematica_Vol3.pdf)

## CH9

[Damas-Hindley-Milner](http://dev.stephendiehl.com/fun/006_hindley_milner.html)

### CH9 References

[Damas,L. 1985.Type Assignment in Programming Languages. CST-33-35. Department of Computer Science, University of Edinburgh. April.](Damas84_TypeAssignmentInProgrammingLanguages.pdf)

[Robinson, J.A. 1965. A machine-oriented logic based on the resolution principle. JournaloftheACM.Vol. 12,no. 1, pp. 23-41.](robinson65_AMachineOrientedLogicBasedOnTheResolutionPrinciple.pdf)

[Wadler, P. 1985. How to replace failure by a list of successes. In Conference on Functional Programming Languages and Computer Architecture, Nancy. Jouannaud(editor). LNCS 201. Springer Verlag.](Jouannaud85_ConferenceOnFunctionalProgrammingLanguagesAndComputerArchitecture.djvu)

## Part II - Graph Reduction

## CH10

### CH10 References

[Clarke, T.J.W., Gladstone, P.J.S., Maclean, C., and Norman, A.C. 1980. SKIM — The SKI reduction machine. Proceedings of the ACM Lisp Conference, Stanford, Calif. 95044.](clarke80_TheSKIReductionMachine.pdf)

[Richards, H. 1985. An Overview of Burroughs NORMA. Austin Research Center, Burroughs Corp., Austin, Texas. January](scheevel86_NORMA_AGraphReductionProcessor.pdf)

## CH11

### CH11 References

[Abelson, H., and Sussman, G.J. 1985. Structure and Interpretation of Computer Programs. MITPress.](sussman85_SICP.pdf)

Arvind, Kathail, V., and Pingali, K. 1984. Sharing of Computation in Functional Language Implementations. Laboratory for Computer Science, MIT.July.

[Barendregt, H.P., Kennaway, J.R., Klop, J.W., and Sleep, M.R. 1986. Needed reduction and spine strategies for the lambda calculus. Report CS-R8621. Centre for Mathematics and Computer Science, Amsterdam. May.](barendregt86_NeededReductionAndSpineStrategiesForTheLambdaCalculus.pdf)

[Henderson, P. 1980. Functional Programming — Application and Implementation. Prentice-Hall.](Henderson80_FunctionalProgramming.djvu)

[Peyton Jones, S.L. 1986. Functional programming languages as a software engineering tool. In Software Engineering - The Critical Decade. Ince (editor). Peter Peregrinus.](spj86_FunctionalProgrammingLanguagesAsASoftwareEngineeringTool.pdf)

[Scheevel, M. 1986. Norma: a graph reduction processor. In Proceedings of the ACM Conference on Lisp and Functional Programming, Cambridge, Mass., pp. 212-19. August.](scheevel86_NORMA_AGraphReductionProcessor.pdf)

[Schorr, H., and Waite, W. 1967. An efficient machine independent procedure for garbage collection. Communications ofthe ACM. Vol.10, no. 8, pp. 501-6.](schorr67_AnEfficientMachineIndependentProcedureForGarbageCollection.pdf)

[Stoye, W.R., Clarke, T.J.W., and Norman, A.C. 1984. Some practical methods for rapid combinator reduction. In Proceedings ofthe ACM Symposium on Lisp and Functional Programming, Austin, pp. 159-66. August.](stoye84_SomePracticalMethodsForRapidCombinatorReduction.pdf)

[Turner, D.A. 1983. The SASL Language Manual. University of Kent. November.](turner83_saslmanual.pdf)

Watson, P., Watson, I., and Woods, V. 1986. A Model of Computation for the Parallel Evaluation of Functional Languages. PMP/MU/PW/000001. Department of ComputerScience, University of Manchester. February.

## CH12

### CH12 References

[Hughes, R.J.M. 1985. Lazy memo functions. In Proceedings of the IFIP Conference on Functional Programming Languages and Computer Architecture, Nancy, pp. 129-46, Jouannaud(editor). LNCS 201. Springer Verlag. September.](Hughes85_LazyMemoFunctions.pdf)

Mago, G.A. 1980. A cellular computer architecture for functional programming. IEEE Computer Society COMPCON,pp.179-87.

Wadsworth, C.P. 1971. Semantics and pragmatics of the lambda calculus, Chapter4. PhD thesis, Oxford.

## CH13

### CH13 References

[Barendregt, H.P. 1984. The Lambda Calculus: Its Syntax and Semantics, 2nd edition,p.24. North-Holland.](SLPJ_READING\Barendregt80_LambdaCalculus.pdf)

[Curry, H.B., and Feys, R. 1958. Combinatory Logic, Vol. 1. North-Holland.](Curry58_CombinatoryLogic.djvu)

[De Bruijn, N.G. 1972. Lambda calculus notation with nameless dummies. Indagationes Mathematicae. Vol. 34, pp. 381-92.](SLPJ_READING\DeBruijn72_LambdaCalculusNotationWithNamelessDummies.pdf)

[Henderson, P. 1980. Functional Programming: application and implementation. Prentice-Hall.](SLPJ_READING\Henderson80_FunctionalProgramming.djvu)

[Hoffman, C.M., and O'Donnell, M.J. 1982. Programming with equations. ‘ACM TOPLAS.Vol. 4, no. 1, pp. 83-112.](hoffman1982_ProgrammingWithEquations.pdf)

[Hughes, R.J.M. 1984. The design and implementation of programming languages. PhDthesis, PRG-40, Programming Research Group, Oxford. September.](Hughes84_TheDesignAndImplementationOfProgrammingLanguages.pdf)

[Johnsson, T. 1984. Efficient compilation of lazy evaluation. In Proceeding of the ACM Symposium on Compiler Construction, Montreal, pp. 58-69. June.](johnsson2004_84_EfficientCompilationOfLazyEvaluation.pdf)

[Johnsson, T. 1985. Lambdalifting: transforming programs to recursive equations. In Conference on Functional Programming Languages and Computer Architecture, Nancy. Jouannaud (editor). LNCS 201. Springer Verlag.](johnsson85_LambdaLiftingTransformingProgramsToRecursiveEquations.pdf)

[Keller, R.M. 1985. Distributed graph reduction from first principles. Department of Computer Science, University of Utah.' Klop, J.W.1980. Combinatory reduction systems. PhD thesis, Mathematisch Centrum, Amsterdam.](Keller85_DistributedComputationByGraphReduction.pdf)

[Landin, P.J. 1964. The mechanical evaluation of expressions. Computer Journal. Vol. 6, pp. 308-20.](Landin64.pdf)

[O’Donnell, M.J. 1977. Computing in Systems Described by Equations. LNCS 58, Springer Verlag.](ODonnell77_ComputingInSystesmDescribedByEquations.djvu)

## CH14

### CH14 References

[Hudak, P., and Kranz, D. 1984. A combinator based compiler for a functional language. In Proceedings of the 11th ACM Symposium on  Principles of Programming Languages, pp. 122-32. January.](Hudak84_ACombinatorBasedCompilerForAFunctionalLanguage.pdf)

[Johnsson, T. 1985. Lambda-lifting— transforming programsto recursive equations. In Conference on Functional Programming and Computer Architecture, Nancy, pp. 190-203. Jouannaud (editor). LNCS 201. Springer Verlag](johnsson85_LambdaLiftingTransformingProgramsToRecursiveEquations.pdf)

## CH15

### CH15 References

[Fairbairn, J. 1985. The design and implementation of a simple typed language based on the lambda calculus, pp. 59-60. PhD thesis, Technical  Report 75. University of Cambridge. May.](Fairbairn85DesignAndImplementationOfASimpleTypedLanguageBasedOnTheLambdaCalculus.pdf)

[Hudak, P., and Goldberg, B. 1985. Serial combinators. In Conference on Functional Programming Languages and Computer Architecture, Nancy, pp. 382-99. Jouannaud(editor). LNCS 201. Springer Verlag.](Hudak85_SerialCombinators.pdf)

[Hughes, R.J.M. 1984. The design and implementation of programming languages. PhD thesis, PRG-40. Programming Research Group, Oxford. September.](Hughes84_TheDesignAndImplementationOfProgrammingLanguages.pdf)

Wadsworth, C.P. 1971. Semantics and pragmatics of the lambda calculus, Chapter4. PhD thesis, Oxford.

## CH16

### CH16 References

[Burton, F.W. 1982. A linear space translation of functional programs to Turner combinators. /nformation Processing Letters. Vol. 14, no. 5, pp. 202-4.](burton82_ALinearSpaceTranslationOfFunctionalProgramsToTurnerCOmbinators.pdf)

[Curry, H.B., and Feys, R. 1958. Combinatory Logic, Vol. 1. North-Holland. Hughes, R.J.M. 1984. The design and implementation of programming languages. PhD thesis, PRG-40. Programming Research Group, Oxford.](Curry58_CombinatoryLogic.djvu)

[Joy, M.S., Rayward-Smith, V.J., and Burton, F.W. 1985. Efficient combinator code. Computer Languages. Vol. 10, no. 3/4, pp. 211-24.](joy85_EfficientCombinatorCode.pdf)

Kennaway, J.R. 1982. The Complexity of a Translation of Lambda Calculus to Combinators. Department of Computer Science, University of East Anglia.

[Kennaway, J.R., and Sleep, M.R. 1982a. Director Strings as Combinators. Department ofComputer Science, University of East Anglia.](kennaway88_DirectorStringsAsCombinators.pdf)

Kennaway, J.R., and Sleep, M.R. 1982b. Counting Director Strings. Department of Computer Science, University ofEast Anglia.

[Scheevel, M., 1986. Norma: a graph reduction processor. In Proceedings ofthe ACM Conference on Lisp and Functional Programming, Cambridge, Mass., pp. 212-19. August.](scheevel86_NORMA_AGraphReductionProcessor.pdf)

[Stoye, W.R. 1983. The SKIM microprogrammer’s guide. Technical Report 31. University of Cambridge. October.](stoy83_TheSKIMMicroprogrammersGuide.pdf)

[Stoye, W.R. 1985. The implementation of functional languages using custom hardware. PhD thesis, Computer Lab., University ofCambridge. May.](stoye85_TheImplementationOfFunctionalLanguagesUsingCustomHardware.pdf)

[Turner, D.A. 1976. SASL Reference Manual. University of St Andrews.](turner83_saslmanual.pdf)

[Turner, D.A. 1979a. A new implementation technique for applicative languages. Software— Practice and Experience. Vol. 9, pp. 31-49.](turner79_ANewImplementationTechniqueForApplicativeLanguages.pdf)

[Turner, D.A. 1979b. Another algorithm for bracket abstraction. Journal of Symbolic Logic. Vol. 44, no. 2, pp. 67-270](turner79_AnotherAlgorithmForBracketAbstraction.pdf)

## CH17

### CH17 References

[Aerts, J.P.H. 1981. Implementing SASL without Garbage Collection. EUT-Report 81-WSK-05. Eindhoven Univ. of Technology. November.](Aerts81_ImplementingSASLWithoutGarbageCollection.pdf)

[Baker, H. 1978. List processing in real time on a serial computer. Communications of the ACM.Vol. 21, no.4, pp. 280-94.](Baker78_ListProcessingInRealTimeOnASerialComputer.pdf)

[Ben-Ari, M. 1984. Algorithms for on-the-fly garbage collection. ACM TOPLAS.Vol. 6, no. 3, pp. 333-44.](ben-ari84_AlgorithmsForOnTheFlyGarbageCollection.pdf)

[Bobrow, D.G. 1980. Managing reentrant structures using reference-counts. ACM TOPLAS.Vol.2,no. 3, pp. 269-73.](bobrow80_ManagingReentraceStructuresUsingReferenceCounts.pdf)

[Brownbridge, D. 1985. Cyclic reference-counting for combinator machines. in Proceedings ofthe IFIP Conference on Functional Programming and Computer Architecture, Nancy. September.](Jouannaud85_ConferenceOnFunctionalProgrammingLanguagesAndComputerArchitecture.djvu)

[Cohen,J. 1981. Garbage collection of linked datastructures. ACMComputing Surveys. Vol. 13, no. 3, pp. 341-67.](cohen81_GarbageCollectionOfLinkedDataStructures.pdf)

[Dijkstra, E.W., Lamport, L., Martin, A.J., Scholten, C.S., and Steffens, E.F.M. 1978. On-the-fly garbage collection—an exercise in cooperation. Communications of the ACM.Vol. 21, no. 11, pp. 966-75.](Dijkstra78_OnTheFlyGarbageCollection.pdf)

[Hudak, P. 1983a. Distributed Graph Marking. Research report 268, Computer Science Dept, Yale. January.](Hudak83_DistributedGraphMarking.pdf)

[Hudak, P. 1983b. Distributed task and memory management. in Proceedings of the ACM Symposium on Principles of Distributed Computing, pp. 277-89. August.](Hudak83_DistributedTaskAndMemoryManagement.pdf)

Hughes, R.J.M. 1982. Reference-counting with Circular Structures in Virtual Memory Applicative Systems. Programming Research Group, Oxford.

[Hughes, R.J.M. 1985. A distributed garbage collection algorithm. in Proceedings of the IFIP Conf. on Functional Programming and Computer Architecture, Nancy, pp 256-72. Jouannaud(editor). LNCS 201, Springer Verlag. September.](Jouannaud85_ConferenceOnFunctionalProgrammingLanguagesAndComputerArchitecture.djvu)

[Knuth, D. 1976. The Art of Computer Programming. Vol. 1, Section 2.5. Addison Wesley.](TAOCP_Vol1_FundamentalAlgorithms.pdf)

[Kung, H.T., and Wong, S.W. 1977. An Efficient Parallel Garbage Collection System, and its Correctness Proof. Dept of Comp. Sci., Carnegie-Mellon Univ. September.](Kung77_AnEfficientParallelGarbageCollectionSystemAndItsCorrectnessProof.pdf)

[Lieberman, H., and Hewitt, C. 1983. A real time garbage collector based on the lifetimes of objects. Communications of the ACM.Vol. 26, no. 6, pp. 419-29.](Lieberman83_ARealTimeGarbageCollectorBasedOnTheLifetimesOfObjects.pdf)

Mohamed-Ali, K.A. 1984. Object oriented storage management and garbage collection in distributed processing systems. PhD Thesis, report TRITA-CS-8406. Royal Institute of Technology, Stockholm. December.

[Schorr, H., and Waite, W. 1967. An efficient machine independent procedure for garbage collection. Communications of the ACM.Vol. 10, no. 8, pp. 501-6.](schorr67_AnEfficientMachineIndependentProcedureForGarbageCollection.pdf)

[Steele, G.L. 1975. Multiprocessing compactifying garbage collection. Communications of the ACM.Vol.18, no. 9, pp. 495-508.](steele75_MultiprocessingCompactifyingGarbageCollection.pdf)

[Stoye, W.R., Clarke, T.J.W., and Norman, A.C. 1984. Some practical methods for rapid combinator reduction. In Proceedings of the ACM Symposium on Lisp and Functional Programming, Austin, pp. 159-66. August.](stoye84_SomePracticalMethodsForRapidCombinatorReduction.pdf)

[Wadler, P. 1984. Listlessness is better than laziness: lazy evaluation and garbage collection at compile-time. In Proceedings ofthe ACM Symposium on Lisp and Functional Programming, Austin, pp. 45-52. August.](Wadler84_ListlessnessIsBetterThanLaziness.pdf)

[Wise, D. 1985. Design for a multiprocessing heap with on-board reference-counting. In Functional Programming and Computer Architecture, Nancy, pp. 289-304. Jouannaud (editor). LNCS 201. Springer Verlag.](Jouannaud85_ConferenceOnFunctionalProgrammingLanguagesAndComputerArchitecture.djvu)

[Wise, D.S., and Friedman, D.P. 1977. The one-bit reference-count. B/T. Vol. 17, no. 3, pp. 351-9.](Wise77_The_one_bit_reference_count.pdf)

## CH18

### CH18 References

[Augustsson, L. 1984. A compiler for lazy ML. In Proceedings of the ACM Symposium on Lisp and Functional Programming, Austin, pp. 218-27, August.](augustsson84_ACompilerForLazyML.pdf)

[Burstall, R.M., MacQueen, D.B., and Sanella, D.T. 1980. HOPE: an experimental applicative language. In Proceedings of the ACM Lisp Conference, pp. 136-43, August.](burstall80_Hope.pdf)

[Clark, R. (editor) 1981. UCSD P-system and UCSD Pascal Users’ Manual, 2nd edition. Softech Microsystems, San Diego.](Clark81_UCSD_PSystemAndUCSDPascalUsersManual.pdf)

Elworthy, D. 1985. Implementing a Ponder cross compiler for the SKIM processor. Dip. Comp.Sci. Dissertation, Computer Lab., Cambridge. July.

[Fairbairn, J. 1982. Ponder and its type system. Technical Report 31. Computer Lab., Cambridge. November.](Fairbairn82_PonderAndItsTypeSystem.pdf)

[Fairbairn,J. 1985. Design and implementationofa simple typed language based on the lambda calculus. Technical Report 75. Computer Lab., Cambridge. May.](Fairbairn85DesignAndImplementationOfASimpleTypedLanguageBasedOnTheLambdaCalculus.pdf)

[Fairbairn, J., and Wray, S.C. 1986. Code generation techniques for functional languages. In Proceedings of the ACM Conference on Lisp and Functional Programming, Boston, pp. 94-104, August.](fairbairn86_CodeGenerationTechniquesForFunctionalLanguages.pdf)

Field, A. 1985. The Compilation of FP/M Programs into Conventional Machine Code. Dept Comp.Sci., Imperial College. June.

[Griss, M.L., and Hearn, A.C. 1981. A portable Lisp compiler. Software— Practice and Experience. Vol. 11, pp. 541-605.](griss81_APortableLispCompiler.pdf)

[Hudak, P., and Kranz, D. 1984. A combinator based compiler for a functional language. In Proceedings of the llth ACM Symposium on Principles of Programming Languages, pp. 122-32, January.](Hudak84_ACombinatorBasedCompilerForAFunctionalLanguage.pdf)

[Johnsson, T. 1984. Efficient compilation of lazy evaluation. In Proceedings of the ACM Conference on Compiler Construction, Montreal, pp. 58-69, June.](johnsson2004_84_EfficientCompilationOfLazyEvaluation.pdf)

Lester, D. 1985. The correctness of a G-machine compiler. MSc dissertation, Programming Research Group, Oxford. December.

[Rees, J.A., and Adams, N.1. 1982. T —a dialect of LISP. In Proceedings of the ACM Symposium on Lisp and Functional Programming, pp. 114-22, August.](rees82_T_ADialectOfLispOrLambdaTheUltimateSoftwareTool.pdf)

[Richards, M. 1971. The portability of the BCPL compiler. Software — Practice and Experience. Vol. 1, no. 2, pp. 135-46.](richards71_ThePortabilityOfTheBCPLCompiler.pdf)

[Steele, G.L., and Sussman, G.J. 1978. The Revised Report on Scheme. Al Memo452, MIT. January.](steele78_TheRevisedReportOnScheme.pdf)

## CH19

### CH19 References

Aho, A.V., and Ullman, J.D. 1977. Principles of Compiler Design. Addison Wesley. Bauer, F.L., and Eickel, J. 1976. Compiler Construction. Springer Verlag.

[Landin, P.J. 1964. The mechanical evaluation of expressions. ComputerJournal. Vol. 6, pp. 308-20.](Landin64_TheMechanicalEvaluationOfExpressions.pdf)

[Wulf, W., Johnsson, R.K., Weinstock, C.B., Hobbs, S.O., and Geschke, C.M. 1975.The Design of an Optimising Compiler. Elsevier.](Wulf77_TheDesignOfAnOptimizingCompiler.djvu)

## CH20

### CH20 References

[Augustsson, L. 1985. Compiling pattern matching. In Conference on Functional Programming Languages and Computer Architecture, Nancy. Jouannaud (editor). LNCS 201. Springer Verlag.](Augustsson85_CompilingPatternMatching.pdf)

