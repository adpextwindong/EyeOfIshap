# Type Checking and Type Inference

- [Mukul Rathi intro to type theory and type checking](https://mukulrathi.com/create-your-own-programming-language/intro-to-type-checking/)

## HM Type inference
Q —
I have an impression that I did an exercise from some book some time ago about HM type inference and the whole thing was just a very basic unification algorithm and the whole implementation was like 10 lines of code
thing is so general that it can be just a basis for whatever else
http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.65.7733&rep=rep1&type=pdf

basically this, and it has more ceremony than needed, can be 2-3 times shorter, and as I see it, any language that has abstract syntax containing the one defined on page 2 as its subset can try start with the same algorithm as basis and see if it extends to the rest of abstract syntax and types

P —
Pretty much, except no actual implementation would use explicit substitutions

I —
not really
HM tells you how to typecheck let bindings, lambdas, applications
you can apply these forms to a number of languages
also almost every actual implementation out there extends HM with a number of other features
ranging from basic stuff like constant literals, list literals
to most sophisticated stuff like keyword arguments, row polymorphism, GADTs, typeclasses!

### Rust
G —
in one paper (rustbelt) a "formal version of rust" is called lambda-rust

E —
I found the paper: https://people.mpi-sws.org/~dreyer/papers/rustbelt/paper.pdf

G —
yeah i guess it is just a faithful map, then there is https://arxiv.org/pdf/1903.00982.pdf

E —
Although I think lambda rust is in fact a "model" of Rust. Rust is not built on top of lambda rust in the same way Haskell is built on top of System F.

I —
papers like rustbelt will create simplified versions of rust that let the authors focus on the stuff they are trying to prove while also allowing the rest of rust to be accurately extrapolated
system f is also a model

### GHC Intermediate Language
E —
https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/fc <-- Is says here: GHC's intermediate language was essentially System Fw.

G —
the oxide paper makes that distinction as well, so they are trying to model the sourse language instead

I —
ghc uses an intermediate language similar to system f
that isnt to say ghc's IR "is" system f

G —
e.g., this [oxide] does have variables, abstractions, and applications as a subset

K —
that doesn't look like the normal type inference to me
core is also typed, it has it's own type system

G —
apparently even if it was HM (as in the algorithm), they switched to some other algorithm

E —
Makes sense. Most algorithms in the wild are never the vanilla versions that you read about in textbooks.

I —
and that's not a bad thing
altho yes HM is extremely limiting
you need to incorporate some form of bidirectional typechecking to have better error messages and to handle the cases that are outside the scope of HM
for instance overlapping record field names dont work for HM at all

P —
the only error message you need in HM is "learn to code"

I —
honestly most of the time when people say HM they just mean "we use type variables and unification"

P —
error locality is a non-issue for competent programmers 🙏

I —
i guess it also implies some sort of non locality
which is perhaps inaccurate
or at least, isnt telling nearly enough

P —
what's more sad is that people always link algorithm W for HM
which is absolute garbage and an extreme overcomplication of the core algorithm

I —
~~hindley milner sounds fancy so its good to put on the README for your toy language to pretend like its any good~~

P —
just tells the reader that you have decidable type inference and favour a style of typing a-la curry or whatever
think it's just a good basis for a decent language


### What do you recommend instead that is representational of the core algorithm? - V

P —
algorithm J
in other words
it's more important to understand quick-union
than it is to understand substitutions
then just add Remy's levels for efficient generalisation and you're golden
of course, you'll still be screwed by pathological cases
but only SMLNJ gives a shit about those
you can make OCaml compiler terminate in a million years with like 40 chars of code
because of poor type representation that doesn't use hash consing or memoization

I —
isnt it because of let-poly anyways
which barely anyone gives a shit about in practice

P —
yeah but you can drastically reduce number of nodes in the type representation by choosing a better type representation
but nobody bothers since the pathological case doesn't arise in practice
so best off just doing weighted quick-union and some other shit

I —
you gotta choose your battles

j —
I am curious now, what are those 40 chars?
P —
let's call it slightly more than 40
Image
can refer to https://www.cs.tufts.edu/~nr/cs257/archive/zhong-shao/implementing-tils.pdf
