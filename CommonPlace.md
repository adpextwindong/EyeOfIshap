# Common Place

## Monads

"As we will see, lambda terms also form a monad; the unit and join operations on
lambda terms will be needed in the definition of lambda abstraction and application."

from Bird 1999

## Applicative VS Normal Order

"In traditional programming languages the most common order of evaluation is applicative order, where the arguments of a function are evaluated before the body. Applicative
order has the disadvantage that it does not necessarily terminate, even though an expression has a value. Consider K (2 + 2) (S I I (S I I)). In applicative order, the arguments
are evaluated first; (2 + 2) gives 4, but (S I I x) = x x so (S I I (S I I)) = (S I I (S I I)),
which is the same as before. If the K is applied first, the whole expression reduces to
(2+ 2) and then to 4. Indeed, if the leftmost reducible expression (sometimes called redex)
is always reduced first, the result is guaranteed to be found if it exists [Barendregt 1980].
This is called normal order reduction."

from Fairbairn 1985

## Type Checking

"The design of a type-system inevitably involves compromise. At one extreme, a specification of a programme can be regarded as a type; type-checking in this regime corresponds
to proof-checking [Martin-L¨of 1975]."

- [Richard S. Bird, Ross Paterson - de Bruijn notation as a nested datatype](https://www.staff.city.ac.uk/~ross/papers/debruijn.html)
- H.P. Barendregt 1980 - The λ-calculus, its Syntax and Semantics
- [Jon Fairbairn 1985 - Design and implementation of a simple typed language based on the lambda-calculus](https://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-75.pdf)
- [Per Martin-Löf 1975 - Intuitionistic Type-Theory](https://archive-pml.github.io/martin-lof/pdfs/Bibliopolis-Book-retypeset-1984.pdf)

## Definitional Interpreters

From ["Abstracting Definitional Interpreters: Functional Pearl"](http://web.archive.org/web/20220414202034/https://plum-umd.github.io/abstracting-definitional-interpreters/)

"Definitional interpreters, in contrast to abstract machines, can leave aspects of computation implicit, relying on the semantics of the defining-language to define the semantics of the defined-language, an observation made by Reynolds (1972) in his landmark paper, [Definitional Interpreters for Higher-order Programming Languages.](https://dl.acm.org/doi/10.1145/800194.805852) For example, Reynolds showed it is possible to write a definitional interpreter such that it defines a call-by-value language when the metalanguage is call-by-value, and defines a call-by-name language when the metalanguage is call-by-name."

## Bidrectional typechecking

From ["Complete and Easy Bidirectional Typechecking
for Higher-Rank Polymorphism"](https://arxiv.org/pdf/1306.6032.pdf)

"Bidirectional typechecking (Pierce and Turner 2000) has become
one of the most popular techniques for implementing typecheckers in new languages. This technique has been used for dependent types (Coquand 1996; Abel et al. 2008; Löh et al. 2008; Asperti et al. 2012); subtyping (Pierce and Turner 2000); intersection, union, indexed and refinement types (Xi 1998; Davies and
Pfenning 2000; Dunfield and Pfenning 2004); termination checking (Abel 2004); higher-rank polymorphism (Peyton Jones et al.
2007; Dunfield 2009); refinement types for LF (Lovas 2010); contextual modal types (Pientka 2008); compiler intermediate representations (Chlipala et al. 2005); and object-oriented languages including C (Bierman et al. 2007) and Scala (Odersky et al. 2001).
As can be seen, it scales well to advanced type systems; moreover,
it is easy to implement, and yields relatively high-quality error messages (Peyton Jones et al. 2007)."
