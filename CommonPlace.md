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
