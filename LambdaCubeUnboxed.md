# [Lambda Cube Unboxed](https://www.youtube.com/watch?v=UCzE15Hvs1E&list=PLNwzBl6BGLwOKBFVbvp-GFjAA_ESZ--q4&index=1)

Sources:

- [Nederpelt, R., Geuvers, H. (2014). Type Theory and Formal Proof: An Introduction. Cambridge University Press.](https://drive.google.com/file/d/10yEDaYdMpINOB3H0AE3SrDWPDvzhGOl-/view?usp=sharing)
- [Barendregt, H. P. (1992). Lambda Calculi with Types.](https://drive.google.com/file/d/1LiiYgG_Yl-sQETxdNxw_C2nZsf9HggA8/view?usp=sharing)
- [Wadler, P. (2015). Propositions as Types. Communications of the ACM, 58(12), 75-84](https://homepages.inf.ed.ac.uk/wadler/papers/propositions-as-types/propositions-as-types.pdf)
- Polymorhpic λ-Calculus:
  - [Girad, J. Y., Taylor, P., Lafont, Y. (1989). Proofs and types (Vol. 7). Cambridge: Cambridge university press.](https://people.mpi-sws.org/~dreyer/ats/papers/girard.pdf)
  - [Pierce, B. Dietzen, S., Michaylov, S. (1989). Programming in Higher-Order Typed Lambda-Calculi](https://drive.google.com/file/d/12TkFzUbN_i-w0IDceT4fvbYpmDpZT8xh/view?usp=sharing)

NOTE: [Tech Report for Pierce Programming in Higher-Order Typed Lambda-Calculi](http://www.cis.upenn.edu/~bcpierce/papers/leap.pdf)

## [1.1 - Introduction to the Untyped λ calculus](https://www.youtube.com/watch?v=233A3bN0UBE&list=PLNwzBl6BGLwOKBFVbvp-GFjAA_ESZ--q4&index=2)

[Lambda Calculus Calculator](https://lambdacalc.io/)

```haskell
type Name = String

data Expr = VAR Name      -- x
          | ABS Name Expr -- (λx.M)
          | APP Expr Expr -- (M N)
```

Operations:

α-conversion: (λx.M[x]) -> (λy.M[y]), renaming bound varaibles to avoid naming colisions.
***THE MAIN OPERATION***
β-reduction: ((\x.M) E) -> (M[x := E]), replacing the bound variable with the arguement expression in the body of the abstraction.

Notation:

Lowercase for vars, Uppercase for λ  Terms, Syntactical identity will be denoted with ≡.

Application is left assoc. (M N) L ≡ M N L
Abstraction is right associative. λx(λy.M) ≡ λx.λy.M

Application takes precedence over abstraction λx.(M N) ≡ λx. M N.

### [Free and Bound Variables](https://youtu.be/233A3bN0UBE?list=PLNwzBl6BGLwOKBFVbvp-GFjAA_ESZ--q4&t=453)

λx.x. x is bound.

Definition: The set of Free Variables FV of a λ-Term

Variable    FV(x) = {x}
Application FV(M N) = FV(M) ∪ FV(N)
Abstraction FV(λx.M) = FV(M) \ {x}

Exercise: The set of Bound Variables BV of a λ-Term
Variable    BV(x) = {}
Application BV((λx.M) N) = (BV(M) \ {x}) ∪ BV(N)
Abstraction FV(λx.M) = {x} ∪ BV(M)

Example:

((λx.λy.x)(λf.λg.λa.f g a)) -> (λy.λf.λg.λa.f g a)
({x y} \ x) ∪ {f g a}
{y f g a}

Name free notation by De Bruijn

α-Conversion, Two λ-terms M, N ∈ Λ, denoted by M ≡ N, if and only if they differ by the names of their bound variables.

Example: x (λx.xy) ≡ x (λz.zy)
                   ≠ z (λx.xy)

Barendregt Convention, All bound variables in a λ-term M ∈ Λ should be pairwise different and chosen such that they differ from the free variables in the term.

For example this would be disallowed. (λx.xx)(λx.xx)

### [Substitution](https://youtu.be/233A3bN0UBE?list=PLNwzBl6BGLwOKBFVbvp-GFjAA_ESZ--q4&t=807)

Variable

$x[x := N] ≡ N.$

$y[x := N] ≡ y, if x \not\equiv y.$

Application --Simply recurse subst.

$(P Q)[x := N] ≡ (P[x := N])(Q[x := N]).$

Abstraction

$(λy.P)[x := N] ≡ λy. (P[x := N]), if y \not\in FV(N).$

Alpha conversion here is important, y could occur freely in N. We need to avoid knowing which y is bound or free. So we alpha convert.

Example trap of blindly doing it:

(λy.yx)[ x := λz.zy ] → λy.y(λz.zy)

Now we don't know which y is bound or free cause we didn't alpha convert.

Because y ∈ FV(λz.zy), lets alpha convert

(λy1.y1 x)[ x := λz.zy ] ≡ λy1.y1(λz.zy)

## [β-Reduction in the Untyped λ-Calculus](https://www.youtube.com/watch?v=R1dmeFEJyqI&list=PLNwzBl6BGLwOKBFVbvp-GFjAA_ESZ--q4&index=3)