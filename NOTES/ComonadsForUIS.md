# [Comonads For UIS](https://web.archive.org/web/20210818153626/https://arthurxavierx.github.io/ComonadsForUIs.pdf)

## Intro

"... for now the reader may bear in mind that comonads are traditionally used for structuring context-dependent computations, whereas monads are commonly used for structuring context-producing or side-effecting computations - and side effecting computations are impure ones, that is, those which do not preserve the property of referential transparency."

## Pure fp and cat theory

[Natural Transformations](https://ncatlab.org/joyalscatlab/published/Natural+transformations)

For any given functors F and G, from a category C to a category D,

A natural transformation α from F to G is a family of morphisms in D such that:

1. for every object A in C, a _component_ of α at A is a morphism αA : F(A) -> G(A) in D;
that is, the natural transformation maps objects in C transformed by the functor F (which are objects in D) to objects transformed by the functor G.

2. for every morphism f : A -> B in C, the components of a natural transformation must obey what is called the naturality condition : aB . F(f) = G(f) . aA (This commuting diagram bellow shows this)

```
         αA
  FA ----------> GA
    |           |
    |           |
F(f)|           |G(f)
    |           |
   \/    αB     \/
   FB----------> GB
```

A in C, FA in D, F G functors are objects of a category [C,D] called the functor category.

Morphism α:F->G between two functors F,G:C->D is called a natural transformation such that the above diagram commutes.

The composite of two natural transformations α:F -> G and β: G -> H in the category [C,D] is the natural transformation βα: F -> H obtained by putting (βα)A = βAαA for every object A in C.

```
         αA             βA
  FA ----------> GA ---------> HA
    |           |              |
    |           |              |
F(f)|           |G(f)          | H(f)
    |           |              |
   \/    αB     \/             |
   FB----------> GB ---------> HB
```

Example 2.3.2: There are multiple functions that map values of type [a] to Maybe a:

```haskell
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

nothing :: [a] -> Maybe a
nothing _ = Nothing


### 2.4 Monads

- [Moggi 91 - Notions of computation and monads](https://www.cs.cmu.edu/~crary/819-f09/Moggi91.pdf)
- [Wadler 95 - Monads for functional programming](https://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf)

A monad is a triple (T,η,μ), where
- T is an endofunctor from category C to itself
- η and μ are natural transformations

η maps the identitiy endofunctor on C(1c) to T and can be written as η : 1c -> T

μ maps a composition of T with itself to T, and can be written as μ: T . T -> T

Both natural transformations must be defined such that, for every object A in C:

- μA . T(ηA) = μA . ηT(A) = 1T(A)
- μA . T(μA) = μA . μT(A)

```haskell
class Functor m => Monad m where
    return :: a -> m a
    join :: m (m a) -> m a
    bind :: (a -> m b) -> m a -> m b

    join = bind id
    bind f = join . fmap f
```

The first two functions of the monad typeclass ARE the η and μ natural transformations.

Bind is simply used for both performance and ergonomics (as >>= infix operator with args flipped)

(>>=) :: Monad m => m a -> (a -> m b) -> m b

Monad Laws

- bind return = id
- bind f . return = f
- bind f . bind g = bind (bind f . g)

id :: a -> a

If we use :t  on the lefthand side of these laws it makes more sense

bind return :: Monad m -> m a -> m a
\f -> bind f . return :: Monad m => (a -> m b) -> a -> m b
\f -> \g -> bind f . bind g :: Monad m => (b -> c) -> (a -> b) -> m a -> m b

If we use :t  on the lefthand side of these laws it makes more sense and they typecheck like we expect.

[The Laws each monad type class should satisfy](https://wiki.haskell.org/Monad_laws)

Left identity: return a >>= h  ≡ h a
Right identiy: m >>= return    ≡ m
Associativity: (m >>= g) >>= h ≡ m >>= (\x -> g x >>= h)

Additionally theres a monad composition operator known as the Kleisli-composition operator

(>=>) :: Monad m => (a -> m b) -> (b -> c) -> (a -> m c)
f >=> g = \x -> f x >>= g

Which lets us express the laws as

Left identity: return >=> h  ≡ h a
Right identiy: f >=> return    ≡ f
Associativity: (f >=> g) >=> h ≡ f >=> (g >=> h)

Which makes it easier to see that monad composition is an associative operator with left and right identities
