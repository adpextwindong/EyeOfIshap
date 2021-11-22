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

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g = \x -> f x >>= g

Which lets us express the laws as

Left identity: return >=> h  ≡ h a
Right identiy: f >=> return    ≡ f
Associativity: (f >=> g) >=> h ≡ f >=> (g >=> h)

Which makes it easier to see that monad composition is an associative operator with left and right identities

### 2.5 Comonads

A comonad is a triple (D,ε,δ) where D is an endofunctor on a category C and ε and δ are natural transformations mapping D to the identity functor on C and D to a composotions of D with itself (written D . D), that is:

ε : D -> 1c
δ: D -> D . D

such that

T(εA) . δA = εT(A) . δA = 1T(A)
T(δA) . δA = δT(A) . δA

In Haskell we get a type constructor for Comonad like this. (Think of w like a flipped m)

```haskell
class Functor w => Comonad w where
    extract :: w a -> a
    duplicate :: w a -> w (w a)
    extend :: (w a -> b) -> w a -> w b

    duplicate = extend id
    extend f = fmap f . duplicate
```

extract represents the ε natural transformation
duplicate represents the δ natural transformation

extend is a helper function defined in terms of fmap and duplicate

Similarly to monad, Comonad type class must obey these following rules derived from the category theoretical rules.

- extend extract = id
- extract . extend f = f
- extend f . extend g = extend (f . extend g)

### Interpretations of comonads as computations

Comonads in functional progams provide means to structure context-dependent notions of computations [[Uustalu & Vene, 2008]](https://doi.org/10.1016/j.entcs.2008.05.029)

Theres a duality in play between interpretatinos of monads and comonads

In one hand, monads can model context-producing (or side-effecting) sequential computations, in the other,

Comonads can model context-dependent sequential computations.

Another valid (and not at all exclusive interpretation is that comonads can represent computations within a context, that is, computations from a global context to a local value, which may be _extended_ to the whole, thus producing a new global context. Two of the most acknowledgeable examples of this are that comonads form a good model for representing cellular automata or operations for image manipulation.

#### Stream Example 2.5.2

```haskell
data Stream a = Cons a (Stream a)

instance Functor Stream where
    fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Comonad Stream where
    extract (Cons a _) = a
    duplicate stream@(Cons a as) = Cons stream (duplicate as)
```

TODO

## [Comonads as Spaces](https://blog.functorial.com/posts/2016-08-07-Comonads-As-Spaces.html)

Freeman [2016b] proceeds to use comonads for representing (pointed) spaces of states of an application, where the current state of the application can be retrieved by extracting the value of out of the comonad (with the extract function), and the neighbourhood of the current value gives the possible future states for this application.

In this way, movements in this space would correspond to state transitions in an application, and distinct comonads may produce different spaces of states with different possibilities for movements.

```haskell
class Pairing f g | f -> g, g -> f where
    pair :: (a -> b -> c) -> f a -> g b -> c
```

F and G structure's match in some way that can annihilate each other yielding values of the types encapsulated by both.

### 3.2 Exploring Comonadic spaces with pairings

Freeman uses comonads to represent the space of states of an application. As such, it is desirable to be able to transition between the current state of an application and a possible future state, having, then, "movements" in this comonadic space.

Freeman uses monads in order to express these movements in the spaces described by comonads.

Leading us to pair comonad w with monad m, a pairing between both "gives a way to explore the data in a comonadic value".

To illustrate this moving around in a space described by a comonad, we can think of the duplicate function.

Duplicating a comonad means adding another dimension to the space, and transforming it in such a way that, in every point x of the space, now lies a replicate of the sapce with x as the base point.

Example 3.2.1

Extending 2.5.2 Streams example
```
data Stream a = Cons a (Stream a)

instance Comonad Stream where
    extract (Cons a _) = a
    duplicate stream@(Cons a as) = Cons stream (duplicate as)
```

By applying duplicate to a value of a Stream a type, one obtains a stream of streams, where the first/current value of the stream is the original stream tiself, and the future values are the old future streams themselves.

Figure 3.1 Inf tap vs Infinite grid figure

#### 3.2.1 Comonads as spaces and monads as movements

Thinking in terms of states of an application, by duplicating the current space of states, one obtains a space of all future spaces of states; and, in order to move to a future state one must simply pick one of the future spaces of states.

THIS IS WHERE PAIR COMES IN

In order to extract a new space from a (duplicated) space of spaces with the function `move` defined below:

```haskell
pair :: (a -> b -> c) -> f a -> g b -> c

move :: (Comonad w, Pairing m w) => w a -> m b -> w a
move space movement = pair (\_ newSpace -> newSpace) movement (duplicate space)
```

#### Example 3.2.2

We can define a monad which can produce a pairing against the Stream comonad sowe can move inside its space. (Functor instance omitted)

```haskell
data Sequence a = End a | Next (Sequence a)

instance Monad Sequence where
    return = End
    bind f (End a) = f a
    bind f (Next next) = Next (bind f next)
```

Leading us to define a Pairing with Stream comonad and Sequence Monad

```haskell
--pair :: (a -> b -> c) -> f a -> g b -> c

instance Pairing Sequence Stream where
    pair f (End a) (Cons b _) = f a b
    pair f (Next next) (Cons _ stream) = pair f next stream

stream :: Stream Int
stream = Cons 1 (Cons 2 (Cons 3 ...)

--Remember move from 3.2.1?
move :: (Comonad w, Pairing m w) => w a -> m b -> w a
move space movement = pair (\_ newSpace -> newSpace) movement (duplicate space)

third :: Stream Int
third = move stream (Next (Next (End ())))
```

## 4 User Interfaces and Comonads

Comonads may be a useful abstraction for the space of states of an application, where the current state of the application is always accessible through the use of the extract function of the Comonad type class.

In this case, this space of states ebcomes the states of the application itself. Providing a useful abstraction for controlling how the states of the application may behave, as well as for talking about different _architectures_ for user interaces, the core of this work.
