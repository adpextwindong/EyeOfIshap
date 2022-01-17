# [ZuriHac 2015 - Discrimination is Wrong: Improving Productivity](https://www.youtube.com/watch?v=cB8DapKQz-I)

Generalized form of sorting/partitioning

Radix/Bucket Sort, sorting on pair-wise comparission takes nlogn time, but other situations don't necessitate pairwise. Bucketting things (if applicable) or the LSB can break items into buckets.

Miguel Ryan Burke - Radix Sort Top Down - [American Flag Sort](https://en.wikipedia.org/wiki/American_flag_sort)

The American Flag technique can be applied to ADTs.

Sorting/Partitioning/Joining Tables/Constructing Maps and Sets in _linear time_.

## Who

Fritz Henglein

- 2007 Generic Discriminiation Sorting and Partitioning Unshared Data in Linear Time
- 2009 Generic Top-down Discrimination
- [2010 Generic Top-down Discrimination for Sorting and Partitioning in Linear Time](https://www.cs.ox.ac.uk/projects/utgp/school/henglein2012c.pdf)
- [2011 Generic multiset programming with discrimination-based joins and symbolic Cartesian products](http://web.archive.org/web/20150908202401/https://www.cs.ox.ac.uk/projects/utgp/school/henglein2010d.pdf)

## Monoid

```haskell
class Monoid m where
  mappend :: m -> m -> m
  mempty :: m
```

Not the most general definition we could give for a monoid.

## Monoidal Category

A monodial category (C, ⊗, I) is a category C equipped with:

- a bifunctor (⊗) :: C * C -> C
- an object I :: C
- and natural isomorphisms
  - ρ :: (A ⊗ I) ~ A    "A tensor I is isomorphic to A"
  - λ :: (I ⊗ A) ~ A
  - α :: (A ⊗ B) ⊗ C ~ A ⊗ (B ⊗ C) "The ability to reassociate things in my bifunctor"

So whats monoidlike here is the associative operation and unit laws.

(,) and Either (Products and Coproducts) in Hask can be monodial categories. (With unit for product and void for coproduct).

Either a Void is isomorphic to a because Void is uninhabitated.

$Hask^{Hask}$ is a category with functors as objects and natural transformations as arrows.

This with compose and identity is a monodial category.

## Monoid Objects

A monoid object in a monoidal category (C, ⊗, I) consists of

- a carrier object M
- η :: I -> M
- μ :: M ⊗ M -> M

```haskell
η () = mempty
μ = uncurry mappend
```

## Monads as Monoid Objects

A monoid object in ($Hask^{Hask}$, Compose, Identity) is a Functor M with

```haskell
η :: Identity -> M
η = return . runIdentity

μ :: Compose M M -> M
μ = join . getCompose
```

s.t. the monad laws hold.

## Day Convolution from (<*>)

```
data Day f g a where
  Day :: f (a -> b) -> g a -> Day f g b

(<*>) :: Applicative f => f (a -> b) -> f a -> f b

Day (<*>) :: Day f f -> f
```

The monoid objects w.r.t are applicatives.

## Applicatives as Monoid Objects

A monoid object in ($Hask^{Hask}$, Day, Identity) is a Functor M with

```haskell
η :: Identity -> M
η = pure . runIdentity

μ :: Day M M -> M
μ (Day m n) = m <*> n
```

such that the _Applicative laws_ hold on M.

## ~~ are Monoids in the category of Endofunctors

"Applicatives are Monoids in the Category of Endofunctors."

"Monads are Monoids in the Category of Endofunctors."

Applicatives were ($Hask^{Hask}$, Day, Identity).
Monads were ($Hask^{Hask}$, Compose, Identity).

The tensors are different.

## Day Convlution from liftA2

Covariant Day Convolution

```haskell
data Day f g a where
  Day :: ((a ⊗_1 b) -> c) ⊗_2 f a ⊗_2 g b -> Day f g c
```

Nothing here is fundamentally about products so we can go towards liftA2.

Contravariant Day Convolution

```haskell
data Day f g a where
  Day :: (c -> (a ⊗_1 b)) ⊗_2 f a ⊗_2 g b -> Day f g c
```

Note: [⊗_1 is the tensor in the category I came from, and ⊗_2 is the tensor in the category im going to.](https://youtu.be/cB8DapKQz-I?t=1283)

Is there a Contravariant notion of Applicative?

## Divide and Conquer

```haskell
class Contravariant f => Divisible f where
  divide :: (a -> (b, c)) -> f b -> f c -> f a
  conquer :: f a
```

Given a function for splitting a into a product (cases) of B and C, and you can "handle" B's and "handle" C's we can "handle" A's.

Conquer :: f a being the default "handler" for all A's. This has to be a unit for divide, like Pure for Ap.

Comes from contravariant Day Convolution:

```haskell
data Day f g a where
  Day :: (c -> (a ⊗_1 b)) ⊗_2 f a ⊗_2 g b -> Day f g c
```

with

⊗_1 = (,)
⊗_2 = (,)

## Some laws for Divide and Conquer

```haskell
delta a = (a,a)
divide delta m conquer = m
divide delta conquer m = m
divide delta (divide delta m n) o = divide delta m (divide delta n o)
```

## Choose and Lose

Contravariant form of alternative.

```haskell
class Divisible f => Decidable f where
  choose :: (a -> Either b c) -> f b -> f c -> f a
  lose :: (a -> Void) -> f a                   --If a "is completely impossible", then its handable
```

⊗_1 = Either
⊗_2 = (,)

## Ex falso quodlibet - from falsehood whatever pleases.

```haskell
pureish :: Applicative f => (() -> a) -> f a
emptyish :: Alternative f => (Void -> a) -> f a
```

Considering the Units in their respective Monoids, the arguments to the functions can be dropped.

```haskell
conquerish :: Divisible f => (a -> ()) -> f a
lose :: Deciadable f => (a -> Void) -> f a
```

() is the terminal object in Haskell so those arrows are completely determined. ( const ())

```haskell
pure a = pureish (const a)
empty = emptyish absurd
conquer = conquerish (const ())

absurd :: Void -> a -- Since Void values logically don't exist, this witnesses the logical reasoning tool of "ex falso quodlibet".

"Category Theory as Vaseline for the lens." - Kmett
```