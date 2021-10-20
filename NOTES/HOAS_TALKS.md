# [Devon Stewart - Higher Order Abstract Syntax](https://www.youtube.com/watch?v=d36y3NYmxH8)

[Slides](https://github.com/blast-hardcheese/talks/tree/hoas)

# [Phil Freeman - Embedded DSLs in Haskell](https://www.youtube.com/watch?v=8DdyWgRYEeI)

[how to $ not write a programming language](https://github.com/paf31/haskell-slides/tree/master/hoas)

Using Haskell as a host language for embedding a DSL eliminates a lot of work up front for us. Such as:

- Lexing
- Parsing
- Type Checking, host language compiler just does this for us

On the otherhand we can still perform some optimization and code generation is in our court. We've reduced the problem space to transformations/functions/syntax representation.

We want:

- Familiar Syntax
- Composability
- Typed Language
- Support for static analysis

A tradeoff appears between static analysis and expressiveness.

## Monoids

:info Monoid

```haskell
class Semigroup a => Monoid a where
    mempty :: a
    mappend :: a -> a -> a
    mconcat :: [a] -> a             -- Can be implemented using mempty and mappend
    {-# MINIMAL mempty #-}
```

### Document Example

We want composition, in this case composing documents vertically.
An empty document needs to exist too.

This means documents can form a monoid.

- The free monoid over a type a is [a]

List is the cannonical choice, free monoid, in most cases.

From wikipedia's [Free Object article](https://en.wikipedia.org/wiki/Free_object).

```
Informally, a free object over a set A can be thought of as being a "generic" algebraic structure over A: the only equations that hold between elements of the free object are those that follow from the defining axioms of the algebraic structure.
```

"[List] is the minimal monoid for a type that doesn't assume any extra laws."

A tree could be a monoid but mappend says we can compose in any order so in reality flattening a tree into a list achieves the free monoid. An empty listen being mempty.

So this leads us to using String as our type for representing documents an [String] as the free monoid.

Now we can move onto making a type class for things that behave like documents

```haskell
import Data.String

--Gives us
class IsString a where
    fromString :: String -> a

class (IsString a, Monoid a) => Document a where
    indent :: Int -> a -> a
    beside :: a -> a -> a
```

Now we can write documents polymorphic in the Document implementation.

```haskell
columns :: (Document a) => a
columns = column1 `beside` " " `beside` column2
    where
    column1 = "Haskell"     <> "C"          <> "Prolog"
    column2 = "Functional"  <> "Imperative" <> "Logic"
```

We can now write a Document instance for [String].
However implementing `beside` requires a nice way to track the document width so we must associate some sort of data with it to do this. (To avoid recomputing max on the free monoid over and over.) Leading us to:

```haskell
data PlainText = PlainText
    { docuWidth :: Int
    , docLines  :: [String]
    }
```

This type isn't necesarrily the best given a purpose. If we wanted to simulate text fitting to answer whether the composition fits in a region, we can just omit docLines and use just a docuWidth containing instance. Fundamentally however we've given ourselves a typeclass to reason about around.Add to gitignor

## [Static Languages - Applicative](https://youtu.be/8DdyWgRYEeI?t=1017)

:info Applicative

```haskell
class Functor f => Applicative (f :: * -> *) where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
    liftA2 :: (a -> b -> c) -> f a -> f b -> F c
    (*>) :: f a -> f b -> f b
    (<*) :: f a -> f b -> f a
    {-# MINIMAL pure, ((<*>) | liftA2) #-}
```

Applicative needs a way to lift values into the functor and then a way to apply functions onto the functorialized type. Functions can also be lifted into this functorial context.

One thing to note is that the type constructor itself is applicative and not the base type. String isn't applicative, list is.

```haskell
data [] a = [] | a : [a] -- Defined in ‘GHC.Types’

instance Applicative []
    pure x      = [x]
    fs <*> xs   = [f x | f <- fs, x <- xs]
    liftA2 f xs ys = [f x y | x <- xs, y <- ys]
    xs *> ys    = [y | _ <- xs, y <- ys]
```

You can think of the Applicative Functor holding functions and tracking some notion of side-effect. (Both the function and value have "effects").

List Applicative can then simulate the notion of having multiple return values. (Which is where the non-determinism notion comes from)

So Applicative can be used to model a language with function application.

### Route Parsing in a web application using a DSL w/ Applicative

Example Route: `/foo/:bar/baz`

Routes have a notion typing. `:bar` has type String, but when we have mutiple types like `/foo/:bar/:baz` its of type (String,String).

We should be able to apply static analysis to routes.

Questions we should be able to answer

- Does the routing table contain redundant routes?

- Is a route a subroute of another?

```haskell
class (IsString (f ()), Applicative f) => Route f where
    match :: f String
```

So we want to be able to instantiate a parser with an appropriate type.

```haskell
route :: (Route f) => f (String, String)
route = (,) <$> ("foo" *> match) <*> match
```

This should route `/foo/:bar/:baz` properly.

Adding more members to the class can support more operations. Then we can do things like differentiate string matches from int matches. Other functionality for matching query params and stuff.

## Dynamic Languages - Monad

```haskell
class Applicative m => Monad (m :: * -> *) where
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    return :: a -> m a
    {-# MINIMAL (>>=) #-}
```

The applicative could only apply functions and perform those effects. Data dependencies between the arguments couldn't cause any effects. While thats good for static analysis, it limits a form of expressness.

### A filesystem DSL that gives users restricted access

```haskell
class (Monad m) => MonadFileSystem m where
    cd :: FilePath -> m ()
    ls :: m [(FilePath, FileType)]
    cat :: [FilePath] -> m String
```

This lets us use the DSL with do notation. This immediately expresses things we wouldn't be able to do in the Applicative.

```haskell
joinFiles :: (MonadFileSystem m) => m String
joinFiles = do
    files <- ls
    cat (map fst files)
```

We can think of this composition as sequencing with data dependency.

`MTL` has classes that act similarly to this for common things.

We can also use a fake filesystem for testing.

```haskell
data FakeFS a = FakeFS (Zipper -> (a, Zipper))

data FS = FS { files :: [(FilePath, String)], directories :: [(FilePath, FS)] } deriving (Show)

data Zipper = Zipper FS [FS]

instance Functor FakeFS
instance Applicative FakeFS
instance Monad FakeFS
instance MonadFileSystem FakeFS
...
```

## Models of Lambda Calculus - HOAS

Define a DSL which embds the full simply-typed lambda calculus.

"The previous examples were of Final Tagless ~~~ Style (Oleg Kiselyov)."

```haskell
class HOAS f where
    ($$) :: f (a -> b) -> f a -> f b
    lam :: (f a -> f b) -> f (a -> b)
```

Untyped Lambda Calculus
```
type Name = String

data ExprLC = Var Name
            | Lam Name ExprLC
            | App ExprLC ExprLC
```

Similar to the other abstractions, we can extend this with stuff like true/false/if/case as if it were any other dang ol language.

With this typeclass the variables are baked in via the host language.

One thing to note is that HOAS typeclass does not have a pure to lift values. We can interpret HOAS in many ways, `f a` could just be a regular haskell value.

```haskell
konst :: (HOAS f) => f ( a -> b -> a)
konst = lam $ \a -> lam $ const a

app :: (HOAS f) => f (a -> (a -> b) -> b)
app = lam $ \a -> lam $ \f -> f $$ a
```

[Code Gen for Javascript using this construction](https://youtu.be/8DdyWgRYEeI?t=4211).


# [Higher-order Abstract Syntax for Cartesian Closed Categories](https://blog.functorial.com/posts/2017-10-08-HOAS-CCCs.html)
