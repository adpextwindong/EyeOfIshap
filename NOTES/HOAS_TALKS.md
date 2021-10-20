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
However implementing `beside` requires a nice way to track the document width so we must associate some sort of data with it to do this. Leading us to:

```haskell
data PlainText = PlainText
    { docuWidth :: Int
    , docLines  :: [String]
    }
```

This type isn't necesarrily the best given a purpose. If we wanted to simulate text fitting to answer whether the composition fits in a region, we can just omit docLines and use just a docuWidth containing instance. Fundamentally however we've given ourselves a typeclass to reason about around.Add to gitignor

## [Static Languages - Applicative](https://youtu.be/8DdyWgRYEeI?t=1017)
