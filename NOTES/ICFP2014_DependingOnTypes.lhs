- ICFP 2014: Depending on Types - Stephanie Weirich](https://www.youtube.com/watch?v=rhWMhTjQzsU

[DTH Repo](https://github.com/sweirich/dth/blob/master/depending-on-types/Okasaki.hs)

-- Red Black Trees

Invariants:
    - Height
    - Color
        - You can't have two red nodes in a row
        - Root to leaf is the same number of black nodes

From [dth Okasaki.hs](https://github.com/sweirich/dth/blob/master/depending-on-types/Okasaki.hs)

```haskell
{-# LANGUAGE KindSignatures, GADTs #-}
-- From Okasaki 1993 functional pearl
data A = A1 | A2 | A3 deriving (Eq, Ord)

data Color = R | B

data Tree :: * where
    E :: Tree
    T :: Color -> Tree -> A -> Tree -> Tree


-- ins may violate invariant by creating a red node with a red child
-- two fixes:
--     - blacken result, so that root is black
--     - rebalance

-- So we have to blacken the root node, and rebalance the tree, after inserting on the correct leaf.

insert :: Tree -> A -> Tree
insert s x = blacken (ins s)
    where ins E = T R E x E
          ins s@(T color a y b)
            | x < y     = balance color (ins a) y b
            | x > y     = balance color a y (ins b)
            | otherwise = s
          blacken (T _ a x b) = T B a x b
          blacken _ = error "impossible"

--Pattern match on consecutive red trees, the natural unbalanced cases
balance :: Color -> Tree -> A -> Tree -> Tree
--Left Left consecutive red
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
--Left Right consecutive red
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
--Right Left consecutive red
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
--Right Right consecutive red
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
--Leaves other trees alone
balance color a x b = T color a x b
```

In the Licata Agda example Tree is an indexed datatype using Color and N (PeanoNat) with two seperate constructors for a red tree and black tree, TR and TB.

Red trees enforce the color invariant by only accepting black subtrees.

Black trees can accept any color subtrees, but its height is Succ n of it its n height subtrees.

The empty tree is Black with Zero height.

--Note: Agda doesn't distinguish between types and terms.
--      So curly braces are used to idicate inferred arguments.

\begin{code}
{-# LANGUAGE DataKinds, KindSignatures, GADTs #-}

data Color :: * where -- Color is both a type and a kind,
    R :: Color        -- R and B can appear in both the term language and type language
    B :: Color

data Nat = Z | S Nat

--Explicit kind signature is needed or else E causes
--Expected kind `Color -> Nat -> *', but `Tree' has kind `*'

data Tree :: Color -> Nat -> * where
    E  :: Tree B Z
    TR :: Tree B n -> a -> Tree B n -> Tree R n
    TB :: Tree c1 n -> a -> Tree c2 n -> Tree B (S n)

\end{code}

Types are special in Haskell (as opposed to Agda)
- Type arguments are always inferred (HM type inferencE)
- Only types can be used as indices to GADTs, in Agda types and terms are the same and can index the same.
- Types are always erased before run-time

-- History

Datatype promotion was introduced in GHC 7.4 (Feb 2012). Also introduced Kind-Polymorphism because List Cons is polymorphic for types and kinds (promoted types).

We couldn't promote things that are indexed (GADTs). So Tree couldn't be promoted.

GADTs were introduced in GHC 6.4 (March 2005).

The current RBT type can't hold intermediate unbalanced trees. We can have a seperate type that represents a consecutive red red tree at its top. We will also need another type for valid non empty trees of unknown color with potential violations.


-- The GHC version

This is the first instance of type families and singletons in the [talk](https://youtu.be/rhWMhTjQzsU?t=1934).

Singleton handles the type erasure issue. (Uses a GADT to create a constraint between some type level data and runtime term data)

Sing R only has one non-bottom (⊥) element, SR. Same with Sing B and SB. You can then pattern match on a type variable Sing c to find out what C is, R or B.

```haskell
type family Incr (c :: Color) (n :: Nat) :: Nat where
    Incr R n = n
    Incr B n = Suc n

data Sing :: Color -> * where
    SR :: Sing R
    SB :: Sing B

data AlmostTree :: Nat -> * where
    AT :: Sing c -> Tree c1 n -> A -> Tree c2 n -> AlmostTree (Incr c n)
```

Singleton types are an old idea [Hayashi 1991][Xi, Pfenning 1998]

When you have seperation between the indices of indexed types and the expression language, singletons are the trick.

Theres a proof that it can expressive as a full-spectrum language
[Monnier, Haguenauer, PLPV 2010]

You can take this function type from Agda and take it to a language without real π-types.
```
(x : A) -> B => forall (x :: A). Sing x -> B
```
Model π-types with parametric polymorphism and singletons. Haskell still can't do all of this. Certain types are inexpressible because of datatype promotion restrictions (on promoting GADTs) [Eisenberg, Weirich; HS 2012].

Programming with a lot of singletons can become painful [Lindley, McBride; HS 2013]

NOTE: Type families are not functions

More restricitve
- No lambdas (must be named, can't be anonymous)
- Application must be saturated (They're not really higher order, no partial application)
- Restrictions on unifications

Brutal, ghc doesn't do any reasoning wether the type families is injective so we have to assume the worst.

More Expressive
- Can pattern match types, not just data
- Equality testing is avalible for any kind

```haskell
type family Item (a :: *) where
    Item Text = Char
    Item [a] = a

type family Eq (a :: k) (b :: k) :: Bool where
    Eq a a = True
    Eq a b = False
```

Pattrick Bahr "Nicer version of data types ala carte"

A delete function is a nice problem set for this using dependent types.
