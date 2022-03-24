# [Algebra of Programming - Richard Bird & Oege de Moor](https://themattchan.com/docs/algprog.pdf)

## Notation

```
f : B <- A
g : C <- B

f . g : A <- C

(f . g)x = f(gx)
```

The notation used will be slightly reversed compared to Haskell.

Uncurried functions will be used as well.

## Chapter 1
```haskell
data Nat = Zero | Succ Nat

plus :: Nat -> Nat -> Nat
plus m Zero = m
plus m (Succ n) = Succ $ plus m n
```

Gets expressed as:

```
Nat ::= zero | succ Nat

plus (m, zero)   = m
plus (m, succ n) = succ (plus (m, n))
```

Structural recursion vs primitive recursion

```
Much of this book is devoted to understanding and exploiting the idea of defining a function (or, more generally, a relation) by structural recursion over a datatype.
```

`foldn (c,h)`, the fold operator for type Nat, describes a homomorphism of Nat.

```Commonplace
It is a fact that not every computable function over the natural numbers can be described using structural recursion, so certainly some functional programs are inaccessible if only structural recursion is allowed.
```

### Exercises

#### 1.1

A recursive equation not satisfied by any equation

```
f x = f (f x)
```
#### 1.2

```
m (x,y) = y + 1                  if x = y
        = m(x,m(x -1, y + 1)),   otherwise.
```

```haskell
m x y | x == y = y + 1
      | otherwise = m x (m (x - 1) (y + 1))
```

This fails to terminate for any pair x,y not equal. Each recursive call to the right, if is to terminate, introduces constraint on x and y that conflict with previous levels of recursion's constraints.
