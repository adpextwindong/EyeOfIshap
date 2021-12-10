# FRP Notes

## [Genuinely Functional GUIs](http://conal.net/papers/genuinely-functional-guis.pdf)

Arrowized FRP (AFRP) is based on two ideas:

- _signals_ which are functions from real-valued time to value
- _signal transformers_ which are functions from signals to signals

A newtype constructor ST is introduced for signal transformers

```haskell
newtype ST a b = ...
```

This provides a number of primitive signal transformers and a set of combinators (the arrow combinators) for assmebling new signal transformers from existing ones.

Since ST is a type constructor, signal transformers are first-class values.

Signals are not first-class _values_.

```haskell
Signal a = Time -> a
```

We outlaw signals as first-class values for two reasons.

Signals alone are inherently non-modular: We can apply point-wise transofmrations to the observable output of a signal, fist-class signal values do not have an input signal.

Signal transformers have _BOTH_ an input signal and an output signal, enabling us to transform both aspects of an ST value.

ST as first-class values guarantees that every signal in the program is always _relative_ to some input signal.

Secondly, signals as first class values inevitably leads to space time leaks. Due to the implementation needing the complete time-history of a signal to compute one sample. Structural induction on the ST type avoids t his.

### Arrows

[Hughes -  Generalizing Monads to Arrows](http://www.cse.chalmers.se/~rjmh/Papers/arrows.pdf)

```haskell
class Arrow a where
    arr :: (b -> c) -> a b c
    (>>>) :: a b c -> a c d -> a b d
    first :: a b c -> a (b,d) (c,d)
```

#### Lifting

Arr lifts a function into a signal transformer that maps `Signal b` to `Signal c` by applying f point-wise to the input signal.

Formally, we define `arr f` for signal transformers as follows

```
[arr f] = λs : Signal α. λt : Time . [f](s(t))
```

#### Serial Composition

The (>>>) operator composes two arrows.

```
fa :: ST b c
ga :: ST c d

fa >>> ga :: ST b d
```

Formally, we define serial composition for signal transformers as reverse function composition:

[fa >>> ga] = ([ga] . [fa])

#### Widening

The first operator widens an arrow from b to c, to (b,d) to (c,d) for all types d.
Like a pair of signals is produced.

```
[first fa] = λs : Signal (β x γ) . pairZ ([fa] (fstZ s)) (sndZ s)
```

where fstZ, sndZ and pairZ are the obvious projection and pairing functions for signals of pairs.

#### ArrowLoop

Pairs the second half of fa's output signal with an external input signal to form fa's input signal

```haskell
class Arrow a => ArrowLoop a where
    loop :: a (b,d) (c,d) -> a b c
```

Formally, we define loop for signal transformers as:

```
[loop fa] = λs : Signal β. fstZ(Y(λr.[fa](pairZ s (sndZ r))))
```

where Y is the standard least fixed point operator.

#### Discrete Events

```haskell
EventSource α = Signal (Maybe α)
              = Time -> (Maybe α)
```

### Primitive Signal Transformers

## [Osabe](https://osabe-app.bitbucket.io/2021-12-07_frp.html)

Osabe makes use of a delayed switch function

```haskell
dSwitch :: Monad m => Wire m i (o, Event (Wire m i o)) -> Wire m i o
```

"Takes a wire as an argument that it uses to process an input stream, emitting the regular stream-output values issue by the wire as its own output. It does this up until there is an Event output from the wire, after which it switches to behave like the Wire carried as the event payload."

[Mun Hon Chegon's "Functional Programming and 3D Games" thesis](https://web.archive.org/web/20170706075201/http://www.cse.unsw.edu.au/~pls/thesis/munc-thesis.pdf) also talks about "delayed parallel switchin functions" with [dpSwitch](https://hackage.haskell.org/package/Yampa-0.13.3/docs/FRP-Yampa-Switches.html#v:dpSwitch)

```haskell
dpSwitch :: Functor col =>
    (forall sf . (a -> col sf -> col (b, sf)))  -- Routing Function
  -> col (SF b c)                               -- Reactive objects collections
  -> SF (a, col c) (Event d)                    -- Update functor for objects in response to events from object
  -> (col (SF b c) -> d -> SF a (col c))        -- Continuation of the game after a parallel switch occurs
  -> SF a (col c)
```

In the space invaders example KillAndSpawn observes the outputs after input has been applied. When an object has to be added or removed it would produce a switching event that invokes the 4th arg, which is the continuation of the game.

Listing 2.13 shows sugar for Arrow syntax that makes things more readable in Yampa.

From "Functional Reactive Programming, Refactored", Dunai can do Yampa's time arrowized frp with MSF by implemting

```
type SF a b = DTime -> a -> (b, SF a b)
```

as

```
type SF a b = MSF (Reader DTime) a b
```

then we need `integral` which integrates the input signal.

WriterT can be used with an MSF to log things without running in IO.
