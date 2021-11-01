# [Lambda Jam 2015 - Conal Elliott - The Essence and Origins of Functional Reactive Programming](https://www.youtube.com/watch?v=j3Q32brCUAI)

[Slides](https://github.com/conal/talk-2015-essence-and-origins-of-frp)

FRP's two fundamental properties

Precise, simple denotation (Elegant & Rigorous)
Continuous time (Natural & Composable)

Leading us to deterministic, continuous "concurrency".

NOTE: Most modern "FRP" systems have neither property.

FRP is not about:

- graphs

- updates and propagation

- streams (streams are discrete model not continuous)

- doing (imperative programming is about doing, declarative programming is being)

# Why (precise & simple) denotation?

Seperates specification from implementation.

Simple so we can reach conclusions. The specification can inform us wether the implementation is correct.

Precise so that our conclusions will be true. Leads to valid conclusions.

Denotations have elegant, functional-friendly style.

# Continuous & Infinite Time vs Discrete/Finite time

Same benefits as for space (vector graphics/fonts)

EX: Fonts used to be bitmap graphics and discrete. Then scalable graphics came along with Postscript and etc.

So image dsl's lay out images over 2d space, we can take that idea to the domain of 1d time for FRP.

Principle: Approximation/Pruning/Discretization too early can mess up composability. For example bitmap graphic rotations that aren't pi/2.

You end up wanting to discritize more finely or more coarsely because you either lose too much information or waste a lot of work.

See [Why Functional Programming Matters](https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf)

# [Semantics](https://youtu.be/j3Q32brCUAI?t=847)

Centeral abstract type: Behavior a -- a "flow" of values

Precise & simple semantics:

```haskell

mu :: Behavior a -> (T -> a) --Yields a function over time
```

where T = Reals
