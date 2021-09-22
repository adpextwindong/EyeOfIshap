[SKI School: The Combinator Calculus Demystified](https://www.youtube.com/watch?v=FC6kl_kLFEo)

A combinator operates on a function to produce another function.

The SKI calculus has three constructs.
| Construct | Description | Lambda Calculus Equivilant |
| K | Constant | λx.λy.x |
| I | Identity | λx.x | 
| S | Substitution | λx.λy.λz.xz(yz) |

In Haskell these can be expressed as:

% \begin{code}
% k = \x -> \y -> x
% i = id
% s = \x -> \y -> \z -> (x z) (y z)
% \end{code}

Alternatively:

\begin{code}
k x _ = x
i = id
s x y z = (x z) (y z)
\end{code}

Note: The SKI Calculus may be percieved as a reduced version of untyped lambda calculus. In combinatorial logic everytrhing is a function.

K and I are intuitive. I is just the identity. K is the Constant function so it tosses the 2nd input.

S on the otherhand stands out as trickier. Its 3rd argument gets distributed to its other arguments.

-- Reverse Engineering Function Composition

For example to achieve function composition we utilize S and K to compose E1 E2 together.

Initially you might gravitate towards the idea of loading S with f g to apply to h. This leads to:

```
S f g h
f h (g h)       -- Apply S
```

Well f can't really deal with h, so we need to figure out how to utilize K to get around this.
```
K f h (g h)
f (g h)         -- Apply K
```

Great, so how can we utilize S to get us the form "K h f (g h)" given expressions f and g to apply to h?

Well lets think of f and g as expressions and f as the function we're combinating on first. Then:

```
S (K E1) E2 f   
K E1 f (E2 f)   -- Apply S, distributes the f
E1 (E2 f)       -- Apply K, tosses the f and now there are no more redexes..
```

Well we need this in a form where we can supply the expressions and the function.
```
S ... f g h
```

Working backwards from:

```
K f h (g h)
```

we can think about how h is common to both (g h) and the left subexpression (K f h). If we look towards S we could imagine supplying (K f) to it to distribute h to (K f) and (g). Leading us to:

```
S (K f) g h
```

Great. We're in a form to supply g and h as arguments on the right side and now we just need f in the same position. This next bit might seem like a jump. We have f nearly in place to be supplied with g and h directly, but we need to replace the ... portion.

```
GOAL: S ... K f g h
UNKNOWN: ~~~~~
HAVE: S (K f) g h
```

Considering S is the only tool we have really, lets work out a term for (S _ K f) that will get us some unknown to evaluate to what we have.

Any term in this underscored position (the X argument to S) will be supplied f from the original S and must evaluate to S.

```
S (_) K f ...
_ f (K f) ...
S (K f) ...
```

Well considering we don't want to use the leftmost f in the middle step why don't we use K to ignore it? We can supply K an S to get us S in the leftmost on the 3rd row anyways.

```
S (K S) K f g h
K S f (K f) g h
S (K f) g h
K f h (g h)
f (g h)
```

This means the dot operation, composition, is achieved with:

```
S (K S) K
```

[TODO notes on Combinatory Logic](https://en.wikipedia.org/wiki/Combinatory_logic)