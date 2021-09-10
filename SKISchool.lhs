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

TODO the K S f ( K f ) g h middle step

So this leads us to use S again
```
S (K S) K f g h
```
TODO finish writing up the notes.

[TODO notes on Combinatory Logic](https://en.wikipedia.org/wiki/Combinatory_logic)