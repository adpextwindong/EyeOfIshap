- [The Curry Howard Correspondence](https://www.youtube.com/watch?v=GdcOy6zVFC4)

Course material for [The Curry Howard Correspondence lecture](https://cs3110.github.io/textbook/chapters/adv/curry-howard.html)

-- [ACT I - Types = Propositions](https://youtu.be/GdcOy6zVFC4?t=77)

Three innocent functions.

\begin{code}
import qualified Prelude

apply :: (a -> b) -> a -> b
apply f x = f x

konst :: a -> (b -> a)
konst x = \_ -> x

subst :: (a -> b -> c) -> (a -> b) -> a -> c
subst x y z = x z (y z)

s = subst
k = konst
i = subst konst

\end{code}

Keeping the types in mind.

apply :: (a -> b) -> a -> b
konst :: a -> (b -> a)
subst :: (a -> b -> c) -> (a -> b) -> a -> c

Three innocent ~~functions~~ propositions. (Replace the arrows with implications and paren nicely).

(A => B) => A => B
A => (B => A)
(A => (B => C)) => ((A => B) => (A => C))

Do you recognize these propositions?

[Halpern slide from cs2800 2016fa](https://www.cs.cornell.edu/courses/cs2800/2016fa/)

---A Sound and Complete Axiomatization for Propositional Logic Slide

```
Consider the following axiom schemes:

    A1. A => (B => A)                              |
    A2. (A => (B => C)) => ((A => B) => (A => C))  |
    A3. ((A => B) => ((A => ¬B) => ¬A))

These are axioms schemes; each one encodes an infinite set of axioms

P => (Q => P), (P => R) => (Q => (P => R)) are instances of A1.

Theorem: A1, A2, A3 + _modus ponens_ give a sound and complete axiomatization for formulas in propositional logic involving only => and ¬.
```

Soundness: You can only prove true things

Completeness: You can prove all the true things

Modus Ponens

```
A => B
A
------
B
```

Looking back at the innocent propositions we can see that SKI is

(A => B) => A => B                         <- Modus Ponens as axiom
A => (B => A)                              <- A1
(A => (B => C)) => ((A => B) => (A => C))  <- A2

Logical propositions can be read as program types and vice versa

|Type|Proposition|
|-|-|
|Type Variable|Atomic proposition A|
|Function type ->|Implication =>|

\begin{code}

fst :: (a,b) -> a
fst (a,b) = a

snd :: (a,b) -> b
snd (a,b) = b

pair :: a -> b -> (a,b)
pair a b = (a,b)

tt :: () --Unit
tt = ()

\end{code}

```
Conjunction and truth

 (A ∧  B) => A
 (A ∧  B) => B
A => (B => (A ∧  B))
true

We can consider unit as true.

Consider the inhabitants of a type.

Void having 0 inhabitants. Unit has 1 value.

True is always guarrenteed to be true, corresponding to unit always being there.
False corresponds to void.

|Type|Proposition|
|-|-|
|Type Variable|Atomic Proposition A|
|Function Type ->|Implication =>|
|Product Type|Conjunction ∧|
|Unit|True|

Well typed programs correspond to a true logical formula.

-- [Act II - Progams = Proofs](https://youtu.be/GdcOy6zVFC4?t=1240)

Innocent Typing Rule

Recall [lec19](https://cs3110.github.io/textbook/chapters/interp/typecheck.html)
- Static environment is a map from identifiers to types
- Typing relation `env |- e : t` says that e has type t in environment env

"Env shows that e has type t"

Typing rule for function application:

if   env |- e1 : t -> u
and  env |- e2 : t
then env |- e1 e2 : u

"If in an environment we can show that e1 is a function type from t to u, and show in the same environment we can show e2 is type t, then the environment shows that e2 applied to e1 results in type u."

This is just like Modus Ponens.

t => u
t
------
u

--- Intermission

Logical proof systems

Ways of formalizing what is _provable_
Which may differ from what is _true_ or _decidable_

Two styles:
- Hilbert:
    Lots of Axioms
    Few inference rules (maybe just modus ponens)

- Gentzen
    Lots of inferences rules (a couple for each operator)
    Few Axioms

Inference Rules

P1 P2 ... Pn
------------
     Q

From premises P1, P2, ... Pn
Infer conclusion Q
Express allowed means of inference or deductive reasoning
Axiom is an inference rule with zero premises

In the type system thesThe ternary relation

https://youtu.be/GdcOy6zVFC4?t=1549
