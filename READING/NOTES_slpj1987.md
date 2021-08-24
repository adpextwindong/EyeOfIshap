# Intro

Implemting functional programming languages using _lazy graph reduction_

1.
- Translation from high level functional language into Lambda Calculus
- Pattern Matching
- Type Checking

2.
- Refinements and alternatives such as:
- Super Combinators
- Full laziness
- SK combinators

3.
- G-machine

The functional programming languages predating 1987:

- SASL [Turner, 1976]()

- ML [Gorden et al., 1979]()

- KRC [Turner, 1982]()

- Hope [Burstall et al., 1980]()

- Ponder [Fairbairn, 1985]()

- LML [Augustsson, 1984]()

- Miranda [Turner, 1985]()

- Orwell [Wadler, 1985]()

Those with non strict semmantics being SASL, KRC, Ponder, LML, Miranda and Orwell.

ML and Hope being the strict languages in this group.


A super set of Lambda Calculus called _enriched lambda calculus_ will be used to specifically allow a straightfoward translation of a Miranda program into an expression in the enriched lambda calculus. This will be shown in chapter 3.
