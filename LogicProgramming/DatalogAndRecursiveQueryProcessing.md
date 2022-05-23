# [Datalog and Recursive Query Processing](http://blogs.evergreen.edu/sosw/files/2014/04/Green-Vol5-DBS-017.pdf)

Snippets:

In recent years, however, Datalog has reemerged at the center of a
wide range of new applications, including data integration [68, 43, 50],
declarative networking [80, 77, 75], program analysis [29], information
extraction [110], network monitoring [10], security [85, 60], optimizations [73], and cloud computing [15, 16].

 Ramakrishnan and Ullman [100] provides a high-level overview
 of the Datalog language, query evaluation and optimizations, and
 more advanced topics on negation and aggregation in a few pages.
 This should be viewed as a “quick-starter” guide for someone
 exposed to Datalog for the first time.

 Intuitively, rule evaluation can be understood as the repeated application of rules over existing tuples to derive new tuples, until no
 more new tuples can be derived (i.e. evaluation has reached a fixpoint).

  Prolog [112] uses a goal-oriented backward-chaining
  approach, starting from the goal (i.e. query), and then expanding the
  rule bodies in a top-down fashion.

  bottom-up evaluation technique,
  where existing facts are used as input to rule bodies to derive new
  facts. A fixpoint is reached when no new facts are derived

  a bottom-up evaluation
  approach used in Datalog allows us to draw upon a wealth of query
  processing and optimization techniques to draw upon for doing the
  computations efficiently even when datasets are too large to fit in main
  memory.

   A Datalog rule must also satisfy the range restriction property,
   which says that every variable occurring in the head of a rule must
   also occur in a predicate atom in the body, or be equivalent (modulo
   the comparisons in the body) to a variable occurring in a predicate
   atom in the body. Note that a rule may have an empty body (when
   n = 0).

Core Datalog is incapable of expressing non-monotone queries, for example, the query which retrieves all unreachable pairs of nodes in a graph. In fact, it is known that there are even monotone, polynomial-time computation not expressible in core Datalog [14]

## Negation

_Stratified negation_ which disallows recursion through negation.

```datalog
rechable(X,Y) :- link(X,Y).
reachable(X,Y) ;- link(X,Z), reachable(Z,Y).
indirect(X,Y) :- reachable(X,Y), not link(X,Y).
```

Notice how indirect diverts the negation away from reachable, acting as an auxilary predicate for reachable without letting negation effect it.

We require also a safety condition which says that every variable in
the body of a rule must occur in at least one positive (i.e., not negated)
atom, as in the example above. This is required to ensure that the
results of programs are finite and that their results depend only on the
actual contents of the database. (In database theory, this is formalized
as the notion of domain independence [12].)

As for core Datalog, evaluation of semipositive Datalog¬ programs can be done in polynomial time in the size
of the database instance

A Datalog¬ program P is stratifiable iff its precedence
graph GP has no cycle containing an edge labeled ¬

Example 2.8, Note that although the graph has a cycle, the cycle does not go through
the edge labeled ¬, so the program is stratifiable. O
n

In order to safely rule out this and other such pathological cases, we
restrict our attention here to cases where aggregation does not occur
through recursion. In analogy with stratified negation, these are the
so-called aggregate stratified [90] programs

TODO 3.1 onwards
