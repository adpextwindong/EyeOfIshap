# CH1 Sets,Functions, and Relations

```
In elementary calculus, the definite integral from a to b is a functionwhose domain is the set of all real-valued functions that are Riemann integrable on the closed interval [a, b], and whose range is the set of all real numbers.
```

Side notes:
For example a non Riemann integrable function could be:

g(x) = { 1 if x is Rational,
         0 if x is Irrational } -- Dirichlet function
--This is Lebesgue integrable however

Exercises

The following exercises will assume that the sets involved are subsets of the real numbers.

1. List explicitly the elements of the set {x: x < 0 and (x - 1)(x + 2)(x + 3) = 0}

{-2,-3}

2. List the elements of the following set. {x:3x - 1 is a multiple of 3}

{4/3 + x | x <- [1..]}

3. Sketch on a number line each of hte following sets.

{x:|x-1| <= 3}
[-2,4]

{x:|x - 1|<= 3 and |x| <= 2}
[-2,2]

{x:|x - 1| <= 3 or |x| <= 2}
[-2,4]


# [Richard Southwell - Point-Set Topology 1: Open and Closed Sets](https://www.youtube.com/wastch?v=kmMiZHSvON)

Notation: 2^X will be the powerset.

Let X be a non empty set. Now Tau, a collection of subsets of X, is a topology on X if three conditions hold.

Conditions for a toplogy
1. X, empty set are both members of Tau.
2. If S is a collection of subsets of Tau, then the union of those members is also a member of Tau.

The union of elements of a topology are also in the topology.

3. For any pair of elements in the topology, their intersection is in the topology.

forall A B in Tau, A intersection B is in the topology.

Once Tau satisfies these three conditions (X, Tau) is a topological space.

## DEF Open Sets
As short hand, we call the members of Tau Open Sets.

## Alt 3rd condition
We can write the third condition as: The intersection of any finite subset of Tau belongs to Tau.

Every finite subset can be written as the repeated intersection of open set pairs. Thus if these open set pairs are in Tau, it follows from the forall pairs statement that any finite subset of Tau intersected belongs to Tau by induction.

A subtle asymmetry emerges from intersections of open sets involving finite subsets and unions dealing with subsets...

## Examples of Topologies

### Discrete Topology
Tau = Powerset of X is the discrete topology on non empty set X
(X, 2^X) the discrete space.

Proposition 1 (X,T) such that that all singletons of X are open sets in Tau, then T is a discrete topology.

Proof: forall subsets of X, it can be written as the union of singletons from X, which are open sets, thus making S an open set in Tau, via condition 2 of the Topology definition.

### Indiscrete Topology

Tau = {empty set, X}
(X,T) is the indiscrete topology on X/= empty set

This has the property of having the least possible open sets, as opposed to the discrete topology which has the most.

--Closed Sets

Let (X,T) be a topological space,

now S subset of X, is closed when X - S is open.

--Prop 2

If (X,T) is a topolical space

1. Empty set, X are closed sets.

X - X = Empty set, which is in Tau
X - Empty Set = X, which is in Tau.

2. The intersection of any collection of closed sets is closed.

For any S collection belong to 2^X, forall S in A, s is closed.

X - S is open, by def 2 of open the union of all s from A U(X-S) is open. Thus X - intersections S for all S from A is an open set.

This implies The intersection S is closed because its complement is open.

3. The union of any finite collection of closed sets is closed.

Similarily to 2 we can take the intersection of its complements and use the open set definition.

This set of finite collection of closed sets intersected can be written as

(X - S1) /\ (X - S2) /\ ... (X - Sn) which is open

= X - (S1 \/ S2 \/ S3 ... \/ Sn). Thusly the union of open sets is open, making its complement closed which is the union of these finite collection of closed sets.

### Finite Closed Topology

Let X be a nonempty set Tau on X, is called the finite closed topology if the closed sets of X are X and all finite subsets of X.

### Pitfall example

The evens in natural numbers is not a closed set because the complement is not finite.

### T0 space

(X,T) is T0 when forall distinct a,b in X, a /= b,
There exists S in T : a in S, b not in S
                   or a not in S, b in S.

We can distinguish between a and b using this open set that exists.

### T1 Space

(X,T) is T1, if each {x} singleton subset is closed, forall x in X.

Exercise: Show T1 implies T0.

# [Topology | Math History | NJ Wildberger](https://www.youtube.com/watch?v=aaXk23JHFck)

Topology is the study of (continuous) shapes.

Especially interested in properties unchanged by continuous deformation.

For example deforming a sphere (S2) into a dumbell without introducing any cuts/pinches/discontinuities results in an homeomorphic (equal) shape.

--Note: The circle being S1.

A discrete aspect of topology is that we can turn a sphere into a polyhedron. So topology can span this continuous and discrete/combinatorial aspect.

## Euler Characteristic of a polyhedron P - 1752

An important concept/invariant:

    X ≡ V - E + F

V = Number of Vertices
E = Number of Edges
F = Number of Faces

ex: Cube Euler Characteristic

Cube = 8 - 12 + 6
Cube = 2

ex: Tetrahedron Euler Characteristic

Tetrahedron = 4 - 6 + 4
Tetrahedron = 2

Thereom: polyhedron P is homemorphic to S2 (Sphere), then X(P) = 2 (It's Euler Characteristic)

This is an example of a topological invariant.

What about the Torus? What are polyhedral objects homeomorphic to it?

Cube with a square hole drilled through it.

Theorem: If Polyhedron P ≃ Torus, then X(P) = 0.

Henri Poincaré (1895) clarified the topological aspect of the Euler Characteristic.

Suppose we have a sphere, and we decompose it into a lot of little faces. Any two of them meet along a vertex/edge. A polygonal map to compute the Euler charactersitic.

X = V - E + F

Suppose we add a vertex to an edge:
    V := V + 1
    E := E + 1
    F := F

X is unchanged. (The additions cancel.)

Supposed we add an edge from one vertex to another (unconnected vertex)
    E := E + 1
    F := F + 1
    V := V

X is unchanged. (The additions cancel.)

Thusly subdividing this polygonal map further and further does not change the Euler Characteristic.

--Two different polygonal maps of a sphere can agree to a common refinement (triangulation/subdivisions) the Euler characterstic will obviously be the same and the shapes are the same to begin with. This means the Euler characteristic is independent of the map and depends on the surface.

## Descartes (Letter to Leibnitz 1676) studied curvature of Polyhedra