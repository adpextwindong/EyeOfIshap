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

--Nonstandard Rational Turn Angle Version

--Rescale 90 degrees as 1/4, 180 = 1/2, 360 = 1. -90deg = -1/4

Sum of the turn angles in a triangle = 1/2

A bunch of lines intersecting to one vertex, all sum to 1.

### Cube

Supposed given a cube and we're studying a vertex X.

Three faces come together at X, and have 3 angles at X. 1/4, 1/4, 1/4, sum to 3/4.

Curvature of P at X = 1 - sum of turn angles at X.

So the curvature of the cube at X = 1/4.

### Tetrahedron

Tetrahedron? Curvature at X = 1 - (1/6 + 1/6 + 1/6) = 1/2. --(Tetrahedron turn angle = 1/3rd of 1/2 = 1/6)

### Total Curvature
What about the total curvature? Cube: 8 * 1/4 = 2
                                Tetrahedron = 4 * 1/2 = 2

The total curvature is always the Euler characterstic.

--Cube with the hole drilled out will have curvature totalling to 0.
--NOTE Negative curvature can occur too.

Example: two holed torus, three holed torus, ... g holed torus.
Genus = # Holes

Theorem. X(g hole'd torus) = 2 - 2g

## History of multi-holed tori

These multi-holed tori came up from the theory of complex functions/complex analysis. Thanks to Bernhard Riemann (1826 - 1866).

### Complex Functions

f: C -> C

Riemann's line of thinking: Instead of thinking about the complex plane in the usual planar way, how about a sphere?

Place a sphere at the origin, of radius 1.

A correspondence between the plane and sphere can be make. The plane being at the equater of this sphere.

We can view things from the north pole of the sphere.

For every point Z in the plane, we can draw a line from the north pole N to Z. This line will meet the sphere at a second point Phi(Z). This maps points outside the unit circle in the plane onto the sphere (northern hemisphere in this case).

The unit circle gets mapped to the equater. The North Pole N is mapped to infinity (I'm assuming no point in Z will cause Phi(Z) = N).

S2 ≃ Rieman Sphere ≃ C ∪ {∞}

A nicety of this is that the sphere is compact, it doesn't go on forever. Points far far away are just close to the north pole. The whole plane now fits on this sphere.

f(Z) = Z^2
f(∞) = ∞

Extends to the Riemann Sphere

Riemann was interested in the inverse of this kind of function f.

f(Z) = sqrt(Z) ?

Well this is a double-valued function. For example we bisect the angle to the plane but the opposite value is a sqrt.

--In a less rigorous setting, looking at a parabola y = sqrt(x), in the reals typically we'd toss the lower half and just look at the upper half. In the complex plane this approach fails to make sense because for any Z it would have opposite values in places that might be worth considering. We can't use this ad-hoc approach anymore.

Riemann looked at z and its square root when "dragging" Z around the unit circle. He observed that you need to get Z around twice to see the square root function properly.

### Riemann Onion
To conduct this with the Riemann sphere idea, Riemann came up with layering two Riemann spheres within each other. In the plane example, we navigated around the unit circle. To achieve this "going around twice bit" we can navigate around the sphere (like along the unit circle) then slicing the outer sphere (vertically), then slicing the inner sphere and stitching the outer sphere to the inner sphere. Then navigate around and stitch the inner sphere back to the outer sphere.

Instead of this Onion idea Riemann saw that topologically its the same as taking two spheres with a hole in them and gluing them together into a dumbbell shape. But this leads us homemorphically back to a sphere.

What about?

y^2 = z(z-a)(z-b)
y = sqrt(z(z-a)(z-b))

Well then you need to cut two holes in the sphere, 0 to alpha, B to inf in the two spheres and stitch them together. Then we get a Torus.

So a Cubic curve in the complex plane gets associated to a torus.

### Clasification of 2 dimensional surfaces

X = 2,  X = 0, X = -2,       X = -4
Sphere, Torus, 2-hole torus, 3-hole torus, ...

These all the compact orrientable surfaces.

--Orrientable, if you're an ant on the surface you can distinguish between left handed and right handed ants.

Mobius discovered a surface that doesn't have this orrientable property.

A mobius band (mobius strip) is an example.

This surface is non-orrientable surface. Theres no sense of lefthand/righthand on this surface.

When considering the edge of a mobius strip, we notice it has one single contious ege going around the whole thing.

--Sewing a disc and a mobius strip together
--Sewing a disc of cloth onto a mobius doesn't work in 3d space as it'll get in its own way. Apparently in 4d space its possible and its a projective plane.

This has X = 1

Other non-orrientable surfaces:

Take a torus and add a crosscap to it. Same with 2hole and the others.
These have  X = -1, X = -2 ....

Dehn & Heegaard (1907)

### The Sphere

The sphere is distinguished (in contrast to the other 2d surfaces) in that its simply connected.

Any closed curve drawn on the sphere can be shrunk continiously on the surface down to a point.

--For the torus you have two types of curves, one where the torus hole prevents you, and another (one facing outwards on the surface for a lack of a picture) that can get shrunk to a point.
--For the sphere nothing is getting in the way of a loop on the curface from shrinking into a point. The torus hole however stops this.

## Poincaré Conjecture

S3 is the only simply connected 3 dimensional surface.

Grigori Perelman proved this in 2002, building on Richard S. Hamilton's work of Ricci flow.

# [Tutorial 1: Topology (International Winter School on Gravity and Light 2015)](https://www.youtube.com/watch?v=_XkhZQ-hNLs&list=WL)

a)
    - A topological space is a set "equipped" with a topology. A pair (X,T).
    - This topology then gives forth a notion of open sets within the topological space.
    - "Always has integer dimension" isn't something we can talk about just from the notion of a topological space. You can talk about the members of the set but that is not about the topological space per se.

-- A topological space is the minimal structure required to define the continuity of a map.

[An initial example](https://youtu.be/7G4SqIboeig?list=PL_-fOl2l5yUhQSPWkt-lAs7fm0SGQkYgb&t=3038)

A continuous map being a continuous function between two topological spaces. [We're concerned with the pre-images of f on the open sets being open sets.]

[Def: A map F : X -> Y is continuous iff the preimage of any open set is open.](https://mathworld.wolfram.com/ContinuousMap.html)

b) The chaotic topology on a set M, {Empty Set, M}
    - The empty set of course exists in the chaotic topology by definition but also by the first axiom of a topology.

    - "It is the coarsest topology on M"

    Define: A topology O2 is coarser than a topology O1 if O2 is contained in O1. (This 'containing' doesn't define a total order because there can be topologies that aren't subsets of eachother. Thusly it defines a partial order)

    Thus by this definition it is the coarsest topology on M.

    "The chaotic topology on a set M, makes all maps f : N -> M continuous, where the domain may carry an arbitrary topology"

    If we look at the preimage of open sets in M (Empty set and M) are open in N.

    The preimage of the empty set being the empty set, and the preimage of M is the set N. Which are both open in a topological space in N. Thus by the definition of continuity this statement is true.

## c) Consider a map f: M -> N between topological spaces (M,Om) and (N, On)

--Undergraduate analysis notion of continuity - the Epsilon Delta criteria.

We need to have an understanding of the Codomain for any map as well as the domain for the map as the open sets in the codomain and their pre-images being open sets is the criteria for continuity.

"Choosing the discrete topology on M makes all maps from M to N makes all maps from M to N continuous."

True. All preimages on the open sets in N will be some member of the power set M, by the definition of the discrete topology, therefore be in the topology making it the preimages open sets. Therefore making it continuous. In this case N can be any arbitrary topology.

## d) A subset U of M, of a topological space (M,O)
TODO play with some real number intervals and look at the definitions of Open and Closed sets again.
[CONT](https://youtu.be/_XkhZQ-hNLs?t=729)

# [Lecture 2: Topological Manifolds (International Winter School on Gravity and Light 2015)](https://www.youtube.com/watch?v=93f-ayezCqE)

Toplogical spaces: ∃ so many that mathematicians cannot even classify them.

Homeomorphism, invertible continuous maps from one topological space to another in both directions.
This map has to be a [bijection](https://en.wikipedia.org/wiki/Bijection) (one-to-one and onto).

For classical space time physics, we may focus on topological spaces (M,O) that can be 'charted', analogously to how the surface of the earth is charted in an atlas.

-Charts and Atlases

## Topological Manifolds

[Def. A topological space (M,O) is called a d-dimensional topological manifold](https://youtu.be/93f-ayezCqE?t=255)

if

∀ p ∈ M  : ∃ U ∈ O, open set U containing the point p : ∃ x : U -> x(U) ⊆ Rᵈ:

--For every point, there exists an open set in the topology, there exists an open set with a map that takes every point around the set of this point to a subset of Rd in an invertible one to one manner.

1. x invertible:
x⁻¹ : x(U) -> U
2. x continuous (We can get this by Rᵈ having the standard toplogy and u being a subset on M)

3. x⁻¹ continuous (x being one to one and onto isn't enough to show the inverse is continous. This was shown in the [first lecture](https://www.youtube.com/watch?v=7G4SqIboeig) TODO TIMESTAMP.)

[Example](https://youtu.be/93f-ayezCqE?t=611):

Doughnut shape = M ⊆ R³ we can give R³ the std topology and inherit on M a subset topology from R³.

Claim: This is a 2 dimensional topological manifold.

For any point in the set (on the surface of the doughnut),
    there exists an open subset U satisfies (we can an open set in R³ without the boundary and 
    intersect it with the doughnut, giving us the open set patch around x without the boundary.)
    [there exists a map from every point in this open set to some part of R² ](https://youtu.be/93f-ayezCqE?t=774)

We could do the same for the Earth and charts.

Eiffel Tower example

R² is simply R X R = {(m,n) |m, n ∈ R}.

So lets say (1,2) represents the foot of the Eiffel Tower with respect to this specific chart. Key part being specific, the publisher of the chart could swap the axes/ordering of the pair or scale the chart differently.

Terminology : (U,x) called a chart.

x(p) for the leg of the Eiffel tower (1,2)

For every point of the mainfold, there exists a chart that contains the point.

[2nd Example. Figure 8 wire](https://youtu.be/93f-ayezCqE?t=1118)

For this figure 8 wire loop can take it in R³, take the standard topology then inherit on M a subset from the R³ std topology, then chart out points and their open set regions loop onto R¹.

[3rd example. Y Wishbone Bifurcating line](https://youtu.be/93f-ayezCqE?t=1183)
M ⊆ (R², Ostd)
(M, Ostd |ₘ ) O restricted to M

Now we have a topological space but it fails to be a topological manifold.

Theres no problems with the legs of the Wishbone. The joint however it is not possible to find an open neighborhood

>Take an open set in R² and the subset topology is open
>We can't find an invertible in both directions, continous map in either R¹ or R² due to the bifurcation.

This is not a ?-dim. top mfd.

[CONT](https://youtu.be/93f-ayezCqE?t=1624)