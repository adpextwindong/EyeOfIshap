# [Complex numbers and curves | Math History | NJ Wildberger](https://www.youtube.com/watch?v=oybzcvv-ZVo)

19th century advances upon Descartes work with looking at polynomials and curves. Newton and others started looking at degree 3 curves.

## New developments

1. Role of Projective Geometry & Homogenous Coordinates. [Mobius & Plucker ~1830]
2. The extension of curves to Complex Numbers.

    Motivated by the fundamental theorem of Algebra

    Bezout's theorem (Intersection of curves and products of their degrees)
3. Toplogical Ideas - Riemann Surface (Bernard Riemann ~1850)

### Role of Projective Geometry & Homogenous Coordinates. [Mobius & Plucker ~1830]

[if p(x,y) is a product of linear factors: p(x,y) = (a₁x₁ + b₁y + c₁) * (a₂x₂ + by₂ + c₂) ...](https://youtu.be/oybzcvv-ZVo?t=495)

the graph is a product of lines.

Note: if we vary the coefficients we get more general algebraic curves.

These sections where the lines/curves head towards infinity could begin using ideas from projective geometry that were already dealing with infinity. Mobius and Plucker give a good way to do that using Homogenous coordinates.

[Affine Plane vs Projective Plane](https://youtu.be/oybzcvv-ZVo?t=588)

If we start thinking about lines through the origin in the projective plane that intersect with the curve example (y = x²), we get a cone of lines formed by the parabola joined at the origin.

```
[X,Y,Z]

[X/Z, Y/Z, 1] -- The original curve in the projective plane corresponding to y=x²

Y\Z = (X\Z)²
```
[The homogenous form of the parabola](https://youtu.be/oybzcvv-ZVo?t=792)
```
YZ = X² 
```

Now all the terms of the same degree. All the lower terms are multiplied by an appropriate power of Z so they're all the same degree. Now we can see the points at infinity more clearly.

To find these points at infinity they correspond to lines through the origin satifying the equation and are horizontal, laying on Z=0.

So we set Z=0, and look at the equation.

```
0 = X² 
[0,1,0] is such a point
```

### Complex Numbers

EX: x² + y² = 1

Basic introduction of homogenous coordinates:

x = X/Z y = Y/Z

This gives us the projective curve

```
X² / Z² + Y² / Z² = 1

X² + Y² = Z² 
```

Now if we want points at infinity, let Z = 0, to get X² + Y² = 0.

Over the rational numbers there are no solutions, however in the Complex numbers we have solutions.

I₁ [1:i:0]
I₂ [1:-i:0]

These are the circular points at infinity.

Circles are now then the conics which pass thru I₁ and I₂.

Laguerre discovered that these circular points can give an alternate projective orrientation to angles in the plane.

Suppose the angle θ between 𝓁1 and 𝓁2. We can look at the line at infinity and I₁ and I₂.

We can join the intersection point P with I₁ and I₂ to get lines 𝓂1 𝓂2. Now if we can get the cross ratio ([A canonical invariant from the fact that we have 4 lines](https://youtu.be/oybzcvv-ZVo?t=1373) in the context of projective geometry).

θ = i log R(𝓁1, 𝓁2; 𝓂1, 𝓂2) --Cross Ratio

Cayley then extended this idea. Around this time people thought Projective Geometry was the hot shit and at the heart of all Geometry.

### What does a (Complex) curve look like?

Simplest case: [projective line](https://youtu.be/oybzcvv-ZVo?t=1559).

[The projective line P1 is topologically a circle.](https://youtu.be/oybzcvv-ZVo?t=1644)

Stereographic Projection.

--Conformal maps (preserves angles/spread

Bernhard Riemann

Made contributions to:
Introduced the notion of Riemannian manifold
Complex Analysis
Number Theory - Riemann Hypothesis

Stereographic Projection, S² and 