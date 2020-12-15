# spherical-projection
 Translations to and from a spherical coordinate system

## Packages
[JuicyPixels](https://hackage.haskell.org/package/JuicyPixels) - Used for image read/write

## Files

### Main.hs
Quick Entry point for the program. Runs a transformation across an image if the transformation exists.

` :main <transform number> <file extension> <input path> <output path> `

Transform numbers:
 - 1 -> inverseFisheyeTransform
 - 2 -> unFisheye
 - 3 -> inversePanoToLittlePlanet
 - 4 -> panoToLittlePlanet
 - 5 -> panoToGnomic

File Extensions: (file types to output to. output path still requires `.<ext>` to work)
 - png
 - jpeg
 - tga

## Files - Expressions
### Expr.hs

#### Expr
`data Expr :: * -> *`

This is the main expression data type that currently encapsulates the following operators:
 - Data Values:
    - Scalar (`Double -> Expr t`)
    - Tuples
    - Variables (special)
    - Identity
 - Unary Operators:
    - Algebraic: abs, sqrt, negate, sign of number
    - Trig: sin, asin, cos, acos, tan, atan,
 - Binary Operators:
    - Arithmetic: + - * / integer exponentiation
    - Trig: atan2
    - Functions: Lambda functions, rectilinear, fisheye
 - Ternary Operators:
    - ifZero (data type for zero checking)

#### Instances
The following instances are implicitly given: `Show, Eq, Ord`
The following instances are explicitly derived for each `Expr`: `Functor, Foldable, Traversable`

#### Eval
The `eval :: Expr e -> e` converts Mu Expr to Expr and then allows for the expression to be evaluate literally and then output as a scalar, tuple, or a failure token.

#### Mu Expr
Mu Expr is a special data type created for referential composition so that you always get a Mu Expr back when composing Mu Expr. It has instance `Show` explicitly derived. Mu Expr is used in reifyFunction to take a graph of Mu Expr to an acyclic graph and outputs it as an ExprFunction.

`data ExprFunction = ExprFunction [V] [(V,Expr V)]  V  deriving Eq`

ExprFunction has a list of input nodes `[V]`, a list of nodes with their corresponding Expr and the nodes attached to the Expr, and the entry point node.

`newtype V = V Int deriving (Eq, Ord)`

This data type encapsulates the integer identifier for a node.

### Types.hs
This file defines a bunch of mathematical types and how they interact with one another. The types in this file increase in comlexity as you go down the file.

**Scalar**: A Scalar is any real number in normal Euclidean space. Scalar is defined as `Scalar :: Mu Expr -> Scalar`. The following instances are defined:
 - `Num` : +, -, *, abs, signum, fromInteger
 - `Fractional`: /, fromRational
 - `Floating`: sqrt
 - `Math`: ^,sin, tan, cos, atan2 (this can be used as a shortcut to get around some radian stuff in the math, but subverts the intended use)
 - `ifZero`


 **Radian**: A Radian is any real number in a spherical space. Radian is defined as `Radian :: Mu Expr -> Radian`. The following instances are defined:
  - `Num` : +, -, *, fromInteger
  - `Fractional`: /, fromRational
  - `Math`: ^, sin, tan, cos, asin, acos, atan, atan2
  - `Floating`: sqrt
  - `ifZero`

 **Latitude** and **Longitude**: These are special wrappers around the Radian type to be expressly used in math for spherical representation and is any real number in a spherical space. Each are defined as a composition of Radian `data Longitude :: Longitude Radian` (resp. Latitude). The following instances are defined:
 - `Num` : +, -, *, fromInteger
 - `Fractional`: /, fromRational
 - `Math`: sin, tan, cos, asin, acos, atan
 - `ifZero`

#### Coordinate Conversions
There are two functions that transfer between the two spaces.

`longLatToPoint :: (Longitude, Latitude) -> Point`

`pointToLongLat :: Point -> (Longitude, Latitude)`

`type Point = (Scalar, Scalar, Scalar)`

`type Point2D = (Scalar, Scalar)`

There are also some utility functions to help facilitate moving from radian to scalar, and from lat/long to scalar, and vice versa.

## Files - File and Image Generation

## Files - Mathematics/Utility
