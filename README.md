# spherical-projection
 Translations to and from a spherical coordinate system

## Packages
[base >=4.12](https://hackage.haskell.org/package/base) - Haskell base, at least version 4.12

[data-reify](https://hackage.haskell.org/package/data-reify) - The package that transforms the expression tree into an acyclic graph

[QuickCheck](https://hackage.haskell.org/package/QuickCheck) - An automated testing suite

[containers](https://hackage.haskell.org/package/containers) - A package with an assortment of abstract data types

[JuicyPixels](https://hackage.haskell.org/package/JuicyPixels) - Used for image read/write

## Modules

### Main
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

## Modules - Expressions

### Expr

#### Expr Data Type
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

### Types
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

There are also some utility functions to help facilitate moving from radian to scalar, and from lat/long to scalar, and vice versa. (Kinda subvert the type system but are really helpful when an operations acts on 2 different spaces)

### Optimizer
This file uses simple algebraic and trig identities to remove unnecessary Expr's in an ExprFunction. Additionally acts as a partial evaluator for nodes where operations are defined (ie not adding a scalar and an Expr node). There are many comments throughout the file so this part will be brief.

#### opt
`opt :: Mu Expr -> Mu Expr`

opt is a function that simplifies algebra and trigonometry when it has the ability to do so, outputting a new form of the operations tree/operation.

#### muConversion
`muConversion :: V -> ExprFunction -> ExprFunction`

muConversion takes an entry node and an ExprFunction and runs optimizations as well as partial evaluation wherever possible, returning a new ExprFunction. It is a deceivingly complicated function used to hack ways to optimize & evaluate an expression tree. Brief description of events:

1. Checks if the input index if <= 0. if so clean up the function and stop, otherwise continue
2. Lookup entry node and see if it is a id, var, or scalar
 - If so, continue to the next iteration
 - Otherwise, continue
3. Try to see if the given node is on the leaves of the expr tree by making the expr a Mu Expr and then running eval on it
 - If a real number/tuple returns Just, replace the given entry node's expr with the scalar evaluated expression. Move on to the next iteration
 - Otherwise, continue
4. Test to see if the Mu Expr of the given node changes after optimization.
 - If so, go to next iteration
 - Otherwise, continue
5. Other stuff idk how to explain just yet

## Modules - File and Image Generation
### CodeGen
#### File Writing
There are multiple functions with slightly different function signatures that allow for expanded capabilities and convenience. In each of them, a list of functions will reified, optimized, and then output in an a given host language (only JS and Haskell right now). There are special `Show` instances for each of the languages that will output code into valid host language code.

#### regenFunctions
This is a special function that rewrites the functions used in the same project, so there can be some weird dependencies. Every time you update the source functions, and want to test them on an image you must do the following:
 1. If you have a funcs.hs already, its a good idea at least to have a copy of it as a backup. If there is a failure in reification process, the program will overwrite the file with nothing and cause a bunch of dependency problems.
 2. Update the math functions you want
 3. Rebuild the project
 4. Change the image manipulation functions to fit the new function arguments, then run regenFunctions
 5. Rebuild the project again
 6. Use the image manipulation stuff

Automatically creates the `Funcs` module for the project to build with
### Image


## Files - Mathematics/Utility
### Util
This file primarily contains the different mathematical functions with the notation seen in Types.hs. Currently, it has the following:
 - Rectilinear
 - Stereographic to/from Rectilinear
 - Stereographic transform through z = -1 (Little Planet effect)
 - Basic vector transforms: rotate, scale, translate

### Props
This is the Quick-Check file used to check various transformations and Expr identities.
