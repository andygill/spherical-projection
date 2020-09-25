{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types where

import Data.Dynamic
import Data.Reify
import Debug.Trace
import Control.Applicative (liftA2)

import Prelude hiding (sin, cos, atan2, asin, acos, (^), tan)
import qualified Prelude as P

import Expr

class Conditional e where
  ifZero :: Scalar -> e -> e -> e

instance (Conditional a, Conditional b) => Conditional (a,b) where
  ifZero s (a,b) (c,d) = (ifZero s a c, ifZero s b d)

newtype Scalar where
  Scalar :: Mu Expr -> Scalar

instance Show Scalar where
  showsPrec d (Scalar e) = showsPrec d e

instance Num Scalar where
  Scalar a + Scalar b = Scalar (Mu $ ExpAdd a b)
  Scalar a - Scalar b = Scalar (Mu $ ExpSub a b)
  Scalar a * Scalar b = Scalar (Mu $ ExpMul a b)
  abs (Scalar a) = Scalar (Mu $ ExpAbs a)
  signum (Scalar a) = Scalar (Mu $ ExpSignum a)
  fromInteger = Scalar . Mu . ExpScalar . fromInteger

instance Fractional Scalar where
  Scalar a / Scalar b = Scalar (Mu $ ExpDiv a b)
  fromRational = Scalar . Mu . ExpScalar . fromRational

instance Floating Scalar where
  sqrt (Scalar n) = Scalar (Mu $ ExpSqrt n)

-- Added for fisheye projections
instance Math Scalar where
    sin (Scalar r) = Scalar $ Mu $ ExpSin r
    cos (Scalar r) = Scalar $ Mu $ ExpCos r
    tan (Scalar r) = Scalar $ Mu $ ExpTan r
    atan2 (Scalar a) (Scalar b) = Scalar $ Mu $ ExpAtan2 a b

instance Conditional Scalar where
  ifZero (Scalar a) (Scalar b) (Scalar c) = Scalar $ Mu $ ExpIfZero a b c

infixr 8 ^

(^) :: Scalar -> Int -> Scalar
Scalar a ^ b = Scalar (Mu $ ExpPower a b)

instance ToMuExpr Scalar where
    toMuExpr (Scalar a) = a

instance ToExpr Scalar where
    reifyToExprFunction n (Scalar a) = reifyToExprFunction n a

instance MuRef Scalar where
  type DeRef Scalar = Expr
  mapDeRef f (Scalar s) = mapDeRef f s

instance Var Scalar where
    mkVar = singletonVar (Scalar . Mu . ExpVar)

instance Body Scalar where
  maxVar (Scalar e) = maxVar e

evalScalar :: Scalar -> Value
evalScalar (Scalar e) = evalMu e

-- Expressions can be radians
newtype Radian where
  Radian :: Mu Expr -> Radian

instance Show Radian where
  showsPrec d (Radian e) = showsPrec d e

instance Num Radian where
  Radian r1 + Radian r2 = Radian $ Mu $ ExpAdd r1 r2
  Radian r1 - Radian r2 = Radian $ Mu $ ExpSub r1 r2
  Radian r1 * Radian r2 = Radian $ Mu $ ExpMul r1 r2
  fromInteger = Radian . Mu . ExpScalar . fromInteger

instance Fractional Radian where
    Radian a / Radian b = Radian (Mu $ ExpDiv a b)
    fromRational = Radian . Mu . ExpScalar . fromRational

class Math radian where
  sin   :: radian -> Scalar
  cos   :: radian -> Scalar
  tan   :: radian -> Scalar
  asin  :: Scalar -> radian
  acos  :: Scalar -> radian
  atan  :: Scalar -> radian
  atan2 :: Scalar -> Scalar -> radian

instance Math Radian where
  sin (Radian r) = Scalar $ Mu $ ExpSin r
  cos (Radian r) = Scalar $ Mu $ ExpCos r
  tan (Radian r) = Scalar $ Mu $ ExpTan r
  asin (Scalar s) = Radian $ Mu $ ExpAsin s
  acos (Scalar s) = Radian $ Mu $ ExpAcos s
  atan (Scalar s) = Radian $ Mu $ ExpAtan s
  atan2 (Scalar y) (Scalar x) = Radian $ Mu $ ExpAtan2 y x

instance Floating Radian where
    sqrt (Radian n) = Radian (Mu $ ExpSqrt n)

instance Var Radian where
  mkVar = singletonVar (Radian . Mu . ExpVar)

instance ToMuExpr Radian where
  toMuExpr (Radian a) = a

instance MuRef Radian where
    type DeRef Radian = Expr
    mapDeRef f (Radian s) = mapDeRef f s

instance Body Radian where
    maxVar (Radian e) = maxVar e

evalRadian :: Radian -> Value
evalRadian (Radian e) = evalMu e

instance ToExpr Radian where
  reifyToExprFunction n (Radian a) = reifyToExprFunction n a

instance Conditional Radian where
  ifZero (Scalar a) (Radian b) (Radian c) = Radian $ Mu $ ExpIfZero a b c

-- Sometimes called elevation
newtype Longitude = Longitude Radian
  deriving (Show)

instance Num Longitude where
  Longitude a + Longitude b = Longitude (a + b)
  Longitude a - Longitude b = Longitude (a - b)
  Longitude a * Longitude b = Longitude (a * b)
  fromInteger = Longitude . fromInteger

instance Fractional Longitude where
  fromRational = Longitude . fromRational

instance Conditional Longitude where
  ifZero a (Longitude b) (Longitude c) = Longitude $ ifZero a b c

-- Sometimes called azimuth
newtype Latitude = Latitude Radian
  deriving (Show)

instance Num Latitude where
    Latitude a + Latitude b = Latitude (a + b)
    Latitude a - Latitude b = Latitude (a - b)
    Latitude a * Latitude b = Latitude (a * b)
    fromInteger = Latitude . fromInteger

instance Fractional Latitude where
  fromRational = Latitude . fromRational

instance Conditional Latitude where
  ifZero a (Latitude b) (Latitude c) = Latitude $ ifZero a b c

instance Math Longitude where
  sin (Longitude a) = sin a
  cos (Longitude a) = cos a
  tan (Longitude a) = tan a
  asin = Longitude . asin
  acos = Longitude . acos
  atan2 y x = Longitude $ atan2 y x

instance Math Latitude where
  sin (Latitude a) = sin a
  cos (Latitude a) = cos a
  tan (Latitude a) = tan a
  asin = Latitude . asin
  acos = Latitude . acos
  atan2 y x = Latitude $ atan2 y x
{-}
newtype Norm where
    Norm :: Mu Expr -> Norm

-- Expressions can be radians
instance Show Norm where
  showsPrec d (Norm es) = showsPrec d es

instance Num Norm where
  Norm r1 + Norm r2 = Norm $ Mu $ ExpAdd r1 r2
  Norm r1 - Norm r2 = Norm $ Mu $ ExpSub r1 r2
  Norm r1 * Norm r2 = Norm $ Mu $ ExpMul r1 r2
  fromInteger = Norm . Mu . ExpNorm . map fromInteger

instance Math Norm where
  sin (Norm r) = Scalar $ Mu $ ExpSin r
  cos (Norm r) = Scalar $ Mu $ ExpCos r
  asin (Norm s) = Radian $ Mu $ ExpAsin s
  acos (Norm s) = Radian $ Mu $ ExpAcos s
  atan (Norm s) = Radian $ Mu $ ExpAtan s
  atan2 (Norm y) (Norm x) = Radian $ Mu $ ExpAtan2 y x

instance Floating Norm where
    sqrt (Norm n) = Norm (Mu $ ExpSqrt n)

instance Var Norm where
  mkVar = singletonVar (Norm . Mu . ExpVar)

instance ToMuExpr Norm where
  toMuExpr (Norm a) = a

instance MuRef Norm where
    type DeRef Norm = Expr
    mapDeRef f (Norm s) = mapDeRef f s

instance Body Norm where
    maxVar (Norm e) = maxVar e

evalNorm :: Norm -> Value
evalNorm (Norm e) = evalMu e

instance (ToMuExpr a) => ToExpr [a] where
  reifyToExprFunction n as =
    reifyToExprFunction n $ Mu $ ExpNorm $ map toMuExpr as

instance (ToMuExpr a) => ToMuExpr [a] where
  toMuExpr as = Mu $ ExpNorm $ map toMuExpr as
-}
-- A Point on a unit sphere
type Point = (Scalar, Scalar, Scalar)

type Point2D = (Scalar, Scalar)

longLatToPoint :: (Longitude, Latitude) -> Point
longLatToPoint (long,lat) =
  ( cos lat * cos long
  , cos lat * sin long
  , sin lat
  )

pointToLongLat :: Point -> (Longitude, Latitude)
pointToLongLat (x,y,z)  = (long,lat)
  where
    long = atan2 y x
    lat  = atan2 z (sqrt (x^2 + y^2))

toRadian :: Scalar -> Radian
toRadian (Scalar x) = Radian x

scalarToLat = Latitude . toRadian
scalarToLong = Longitude . toRadian

toScalar :: Radian -> Scalar
toScalar (Radian x) = Scalar x

latToScalar (Latitude x) = toScalar x
longToScalar (Longitude x) = toScalar x

num_piS :: Scalar
num_piS = Scalar $ Mu $ ExpScalar $ num_pi
------------------------------------------------------------------------------

-- Rectilinear projection

data Rectilinear = Rectilinear Scalar Scalar
  deriving Show

instance Conditional Rectilinear where
  ifZero s (Rectilinear a b) (Rectilinear c d) =
    Rectilinear (ifZero s a c) (ifZero s b d)

instance MuRef Rectilinear where
  type DeRef Rectilinear = Expr
  mapDeRef f (Rectilinear (Scalar x) (Scalar y)) =
    mapDeRef f (Mu $ ExpRectilinear x y)

-- fisheye projection
data Fisheye = Fisheye Scalar Radian
    deriving Show

instance MuRef Fisheye where
  type DeRef Fisheye = Expr
  mapDeRef f (Fisheye (Scalar r) (Radian t)) =
    mapDeRef f (Mu $ ExpFisheye r t)
------------------------------------------------------------------------------
instance Var Latitude where
    mkVar = Latitude <$> mkVar

instance Var Longitude where
    mkVar = Longitude <$> mkVar

instance ToMuExpr Latitude where
    toMuExpr (Latitude a) = toMuExpr a

instance ToMuExpr Longitude where
    toMuExpr (Longitude a) = toMuExpr a

instance Body Rectilinear where
    maxVar (Rectilinear a b) = maxVar a `max` maxVar b

instance ToMuExpr Rectilinear where
    toMuExpr (Rectilinear (Scalar a) (Scalar b)) = Mu $ ExpRectilinear a b

instance ToExpr Rectilinear where
    reifyToExprFunction n (Rectilinear (Scalar a) (Scalar b)) =
        reifyToExprFunction n $ Mu $ ExpRectilinear a b

instance Var Rectilinear where
    mkVar = liftA2 Rectilinear mkVar mkVar

instance ToMuExpr Fisheye where
    toMuExpr (Fisheye (Scalar a) (Radian b)) = Mu $ ExpFisheye a b

instance ToExpr Fisheye where
    reifyToExprFunction n (Fisheye (Scalar a) (Radian b)) =
      reifyToExprFunction n $ Mu $ ExpFisheye a b

instance Var Fisheye where
    mkVar = liftA2 Fisheye mkVar mkVar
