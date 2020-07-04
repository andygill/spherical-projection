{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types where

import Data.Dynamic
import Data.Reify
import Debug.Trace
import Control.Applicative

import Prelude hiding (sin, cos, atan2, asin, (^))
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
  fromInteger = Scalar . Mu . ExpScalar . fromInteger


instance Fractional Scalar where
  Scalar a / Scalar b = Scalar (Mu $ ExpDiv a b)
  fromRational = Scalar . Mu . ExpScalar . fromRational

instance Floating Scalar where
  sqrt (Scalar n) = Scalar (Mu $ ExpSqrt n)

-- Added for fisheye projections
instance Math Scalar where
    atan2 (Scalar a) (Scalar b) = Scalar $ Mu $ ExpAtan2 a b

instance Conditional Scalar where
  ifZero (Scalar a) (Scalar b) (Scalar c) = Scalar $ Mu $ ExpIfZero a b c

infixr 8 ^

(^) :: Scalar -> Int -> Scalar
Scalar a ^ b = Scalar (Mu $ ExpPower a b)

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
  fromRational = Radian . Mu . ExpScalar . fromRational

class Math radian where
  sin   :: radian -> Scalar
  cos   :: radian -> Scalar
  asin  :: Scalar -> radian
  acos  :: Scalar -> radian
  atan  :: Scalar -> radian
  atan2 :: Scalar -> Scalar -> radian

instance Math Radian where
  sin (Radian r) = Scalar $ Mu $ ExpSin r
  cos (Radian r) = Scalar $ Mu $ ExpCos r
  asin (Scalar s) = Radian $ Mu $ ExpAsin s
  acos (Scalar s) = Radian $ Mu $ ExpAcos s
  atan (Scalar s) = Radian $ Mu $ ExpAtan s
  atan2 (Scalar y) (Scalar x) = Radian $ Mu $ ExpAtan2 y x

instance Var Radian where
  mkVar = singletonVar (Radian . Mu . ExpVar)

instance ToMuExpr Radian where
  toMuExpr (Radian a) = a

instance Conditional Radian where
  ifZero (Scalar a) (Radian b) (Radian c) = Radian $ Mu $ ExpIfZero a b c

-- Sometimes called elevation
newtype Longitude = Longitude Radian
  deriving (Show)

instance Num Longitude where
  Longitude a + Longitude b = Longitude (a + b)
  Longitude a - Longitude b = Longitude (a - b)

  fromInteger = Longitude . fromInteger

instance Fractional Longitude where
  fromRational = Longitude . fromRational

instance Conditional Longitude where
  ifZero a (Longitude b) (Longitude c) = Longitude $ ifZero a b c


-- Sometimes called azimuth
newtype Latitude = Latitude Radian
  deriving (Show)

instance Num Latitude where
  fromInteger = Latitude . fromInteger

instance Fractional Latitude where
  fromRational = Latitude . fromRational

instance Math Longitude where
  sin (Longitude a) = sin a
  cos (Longitude a) = cos a
  asin = Longitude . asin
  atan2 y x = Longitude $ atan2 y x

instance Math Latitude where
  sin (Latitude a) = sin a
  cos (Latitude a) = cos a
  asin = Latitude . asin
  atan2 y x = Latitude $ atan2 y x

instance Conditional Latitude where
  ifZero a (Latitude b) (Latitude c) = Latitude $ ifZero a b c

-- A Coordinate on a sphere
-- 0,0 is looking straight ahead
-- Like rotating in Y, X, then Z?
data Coord = Coord Latitude Longitude

-- A Point on a unit sphere
type Point = (Scalar, Scalar, Scalar)

--type Point2D = (Scalar, Scalar)

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
{-
longLatToPoint2D :: (Longitude, Latitude) -> Point2D
longLatToPoint2D (long, lat) = (x,y)
    where
        x = long / pi
        y = (2 * lat) / pi

normPoint2DToLongLat :: Point2D -> (Longitude, Latitude)
normPoint2DToLongLat (x,y) = (long, lat)
    where
        long = x * pi
        lat = (y * pi) / 2
        -}
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

instance MuRef Scalar where
  type DeRef Scalar = Expr
  mapDeRef f (Scalar s) = mapDeRef f s

instance Var Scalar where
    mkVar = singletonVar (Scalar . Mu . ExpVar)

instance Body Scalar where
  maxVar (Scalar e) = maxVar e

evalScalar :: Scalar -> Value
evalScalar (Scalar e) = evalMu e

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
