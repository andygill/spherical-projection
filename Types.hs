{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types where

import Data.Dynamic
import Data.Reify
import Debug.Trace

import Prelude hiding (sin, cos, atan2, (^))
import qualified Prelude as P

import Expr

class Conditional e where
  ifZero :: Scalar -> e -> e -> e
      
newtype Scalar where
  Scalar :: Mu Expr -> Scalar

instance Show Scalar where
  showsPrec d (Scalar e) = showsPrec d e

instance Num Scalar where
  Scalar a + Scalar b = Scalar (Mu $ ExpAdd a b)
  Scalar a - Scalar b = Scalar (Mu $ ExpSub a b)
  Scalar a * Scalar b = Scalar (Mu $ ExpMul a b)
  fromInteger = Scalar . Mu . ExpScalar . fromInteger
  
instance Fractional Scalar where
  Scalar a / Scalar b = Scalar (Mu $ ExpDiv a b)
  fromRational = Scalar . Mu . ExpScalar . fromRational  

instance Floating Scalar where
  sqrt (Scalar n) = Scalar (Mu $ ExpSqrt n)

instance Conditional Scalar where
  ifZero (Scalar a) (Scalar b) (Scalar c) = Scalar $ Mu $ ExpIfZero a b c

infixr 8 ^

(^) :: Scalar -> Scalar -> Scalar
Scalar a ^ Scalar b = Scalar (Mu $ ExpPower a b)

-- Expressions can be radians
newtype Radian where
  Radian :: Mu Expr -> Radian

instance Show Radian where
  showsPrec d (Radian e) = showsPrec d e
  
instance Num Radian where
  Radian r1 + Radian r2 = Radian $ Mu $ ExpAdd r1 r2
  Radian r1 - Radian r2 = Radian $ Mu $ ExpSub r1 r2
  fromInteger = Radian . Mu . ExpScalar . fromInteger

instance Fractional Radian where
  fromRational = Radian . Mu . ExpScalar . fromRational

class Math radian where
  sin :: radian -> Scalar
  cos :: radian -> Scalar
  atan2 :: Scalar -> Scalar -> radian

instance Math Radian where
  sin (Radian r) = Scalar $ Mu $ ExpSin r
  cos (Radian r) = Scalar $ Mu $ ExpCos r
  atan2 (Scalar y) (Scalar x) = Radian $ Mu $ ExpAtan2 y x
    
instance Var Radian where
  mkVar' = VarGen $ \ i -> (Radian . Mu . ExpVar $ i , succ i)

-- Sometimes called elevation
newtype Longitude = Longitude Radian
  deriving (Show)

instance Num Longitude where
  Longitude a + Longitude b = Longitude (a + b)
  Longitude a - Longitude b = Longitude (a - b)

  fromInteger = Longitude . fromInteger 

instance Fractional Longitude where
  fromRational = Longitude . fromRational

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
  atan2 y x = Longitude $ atan2 y x

instance Math Latitude where
  sin (Latitude a) = sin a
  cos (Latitude a) = cos a  
  atan2 y x = Latitude $ atan2 y x

-- A Coordinate on a sphere
-- 0,0 is looking straight ahead
-- Like rotating in Y, X, then Z?
data Coord = Coord Latitude Longitude

-- A Point on a unit sphere
type Point = (Scalar, Scalar, Scalar)

longLatToPoint :: (Longitude, Latitude) -> Point
longLatToPoint (long,lat) =
  ( cos lat * cos long
  , cos lat * sin long
  , sin lat
  )

pointToLatLong :: Point -> (Longitude, Latitude)
pointToLatLong (x,y,z)  = (long,lat)
  where
    long = atan2 y x
    lat  = atan2 z (sqrt (x^2 + y^2))

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

-- These are from https://mathworld.wolfram.com/GnomonicProjection.html
fromRecilinear :: (Latitude, Longitude) -> Rectilinear -> (Latitude, Longitude)
fromRecilinear = undefined

toRecilinear :: (Latitude, Longitude) -> (Latitude, Longitude) -> Rectilinear
toRecilinear (phi_1,lam_0) (phi,lam) = ifZero cos_c (Rectilinear 0 0)
                                     $ Rectilinear x y
  where
    cos_c = sin(phi_1) * sin(phi) + cos(phi_1) * cos(phi) * cos (lam - lam_0);
    x = cos(phi) * sin (lam - lam_0) / cos_c
    y = (cos(phi_1) * sin(phi) - sin(phi_1) * cos(phi) * cos (lam - lam_0)) / cos_c

------------------------------------------------------------------------------

instance MuRef Scalar where
  type DeRef Scalar = Expr
  mapDeRef f (Scalar s) = mapDeRef f s

instance Var Scalar where
  mkVar i = (Scalar . Mu . ExpVar $ i , [i])

instance Body Scalar where
  maxVar (Scalar e) = maxVar e

evalScalar :: Scalar -> Value
evalScalar (Scalar e) = evalMu e

instance Var Latitude where
  mkVar i = (Latitude . Radian . Mu . ExpVar $ i , [i])
  mkVar' = Latitude <$> mkVar'

instance Var Longitude where
  mkVar i = (Longitude . Radian . Mu . ExpVar $ i , [i])
  mkVar' = Longitude <$> mkVar'
  
instance Body Rectilinear where
  maxVar (Rectilinear a b) = maxVar a `max` maxVar b
