{-# LANGUAGE GADTs #-}

module Main where

import Prelude hiding (sin, cos, atan2)
import qualified Prelude as P

class Eval e where
  eval :: () -> e -> Double

-- Sometimes, they are just floating point values
data Number where
  Number :: Double -> Number
  Cos    :: Radian -> Number
  Sin    :: Radian -> Number
  Mul    :: Number -> Number -> Number
  Add    :: Number -> Number -> Number
  Sqrt   :: Number -> Number
  deriving (Eq, Show)

instance Num Number where
  Number a + Number b = Number (a + b)
  a + b = Add a b

  Number a * Number b = Number (a * b)
  a * b = Mul a b

instance Eval Number where
  eval _   (Number n) = n
  eval env (Cos r) = P.cos (eval env r)

instance Fractional Number where

instance Floating Number where
  sqrt (Number n) = Number (sqrt n)
  sqrt n          = Sqrt n

-- Expressions can be radians
data Radian where
  Radian :: Double -> Radian
  Atan2 :: Number -> Number -> Radian
  deriving (Eq, Show)

instance Num Radian where
  fromInteger = Radian  . fromInteger

instance Fractional Radian where
  fromRational = Radian  . fromRational

instance Eval Radian where
  eval _   (Radian n) = n
  eval env (Atan2 y x) = P.atan2 (eval env y) (eval env x)

class Math radian where
  sin :: radian -> Number
  cos :: radian -> Number
  atan2 :: Number -> Number -> radian

instance Math Radian where
  cos = Cos
  sin = Sin
  atan2 y x = Atan2 y x
    
newtype Longitude = Longitude Radian
  deriving (Eq, Show)

instance Num Longitude where
  fromInteger = Longitude . fromInteger 

instance Fractional Longitude where
  fromRational = Longitude . fromRational

newtype Latitude = Latitude Radian
  deriving (Eq, Show)

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
{-
sin :: Radian -> Number
sin = error ""

cos :: Radian -> Number
cos = error ""
-}
-- A Point on a unit sphere
type Point = (Number, Number, Number)

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

{-
-- add together two direction.
addDirection :: (Latitude,Longitude)
	     -> (Latitude,Longitude)
	     -> (Latitude,Longitude)
addDirection (lat0,long0) (lat1,long1) = (lat,long)
  where
    lat  = 
    long = 
-}
main = print ()
