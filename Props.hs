{-# LANGUAGE ScopedTypeVariables #-}

module Props where

import Prelude hiding (sin, cos, atan2, (^))
import qualified Prelude as P

import Test.QuickCheck

import Expr
import Types
import Utils

import Debug.Trace

-- Useful quickcheck properties
prop_sin (n :: Double) = P.sin n == r
  where
    Double r = evalScalar (sin (realToFrac n :: Radian))

prop_cos (n :: Double) = P.cos n == r
  where
    Double r = evalScalar (cos (realToFrac n :: Radian))

prop_cos (n :: Double) = P.cos n == r
  where
    Double r = evalScalar (cos (realToFrac n :: Radian))

newtype Semi = Semi Double -- +/- pi/2, excluding the limits.
  deriving Show

instance Arbitrary Semi where
  arbitrary = Semi <$> choose (-pi/2+0.001,pi/2-0.001)
  shrink (Semi a) | a == 0 || a == -0 = []
                  | nearZero a = [Semi $ 0 * signum a]
                  | otherwise  = [Semi (a*0.9)]

prop_fromRecilinear_toRecilinear0 (Semi a, Semi b) =
    -- label (show (a,b,a',b')) $
    nearZero (a - a') && nearZero (b - b')
  where
    Tuple [Double a',Double b'] =
      evalMu $
      toMuExpr $
      (fromRecilinear  (0,0) .toRecilinear  (0,0)) (realToFrac a,realToFrac b)

prop_fromRecilinear_toRecilinear (Semi x, Semi y) (Semi a, Semi b) =
  cos_c >= 0 ==>
    nearZero (a - a') && nearZero (b - b')
  where
    cos_c = P.sin(x) * P.sin(a) + P.cos(x) * P.cos(a) * P.cos (b - y)
    o = (realToFrac x, realToFrac y)
    (a'',b'') = norm (a',b')
    Tuple [Double a',Double b'] =
      evalMu $
      toMuExpr $
      (fromRecilinear  o . toRecilinear  o) (realToFrac a,realToFrac b)


inSemi :: Double -> Bool
inSemi n = -pi/2 < n && n < pi/2

-- Have to think about this
norm :: (Double,Double) -> (Double, Double)
norm (lat,long)
  | traceShow ("norm",lat,long) False = undefined
  | lat > pi/2  = norm (- (lat - pi), long + pi)
  | lat < -pi/2 = norm (- (lat + pi), long + pi)
  | long > pi   = norm (lat, long - pi*2)
  | long < -pi  = norm (lat, long + pi*2)
  | otherwise   = (lat,long)
--  | lat < -pi/2 =
