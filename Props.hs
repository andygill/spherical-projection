{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Props where

import Prelude hiding (sin, cos, atan2, (^))
import qualified Prelude as P

import Test.QuickCheck

import Expr
import Types
import Optimizer
import Utils

import Debug.Trace

val_to_dbl :: Value -> Double
val_to_dbl (Double x) = x
val_to_tuple (Tuple xs) = map val_to_dbl xs
val_to_fail (Fail s) = s

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

--Optimizer
prop_add_0_l (n :: Double) = (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m =  Mu (ExpAdd (Mu $ ExpScalar 0) (Mu $ ExpScalar n))

prop_add_0_r (n :: Double) = (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m =  Mu (ExpAdd (Mu $ ExpScalar n) (Mu $ ExpScalar 0))

prop_sub_0_l (n :: Double) = (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m = Mu (ExpSub (Mu $ ExpScalar 0) (Mu $ ExpScalar n))

prop_sub_0_r (n :: Double) = (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m = Mu (ExpSub (Mu $ ExpScalar n) (Mu $ ExpScalar 0))

prop_sub_equal (n :: Double) = (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m = Mu (ExpSub (Mu $ ExpScalar n) (Mu $ ExpScalar n))

prop_sin_0 = (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m = Mu (ExpSin (Mu $ ExpScalar 0))

prop_sin_asin (n :: Double) = n <= 1 && n >= -1 ==> (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m = Mu $ ExpSin $ Mu $ ExpAsin $ Mu $ ExpScalar n

prop_asin_sin (n :: Double) = n <= pi/2 && n >= -pi/2 ==> (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m = Mu $ ExpAsin $ Mu $ ExpSin $ Mu $ ExpScalar n

prop_asin_1 = (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m = Mu $ ExpAsin $ Mu $ ExpScalar 1

prop_asin_neg1 = (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m = Mu $ ExpAsin $ Mu $ ExpScalar (-1)

prop_asin_0 = (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m = Mu $ ExpAsin $ Mu $ ExpScalar 0

prop_cos_0 = (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m = Mu (ExpCos (Mu $ ExpScalar 0))

prop_cos_acos (n :: Double) = n <= 1 && n >= -1 ==> (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m = Mu $ ExpCos $ Mu $ ExpAcos $ Mu $ ExpScalar n

prop_acos_cos (n :: Double) = n <= pi && n >= 0 ==> (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m = Mu $ ExpAcos $ Mu $ ExpCos $ Mu $ ExpScalar n

prop_acos_1 = (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m = Mu $ ExpAcos $ Mu $ ExpScalar 1

prop_acos_neg1 = (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m = Mu $ ExpAcos $ Mu $ ExpScalar (-1)

prop_acos_0 = (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m = Mu $ ExpAcos $ Mu $ ExpScalar 0

prop_mul_0_l (n :: Double) = (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m =  Mu (ExpMul (Mu $ ExpScalar 0) (Mu $ ExpScalar n))

prop_mul_0_r (n :: Double) = (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m =  Mu (ExpMul (Mu $ ExpScalar n) (Mu $ ExpScalar 0))

prop_mul_1_l (n :: Double) = (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m =  Mu (ExpMul (Mu $ ExpScalar 1) (Mu $ ExpScalar n))

prop_mul_1_r (n :: Double) = (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m =  Mu (ExpMul (Mu $ ExpScalar n) (Mu $ ExpScalar 1))
{-
prop_mul_pow_r (r :: Double) (n :: Int) = n >= 0 && r /= 0 ==> (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m = Mu (ExpMul (Mu $ ExpScalar r) (Mu (ExpPower (Mu $ ExpScalar r) n)))

prop_mul_pow_l (r :: Double) (n :: Int) = n >= 0 && r /= 0 ==> (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m = Mu (ExpMul (Mu (ExpPower (Mu $ ExpScalar r) n)) (Mu $ ExpScalar r))
-}
prop_div_0_l (n :: Double) = n /= 0 ==> (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m =  Mu (ExpDiv (Mu $ ExpScalar 0) (Mu $ ExpScalar n))

prop_div_equals (n :: Double) = n /= 0 ==> (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m =  Mu (ExpDiv (Mu $ ExpScalar n) (Mu $ ExpScalar n))
{-
prop_div_pow_r (r :: Double) (n :: Int) = r /= 0 ==> (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m = Mu (ExpDiv (Mu $ ExpScalar r) (Mu (ExpPower (Mu $ ExpScalar r) n)))

prop_div_pow_l (r :: Double) (n :: Int) = r /= 0 ==> (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m = Mu (ExpDiv (Mu (ExpPower (Mu $ ExpScalar r) n)) (Mu $ ExpScalar r))
-}
prop_div_ab_over_a (r :: Double) (n :: Double) = r /= 0 ==> (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m = Mu (ExpDiv (Mu (ExpMul (Mu $ ExpScalar r) (Mu $ ExpScalar n))) (Mu $ ExpScalar r))

prop_div_ba_over_a (r :: Double) (n :: Double) = r /= 0 ==> (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m = Mu (ExpDiv (Mu (ExpMul (Mu $ ExpScalar n) (Mu $ ExpScalar r))) (Mu $ ExpScalar r))

prop_div_a_over_ab (r :: Double) (n :: Double) = r /= 0 && n /= 0 ==> (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m = Mu (ExpDiv (Mu $ ExpScalar r) $ Mu $ ExpMul (Mu $ ExpScalar r) (Mu $ ExpScalar n))

prop_div_a_over_ba (r :: Double) (n :: Double) = r /= 0 && n /= 0 ==> (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m = Mu (ExpDiv (Mu $ ExpScalar r) $ Mu $ ExpMul (Mu $ ExpScalar n) (Mu $ ExpScalar r))

prop_pow_0 (n::Double) = n /= 0 ==>(val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m = Mu $ ExpPower (Mu $ ExpScalar n) 0

prop_pow_1 (n::Double) = (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m = Mu $ ExpPower (Mu $ ExpScalar n) 1

prop_distrib_ab_add_ac (a :: Double) (b :: Double) (c :: Double) = (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m = Mu (ExpAdd (Mu (ExpMul (Mu $ ExpScalar a) (Mu $ ExpScalar b))) (Mu (ExpMul (Mu $ ExpScalar a) (Mu $ ExpScalar c))))


prop_distrib_ab_add_ca (a :: Double) (b :: Double) (c :: Double) = (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m = Mu (ExpAdd (Mu (ExpMul (Mu $ ExpScalar a) (Mu $ ExpScalar b))) (Mu (ExpMul (Mu $ ExpScalar c) (Mu $ ExpScalar a))))


prop_distrib_ba_add_ac (a :: Double) (b :: Double) (c :: Double) = (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m = Mu (ExpAdd (Mu (ExpMul (Mu $ ExpScalar b) (Mu $ ExpScalar a))) (Mu (ExpMul (Mu $ ExpScalar a) (Mu $ ExpScalar c))))


prop_distrib_ba_add_ca (a :: Double) (b :: Double) (c :: Double) = (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m = Mu (ExpAdd (Mu (ExpMul (Mu $ ExpScalar b) (Mu $ ExpScalar a))) (Mu (ExpMul (Mu $ ExpScalar c) (Mu $ ExpScalar a))))

prop_distrib_ab_sub_ac (a :: Double) (b :: Double) (c :: Double) = (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m = Mu (ExpSub (Mu (ExpMul (Mu $ ExpScalar a) (Mu $ ExpScalar b))) (Mu (ExpMul (Mu $ ExpScalar a) (Mu $ ExpScalar c))))


prop_distrib_ab_sub_ca (a :: Double) (b :: Double) (c :: Double) = (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m = Mu (ExpSub (Mu (ExpMul (Mu $ ExpScalar a) (Mu $ ExpScalar b))) (Mu (ExpMul (Mu $ ExpScalar c) (Mu $ ExpScalar a))))


prop_distrib_ba_sub_ac (a :: Double) (b :: Double) (c :: Double) = (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m = Mu (ExpSub (Mu (ExpMul (Mu $ ExpScalar b) (Mu $ ExpScalar a))) (Mu (ExpMul (Mu $ ExpScalar a) (Mu $ ExpScalar c))))


prop_distrib_ba_sub_ca (a :: Double) (b :: Double) (c :: Double) = (val_to_dbl $ evalMu m) == (val_to_dbl $ evalMu $ opt m)
    where
        m = Mu (ExpSub (Mu (ExpMul (Mu $ ExpScalar b) (Mu $ ExpScalar a))) (Mu (ExpMul (Mu $ ExpScalar c) (Mu $ ExpScalar a))))

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

return []
runTests = $quickCheckAll
