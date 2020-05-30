{-# LANGUAGE ScopedTypeVariables #-}

module Props where

import Prelude hiding (sin, cos, atan2, (^))
import qualified Prelude as P

import Test.QuickCheck

import Expr
import Types

-- Useful quickcheck properties
prop_sin (n :: Double) = P.sin n == r
  where
    Double r = evalScalar (sin (realToFrac n :: Radian))

prop_cos (n :: Double) = P.cos n == r
  where
    Double r = evalScalar (cos (realToFrac n :: Radian))
