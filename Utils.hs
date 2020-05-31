module Utils where

import Prelude hiding (sin, cos, atan2, asin, acos, atan, (^))
import qualified Prelude as P

import Expr
import Types

-- These are from https://mathworld.wolfram.com/GnomonicProjection.html
fromRecilinear :: (Latitude, Longitude) -> Rectilinear -> (Latitude, Longitude)
fromRecilinear (phi,lam) (Rectilinear x y) = ifZero p (phi,lam) (phi',lam')
  where
    p = sqrt (x*x + y*y) :: Scalar  -- we could have a distance parm
    c = atan (p) :: Radian
    phi' = asin (cos(c) * sin(phi) + (y * sin(c) * cos(phi) / p))
    lam' = lam + atan2 (x * sin(c)) (p * cos(phi) * cos(c) - y * sin(phi) * sin(c))

toRecilinear :: (Latitude, Longitude) -> (Latitude, Longitude) -> Rectilinear
toRecilinear (phi_1,lam_0) (phi,lam) = ifZero cos_c (Rectilinear 0 0)
                                     $ Rectilinear x y
  where
    cos_c = sin(phi_1) * sin(phi) + cos(phi_1) * cos(phi) * cos (lam - lam_0);
    x = cos(phi) * sin (lam - lam_0) / cos_c
    y = (cos(phi_1) * sin(phi) - sin(phi_1) * cos(phi) * cos (lam - lam_0)) / cos_c

