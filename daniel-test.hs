module Utils where

import Prelude hiding (sin, cos, atan2, asin, acos, atan, (^))
import qualified Prelude as P
import Debug.Trace

import Expr
import Types

-- These are from https://mathworld.wolfram.com/GnomonicProjection.html
fromRecilinear :: (Latitude, Longitude) -> Rectilinear -> (Latitude, Longitude)
fromRecilinear (phi,lam) (Rectilinear x y) = ifZero p (phi,lam) (phi',lam')
  where
    p = sqrt (x*x + y*y) :: Scalar  -- we could have a distance prim
    c = atan (p) :: Radian
    phi' = asin (cos(c) * sin(phi) + (y * sin(c) * cos(phi) / p))
    lam' = lam + atan2 (x * sin(c)) (p * cos(phi) * cos(c) - y * sin(phi) * sin(c))

-- Precondition
-- -pi/2 <= phi_1 + phi <= pi/2
-- -pi/2 <= lam_1 + lam <= pi/2
-- From MAP PROJECTIONS A WORKING MANUAL By JOHN P. SNYDER
-- That is, if cos cis zero or negative, the point is to be rejected.
-- If cos c is positive, it may or may not be plotted depending on
-- the desired limits of the map.
-- Precondition of cos_c >= 0

toRecilinear :: (Latitude, Longitude) -> (Latitude, Longitude) -> Rectilinear
toRecilinear (phi_1,lam_0) (phi,lam) =
  ifZero cos_c (Rectilinear 0 0) $ Rectilinear x y
  where
    cos_c = (sin(phi_1) * sin(phi) + cos(phi_1) * cos(phi) * cos (lam - lam_0))
    x = cos(phi) * sin (lam - lam_0) / cos_c
    y = (cos(phi_1) * sin(phi) - sin(phi_1) * cos(phi) * cos (lam - lam_0)) / cos_c

-- https://mathworld.wolfram.com/StereographicProjection.html
fromLongLatToStero :: (Latitude, Longitude) -> (Latitude, Longitude) -> Rectilinear
fromLongLatToStero (phi_1,lam_0) (phi,lam) = (Rectilinear 0 0) $ Rectilinear x y
  --ifZero cos_c (Rectilinear 0 0) $ Rectilinear x y
  where
    -- it is supposed to be 2R but I am assuming unit sphere
    k = (/) 2 $ 1 + (sin(phi_1) * sin(phi)) + (cos(phi_1) * cos(phi) * cos (lam - lam_0))
    x = k * cos(phi) * sin (lam - lam_0)
    y = k * (cos(phi_1) * sin(phi) - sin(phi_1) * cos(phi) * cos (lam - lam_0))

fromSteroToLongLat :: (Latitude, Longitude) -> Rectilinear -> (Latitude, Longitude)
fromSteroToLongLat (phi,lam) (Rectilinear x y) = ifZero p (phi,lam) (phi',lam')
  where
    p = sqrt (x*x + y*y) :: Scalar
    c = 2 * atan (p/2) :: Radian -- also should be 2R
    phi' = asin (cos(c) * sin(phi) + (y * sin(c) * cos(phi) / p))
    lam' = lam + atan $ (/) (x * sin(c)) (p * cos(phi) * cos(c) - y * sin(phi) * sin(c))

----------------------------------------------------
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
    long = (atan2 y x) :: Radian
    lat  = (atan2 z (sqrt (x^2 + y^2))) :: Radian

longLatToPoint2D :: (Longitude, Latitude) -> Point2D
longLatToPoint2D (long, lat) = (x,y)
    where
        x = long / pi :: Scalar
        y = (2 * lat) / pi :: Scalar

normPoint2DToLongLat :: Point2D -> (Longitude, Latitude)
normPoint2DToLongLat (x,y) = (long, lat)
    where
        long = x * pi :: Radian
        lat = (y * pi) / 2 :: Radian

-- 3D to fisheye http://paulbourke.net/dome/dualfish2sphere/
-- f is the camera's aperature
fromLongLatToFisheye :: (Longitude, Latitude) -> Double -> Fisheye
fromLongLatToFisheye ll f = Fisheye r t
    where
        (x,y,z) = longLatToPoint ll
        t = atan2 z x :: Radian
        r = ((/) ((*) 2 $ atan2 (sqrt (x^2 + z^2)) (y)) f) :: Scalar

-- f is aperature
fromFisheyeToLongLat :: Fisheye -> Double -> (Longitude, Latitude)
fromFisheyeToLongLat (Fisheye r t) f = (long, lat)
    where
        x = r * cos(t) :: Scalar
        y = r * sin(t) :: Scalar
        long = atan2 y x :: Radian
        lat = (r * f) / 2 :: Radian

-- The double is the aperature val
fishEyeToPano :: Fisheye -> Double -> Point2D
fishEyeToPano = longLatToPoint2D . fromFisheyeToLongLat

-- The double is the aperature val
panoToFishEye :: Point2D -> Double -> Fisheye
panoToFishEye = fromLongLatToFisheye . normPoint2DToLongLat
