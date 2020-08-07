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

-- 3D to fisheye http://paulbourke.net/dome/dualfish2sphere/
-- f is the camera's aperature
fromLongLatToFisheye :: Scalar -> (Longitude, Latitude) -> Fisheye
fromLongLatToFisheye f ll = Fisheye r t
    where
        (x,y,z) = (longLatToPoint ll) :: Point
        t = atan2 z x :: Radian
        r = ((2 * atan2 (sqrt (x^2 + z^2)) (y)) / (f * num_piS)) :: Scalar

-- f is aperature
fromFisheyeToLongLat :: Scalar -> Fisheye -> (Longitude, Latitude)
fromFisheyeToLongLat f (Fisheye r t) = (long, lat)
    where
        x = r * cos(t) :: Scalar
        y = r * sin(t) :: Scalar
        long = atan2 y x :: Longitude
        lat = scalarToLat $ (r * f) / 2

-- Little planet => http://codeofthedamned.com/index.php/the-little-planet-effect
-- for
-- Stereographic Projection => https://mathworld.wolfram.com/StereographicProjection.html
-- TODO: We need the if zero thing here as well
fromSteroToRectilinear :: (Longitude, Latitude) -> (Longitude, Latitude) -> Rectilinear
fromSteroToRectilinear (lam_0, phi_1) (lam, phi) = Rectilinear x y
  where
    -- it is supposed to be 2R but I am assuming unit sphere
    k = (/) 2 $ 1 + (sin(phi_1) * sin(phi)) + (cos(phi_1) * cos(phi) * cos (lam - lam_0))
    x = k * cos(phi) * sin (lam - lam_0)
    y = k * (cos(phi_1) * sin(phi) - sin(phi_1) * cos(phi) * cos (lam - lam_0))

-- TODO: We need the if zero thing here as well
fromRectilinearToStereo :: (Longitude, Latitude) -> Rectilinear -> (Longitude, Latitude)
fromRectilinearToStereo (lam_0, phi_1) (Rectilinear x y) = ifZero p (lam_0,phi_1) (lam', phi')
  where
    p    = sqrt (x*x + y*y) :: Scalar
    c    = (2 * atan2 p 2) :: Radian -- also should be 2R
    phi' = asin (cos(c) * sin(phi_1) + (y * sin(c) * cos(phi_1) / p))
    lam' = (+) (lam_0) $ Longitude $ atan $ (x * sin(c)) / (p * cos(phi_1) * cos(c) - y * sin(phi_1) * sin(c))

longLatToPoint2D :: (Longitude, Latitude) -> Point2D
longLatToPoint2D (long, lat) = (x,y)
    where
        x = (longToScalar long) / num_piS
        y = (latToScalar lat) * 2 / num_piS

normPoint2DToLongLat :: Point2D -> (Longitude, Latitude)
normPoint2DToLongLat (x,y) = (long, lat)
    where
        long = scalarToLong $ x * num_piS
        lat = scalarToLat $ (y * num_piS) / 2

algebraicStereoThroughNeg1 :: Scalar -> Point2D -> (Longitude, Latitude)
algebraicStereoThroughNeg1 s (x,y) = pointToLongLat (x * t, y * t, 1 - 2 * t)
    where
        -- multiply each point times 10 (easier to sift)
        --scale = (sqrt $ ( (8/s) - 4)) / 2
        r_sq = s * (x*x + y*y)
        t = 4 / (r_sq + 4)

algebraicStereoThroughPos1 :: Scalar -> Point2D -> (Longitude, Latitude)
algebraicStereoThroughPos1 ap (x,y) = pointToLongLat (x * t, y * t, (2*t) - 1)
    where
        -- multiply each point times 10 (easier to sift)
        --scale = (sqrt $ ( (8/s) - 4)) / 2
        r_sq = ap * (x*x + y*y)
        t = 4 / (r_sq + 4)

translate :: Scalar -> Scalar -> Point2D -> Point2D
translate dx dy (x,y) = (x + dx, y + dy)

scale :: Scalar -> Scalar -> Point2D -> Point2D
scale sx sy (x,y) = (sx * x, sx * y)

rotate :: Scalar -> Point2D -> Point2D
rotate ang (x,y) = (x*c - y*s, y*c + x*s)
    where
        s = sin ang
        c = cos ang
