module Image where

import Prelude hiding (sin, cos, atan2, asin, acos, atan, (^))
import qualified Prelude as P
import Debug.Trace
import Codec.Picture

import Expr
import Types
import Utils

testFunc = do
    thing <- readImage "./pano.jpeg"
    let img = case thing of
            Left m -> error m
            Right d -> d
    let abstractfunc = reifyFunction $ fromLongLatToFisheye (1.0,0.0)
    saveJpgImage 100 "./test.jpg" img
    print "done"


case evalMu . toMuExpr fromLongLatToFisheye 1
$ normPoint2DToLongLat . normalize
-- origin = (width/2, height/2) val = (x - x0, y - y0)
-- also need to normalize and floor
-- image to LongLat        n = sqrt (x*x + y*y)


normalize :: (Int, Int) -> (Int, Int) -> (Double, Double)
normalize (h,w) (x,y) = (x',y')
    where
        --normalized from (h/2)^2 + (w/2)^2
        dx = div w 2
        dy = div h 2
        x' = (fromIntegral $ x - dx) / fromIntegral dx
        y' = (fromIntegral $ dy - y) / fromIntegral dy

unnormalize :: (Int, Int) -> (Double, Double) -> (Int, Int)
unnormalize (h,w) (x',y') = (x,y)
    where
        dx = fromIntegral $ div w 2
        dy = fromIntegral $ div h 2
        x = round $ (dx * x') + dx
        y = round $ (dy * y') - dy

longLatToPoint2D :: (Double, Double) -> (Double, Double)
longLatToPoint2D (long, lat) = (x,y)
    where
        x = long / pi
        y = (2 * lat) / pi

normPoint2DToLongLat :: (Double, Double) -> (Double, Double)
normPoint2DToLongLat (x,y) = (long, lat)
    where
        long = x * pi
        lat = (y * pi) / 2
