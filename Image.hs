{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}
module Image where

import Prelude hiding (atan2, asin, acos, atan, (^))
import qualified Prelude as P
import Debug.Trace
import Codec.Picture
import qualified Codec.Picture.Types as M
import Control.Monad.ST
import Control.Monad
import System.Environment

import Expr
import Types
import Utils

testFunc = do
    thing <- readImage "./before-rotation.png"
    case thing of
            Left err -> putStrLn ("Could not read image: " ++ err)
            Right (ImageRGB8 img) -> (savePngImage "./test.png" . ImageRGB8 . fisheyeTransform) img
            Right _ -> putStrLn "Unexpected pixel format"
    --let abstractfunc = fisheyeToPixelCoord 100 200 $ extractTuple $ evalMu $ toMuExpr $ fromLongLatToFisheye (pixelCoordToLongLat 100 200 (50,100)) 1.0
    --putStrLn (show abstractfunc)
    --newImg <- withImage (dynWidth img) (dynHeight img) (\x y -> fisheyeTransform img x y)

    --saveJpgImage 100 "./test.jpg" newImg
    print "done"
{-
case evalMu . toMuExpr . fromLongLatToFisheye 1
$ normPoint2DToLongLat . normalize --in PixelCoord
-- extraxt -}

dynWidth :: DynamicImage -> Int
dynWidth img = dynamicMap imageWidth img

dynHeight :: DynamicImage -> Int
dynHeight img = dynamicMap imageHeight img

-- reference this: https://www.stackbuilders.com/tutorials/haskell/image-processing/
fisheyeTransform :: Image PixelRGB8 -> Image PixelRGB8
fisheyeTransform img@Image {..} = runST $ do
    mimg <- M.newMutableImage imageWidth imageHeight
    let go x y  | x >= imageWidth  = go 0 (y + 1)
                | y >= imageHeight = M.freezeImage mimg
                | otherwise = do
                    let (long, lat) = pixelCoordToLongLat imageHeight imageWidth (x,y)
                    -- let (long, lat) = pixelCoordToLongLat imageHeight imageWidth (x,y)
                    {-writePixel mimg
                      (imageWidth - x - 1)
                      (imageHeight - y - 1)
                      (pixelAt img x y)-}
                    let ll = pixelCoordToLongLat imageHeight imageWidth (x,y)
                    let (x',y') = fisheyeToPixelCoord imageHeight imageWidth $ extractTuple $ evalMu $ toMuExpr $ flip $ fromLongLatToFisheye 1.0 ll
                    writePixel mimg x' y' (pixelAt img x y)
                    go (x + 1) y
    go 0 0


extractTuple t = case t of Tuple [Double x, Double y] -> (x,y)

-- origin = (width/2, height/2) val = (x - x0, y - y0)
-- also need to normalize and floor
-- image to LongLat        n = sqrt (x*x + y*y)
type Point2D = (Double, Double)
type LongLat = (Double, Double)
type PixelCoord = (Int, Int)
type Height = Int
type Width = Int

longLatToMuLongLat (long,lat) = (scalarToLong (long :: Scalar), scalarToLat (lat :: Scalar))

normalize :: Height -> Width -> PixelCoord -> Point2D
normalize h w (x,y) = (x',y')
    where
        dx = div w 2
        dy = div h 2
        x' = (fromIntegral $ x - dx) / fromIntegral dx
        y' = (fromIntegral $ dy - y) / fromIntegral dy

unnormalize :: Height -> Width -> Point2D -> PixelCoord
unnormalize h w (x',y') = (x,y)
    where
        dx = fromIntegral $ div w 2
        dy = fromIntegral $ div h 2
        x = round $ (dx * x') + dx
        y = round $ (dy * y') - dy

pixelCoordToLongLat :: Height -> Width -> PixelCoord -> LongLat
pixelCoordToLongLat h w p = normPoint2DToLongLat $ normalize h w p

fisheyeToPixelCoord :: Height -> Width -> Point2D -> PixelCoord
fisheyeToPixelCoord h w (r,t) = unnormalize h w (r * P.cos t, r * P.sin t)

longLatToPoint2D :: LongLat -> Point2D
longLatToPoint2D (long, lat) = (x,y)
    where
        x = long / pi
        y = (2 * lat) / pi

normPoint2DToLongLat :: Point2D -> LongLat
normPoint2DToLongLat (x,y) = (long, lat)
    where
        long = x * pi
        lat = (y * pi) / 2
