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
    thing <- readImage "./pano.jpeg"
    let filename = "./test.jpeg"
    case thing of
            Left err -> putStrLn ("Could not read image: " ++ err)
            Right img -> (saveJpgImage 100 filename . ImageRGB16 . fisheyeTransform . convertRGB16 . dynSquare) img
    print "done"
{-
case evalMu . toMuExpr . fromLongLatToFisheye 1
$ normPoint2DToLongLat . normalize --in PixelCoord
-- extraxt -}

dynWidth :: DynamicImage -> Int
dynWidth img = dynamicMap imageWidth img

dynHeight :: DynamicImage -> Int
dynHeight img = dynamicMap imageHeight img

dynSquare :: DynamicImage -> DynamicImage
dynSquare = dynamicPixelMap squareImage

squareImage :: Pixel a => Image a -> Image a
squareImage img = generateImage (\x y -> pixelAt img x y) edge edge
    where
        edge = min (imageWidth img) (imageHeight img)

-- reference this: https://www.stackbuilders.com/tutorials/haskell/image-processing/
fisheyeTransform :: Pixel a => Image a -> Image a
fisheyeTransform img@Image {..} = runST $ do
    mimg <- M.newMutableImage imageWidth imageHeight
    let go x y  | x >= imageWidth  = go 0 (y + 1)
                | y >= imageHeight = M.freezeImage mimg
                | otherwise = do
                    {-writePixel mimg
                      (imageWidth - x - 1)
                      (imageHeight - y - 1)
                      (pixelAt img x y)-}
                    let ll = pixelCoordToLongLat imageHeight imageWidth (x,y)
                    let a = extractTuple $ evalMu $ toMuExpr $ fromLongLatToFisheye (1.0 :: Scalar) ll
                    let (x',y') = fisheyeToPixelCoord imageHeight imageWidth a
                    writePixel mimg x' y' (pixelAt img x y)
                    go (x + 1) y
    go 0 0

extractTuple :: Value -> (Double, Double)
extractTuple t = case t of Tuple [Double x, Double y] -> (x,y)

-- origin = (width/2, height/2) val = (x - x0, y - y0)
-- also need to normalize and floor
-- image to LongLat        n = sqrt (x*x + y*y)
type Double2D = (Double,Double)
type PixelCoord = (Int, Int)
type Height = Int
type Width = Int

normalize :: Height -> Width -> PixelCoord -> Point2D
normalize h w (x,y) = (x', y')
    where
        dx = div w 2
        dy = div h 2
        x' = (fromIntegral $ x - dx) / fromIntegral dx
        y' = (fromIntegral $ dy - y) / fromIntegral dy

unnormalize :: Height -> Width -> Double2D -> PixelCoord
unnormalize h w (x', y') = (x,y)
    where
        dx = fromIntegral $ div w 2
        dy = fromIntegral $ div h 2
        x  = round $ (dx * x') + dx
        y  = round $ dy - (dy * y')

pixelCoordToLongLat :: Height -> Width -> PixelCoord -> (Longitude, Latitude)
pixelCoordToLongLat h w p = normPoint2DToLongLat $ normalize h w p

fisheyeToPixelCoord :: Height -> Width -> Double2D -> PixelCoord
fisheyeToPixelCoord h w fe = unnormalize h w $ filterRadius fe

filterRadius :: (Double, Double) -> (Double, Double)
filterRadius (r, t) | r >= pi = (- pi, pi / 2)
                    | otherwise = ((r / pi) * P.cos t, (r/pi) * P.sin t)
