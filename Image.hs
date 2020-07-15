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
    --thing <- readImage "./pano.jpeg"
    --let filename = "./test.jpeg"
    thing <- readImage "./before-rotation.png"
    let filename = "./test.png"
    case thing of
            Left err -> putStrLn ("Could not read image: " ++ err)
            Right img -> (savePngImage filename . ImageRGB8 . inverseFisheyeTransform . convertRGB8 . dynSquare) img
            --Right img -> (saveJpgImage 100 filename . ImageRGB8 . inverseFisheyeTransform . convertRGB8 . dynSquare) img
            --Right img -> putStrLn $ "Height: " ++ show (dynHeight img) ++ " Width: " ++ show (dynWidth img)
    print $ "done"

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
                    let ll = pixelCoordToLongLat imageHeight imageWidth (x,y)
                    let a = extractTuple $ evalMu $ toMuExpr $ fromLongLatToFisheye (1.0 :: Scalar) ll
                    let (x',y') = fisheyeToPixelCoord imageHeight imageWidth a
                    writePixel mimg x' y' (pixelAt img x y)
                    go (x + 1) y
    go 0 0

inverseFisheyeTransform :: Image PixelRGB8 -> Image PixelRGB8
inverseFisheyeTransform img@Image {..} = runST $ do
    let radius = div (min imageWidth imageHeight) 2
    mimg <- M.newMutableImage radius radius
    let go x y  | x >= 2 * radius = go 0 (y + 1)
                | y >= 2 * radius = M.freezeImage mimg
                | otherwise = do
                    let (x1,y1) = normalize' radius radius (x,y)
                    if (x1*x1 + y1*y1) <= 1.0 then do
                        let norm = normalize radius radius (x,y)
                        let ll = extractTuple $ evalMu $ toMuExpr $ fromFisheyeToLongLat 1.0 $ equiRecToFisheye norm
                        let (x',y') = longLatDoubleToPixelCoord radius radius ll
                        writePixel mimg x' y' (pixelAt img x y)
                    else
                        writePixel mimg x y $ PixelRGB8 0 0 0
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

normalize' :: Height -> Width -> PixelCoord -> Double2D
normalize' h w (x,y) = (x', y')
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

--inverse where it takes a pixel on the image to be created and seeing where it is on the original image
equiRecToFisheye :: Point2D -> Fisheye
equiRecToFisheye (x,y) = Fisheye r t
    where
        r = sqrt $ x*x + y*y
        t = ifZero r 0 $ ifZero x (toRadian $ -num_pi) $ acos $ x / r :: Radian

        --t = ifZero r 0 $ ifZero x (toRadian $ num_pi / 2) (acos $ x / r)

{-
WHAT MY GOAL IS: This breaks up teh quadrants correctly
if (x > 0 && y > 0) || (x <= 0 && y > 0) -> acos x
if x > 0 && y <= 0 -> asin y
if x < 0 && y < 0 -> 2*pi - acos x


equiRecToFisheye :: Point2D -> Fisheye
equiRecToFisheye (x,y) = Fisheye r t
    where
        r = sqrt $ x*x + y*y
        t = case nearZero r of
            False -> acos $ x / r :: Radian
            True -> case x > 0 of
                True -> num_pi :: Radian
                False -> 0 :: Radian-}

longLatDoubleToPixelCoord :: Int -> Int -> Double2D -> PixelCoord
longLatDoubleToPixelCoord h w (x,y) = unnormalize h w (x/pi, y*2/pi)

filterRadius :: Double2D -> Double2D
filterRadius (r, t) | r >= pi = (- pi, pi / 2)
                    | otherwise = ((r / pi) * P.cos t, (r/pi) * P.sin t)
