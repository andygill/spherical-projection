{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}
module Image where

import Prelude hiding (atan2, asin, acos, atan, (^))
import qualified Prelude as P
import Codec.Picture
import qualified Codec.Picture.Types as M
import Control.Monad.ST
import Control.Monad

import Expr
import Types as T
import Utils
--import GenFunctions
import Funcs

dynWidth :: DynamicImage -> Int
dynWidth img = dynamicMap imageWidth img

dynHeight :: DynamicImage -> Int
dynHeight img = dynamicMap imageHeight img

dynSquare :: DynamicImage -> DynamicImage
dynSquare = dynamicPixelMap squareImage

squareImage :: Pixel a => Image a -> Image a
--squareImage img = generateImage (\x y -> pixelAt img (x + (edge `div` 3)) y) edge edge
squareImage img = generateImage (\x y -> pixelAt img x y) edge edge
    where
        edge = min (imageWidth img) (imageHeight img)

-- reference this: https://www.stackbuilders.com/tutorials/haskell/image-processing/
inverseFisheyeTransform :: Image PixelRGB8 -> Image PixelRGB8
inverseFisheyeTransform img@Image {..} = runST $ do
    let size = min imageHeight imageWidth
    mimg <- M.newMutableImage size size
    let go x y  | x >= size = go 0 $ y + 1
                | y >= size = M.freezeImage mimg
                | otherwise = do
                    let (x1,y1) = normalize' size size (x,y)
                    if (x1*x1 + y1*y1) <= 1.0 then do
                        let (x',y') = (\(a,b) -> (round a, round b)) $ inverseFisheye (fromIntegral imageHeight, fromIntegral imageWidth, fromIntegral size, (35/4), fromIntegral x, fromIntegral y)
                        if x' >= imageWidth || x' < 0 || y' >= imageHeight || y' < 0 then
                            writePixel mimg x y $ PixelRGB8 0 0 0
                        else
                            writePixel mimg x y $ pixelAt img x' y'
                    else
                        writePixel mimg x y $ PixelRGB8 0 0 0
                    go (x + 1) y
    go 0 0

unFisheye :: Image PixelRGB8 -> Image PixelRGB8
unFisheye img@Image {..} = runST $ do
    let size = min imageHeight imageWidth
    mimg <- M.newMutableImage size size
    let go x y  | x >= size = go 0 $ y + 1
                | y >= size = M.freezeImage mimg
                | otherwise = do
                    --potential example: https://github.com/raboof/dualfisheye2equirectangular/blob/master/projection.c
                    --let (x',y') = unnormalize imageHeight imageWidth $ extractTuple $ evalMu $ toMuExpr $ targetPtToFishImage (35/4) $ normalize size size (x,y)
                    let (x',y') = (\(a,b) -> (round a, round b)) $ unFisheyeTransform (fromIntegral imageHeight, fromIntegral imageWidth, fromIntegral size, (35/4), fromIntegral x, fromIntegral y)
                    if x' >= imageWidth || x' < 0 || y' >= imageHeight || y' < 0 then
                        writePixel mimg x y $ PixelRGB8 0 0 0
                    else
                        writePixel mimg x y $ pixelAt img x' y'
                    go (x + 1) y
    go 0 0

inversePanoToLittlePlanet :: Image PixelRGB8 -> Image PixelRGB8
inversePanoToLittlePlanet img@Image {..} = runST $ do
    mimg <- M.newMutableImage imageWidth imageHeight
    let go x y  | x >= imageWidth = go 0 $ y + 1
                | y >= imageHeight = M.freezeImage mimg
                | otherwise = do
                    let (x',y') = longLatDoubleToPixelCoord imageHeight imageWidth $ extractTuple $ evalMu $ toMuExpr $ fromRectilinearToStereo (0,scalarToLat $ num_piS/2) $ point2DtoRectilinear $ normalize imageHeight imageWidth (x,y)
                    if x' >= imageWidth || x' < 0 || y' >= imageHeight || y' < 0 then
                        writePixel mimg x y $ PixelRGB8 0 0 0
                    else
                        writePixel mimg x y $ pixelAt img x' y'
                    go (x + 1) y
    go 0 0

panoToLittlePlanet :: Image PixelRGB8 -> Image PixelRGB8
panoToLittlePlanet img@Image {..} = runST $ do
    let size = min imageWidth imageHeight
    mimg <- M.newMutableImage size size
    let go x y  | x >= size = go 0 $ y + 1
                | y >= size = M.freezeImage mimg
                | otherwise = do
                    let (x',y') = longLatDoubleToPixelCoord imageHeight imageWidth $ extractTuple $ evalMu $ toMuExpr $ algebraicStereoThroughNeg1 (35/2) $ normalize size size (x,y)
                    if x' >= imageWidth || x' < 0 || y' >= imageHeight || y' < 0 then
                        writePixel mimg x y $ PixelRGB8 0 0 0
                    else
                        writePixel mimg x y $ pixelAt img x' y'

                    go (x + 1) y
    go 0 0

panoToGnomic :: Image PixelRGB8 -> Image PixelRGB8
panoToGnomic img@Image {..} = runST $ do
    let big = max imageWidth imageHeight
    let small = min imageWidth imageHeight
    mimg <- M.newMutableImage small big
    let go x y  | x >= small = go 0 $ y + 1
                | y >= big = M.freezeImage mimg
                | otherwise = do
                    let (x',y') = longLatDoubleToPixelCoord imageHeight imageWidth $ extractTuple $ evalMu $ toMuExpr $ fromRecilinear (0,0) $ point2DtoRectilinear $ normalize imageHeight imageWidth (x,y)
                    if x' >= small || x' < 0 || y' >= big || y' < 0 then
                        writePixel mimg x y $ PixelRGB8 0 0 0
                    else
                        writePixel mimg x y $ pixelAt img x' y'
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
type HeightS = Scalar
type WidthS = Scalar
type PixelCoordS = (Scalar, Scalar)

normalize :: Height -> Width -> PixelCoord -> Point2D
normalize h w (x,y) = (x', y')
    where
        dx = div w 2
        dy = div h 2
        x' = (fromIntegral x / fromIntegral dx) - 1
        y' = (-) 1 $ fromIntegral y / fromIntegral dy

normalize'' :: HeightS -> WidthS -> Point2D -> Point2D
normalize'' h w (x,y) = (x', y')
    where
        dx = w / 2
        dy = h / 2
        x' = (x / dx) - 1
        y' = 1 - (y / dy)

normalize' :: Height -> Width -> PixelCoord -> Double2D
normalize' h w (x,y) = (x', y')
    where
        dx = div w 2
        dy = div h 2
        x' = (fromIntegral x / fromIntegral dx) - 1
        y' = (-) 1 $ fromIntegral y / fromIntegral dy

unnormalize :: Height -> Width -> Double2D -> PixelCoord
unnormalize h w (x', y') = (x,y)
    where
        dx = fromIntegral $ div w 2
        dy = fromIntegral $ div h 2
        x  = round $ dx * (1 + x')
        y  = round $ dy * (1 - y')

unnormalize' :: HeightS -> WidthS -> PixelCoordS -> Point2D
unnormalize' h w (x', y') = ((w / 2) * (1 + x'), (h / 2) * (1 - y'))

point2DtoRectilinear :: Point2D -> Rectilinear
point2DtoRectilinear (x,y) = Rectilinear x y

pixelCoordToLongLat :: Height -> Width -> PixelCoord -> (Longitude, Latitude)
pixelCoordToLongLat h w p = normPoint2DToLongLat $ normalize h w p

fisheyeToPixelCoord :: Height -> Width -> Double2D -> PixelCoord
fisheyeToPixelCoord h w fe = unnormalize h w $ filterRadius fe

--inverse where it takes a pixel on the image to be created and seeing where it is on the original image
equiRecFisheyeToLongLat :: Scalar -> Point2D -> (Longitude, Latitude)
equiRecFisheyeToLongLat ap (x,y) = pointToLongLat (T.sin phi * T.cos theta, T.cos phi, T.sin phi * T.sin theta)
    where
        r       = sqrt $ x*x + y*y
        phi     = toRadian $ r * ap / 2
        theta   = atan2 y x :: Radian

-- SUPER MATHY
normFisheyeToLongLat :: Scalar -> Point2D -> (Longitude, Latitude)
normFisheyeToLongLat ap (x,y) = pointToLongLat (p_x, p_y, p_z)
    where
        r   = sqrt $ x*x + y*y
        a   = toRadian $ r * ap / 2
        p_x = (T.sin a) * x / r
        p_y = T.cos a
        p_z = (T.sin a) * y / r

targetPtToFishImage :: Scalar -> (Scalar, Scalar) -> PixelCoordS
targetPtToFishImage f (x, y) = (x', y')
    where
        (p_x, p_y, p_z) = longLatToPoint (scalarToLong $ x * num_piS, scalarToLat $ y * num_piS / 2)
        a = T.acos p_y
        r = (toScalar a) * 2 / f
        x' = p_z * r / (T.sin a)
        y' = p_x * r / (T.sin a)

--Projection plane at (-1), essentially an equivalent val
projStereoNeg1ToLongLat :: (Longitude, Latitude) -> (Longitude, Latitude)
projStereoNeg1ToLongLat (long, lat) = ((scalarToLong c) * long, (scalarToLat c) * lat)
    where
        c = (1 - T.sin lat) / 2

longLatDoubleToPixelCoord :: Int -> Int -> Double2D -> PixelCoord
longLatDoubleToPixelCoord h w (x,y) = unnormalize h w (x/pi, y*2/pi)--filterBadBoys (x/pi, y*2/pi)

-- if the abs (x,y) > 1 then put the point in teh top left corner, otherwise continue
filterBadBoys :: Double2D -> Double2D
filterBadBoys (x,y) | abs x > 1.0 || abs y > 1.0 = (-1.0, 1.0)
                    | otherwise = (x,y)

filterRadius :: Double2D -> Double2D
filterRadius (r, t) | r >= pi = (- pi, pi / 2)
                    | otherwise = ((r / pi) * P.cos t, (r/pi) * P.sin t)
