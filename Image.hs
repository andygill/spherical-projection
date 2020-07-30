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
import Control.Applicative (liftA2)

import Expr
import Types as T
import Utils

runner :: [String] -> IO ()
runner [pathFrom, pathTo, ext, transform] = do
    inImage <- readImage pathFrom
    case inImage of
        Left err -> error ("Could not read image: " ++ err)
        --Right img -> putStrLn $ "Height: " ++ show (dynHeight img) ++ " Width: " ++ show (dynWidth img)
        Right img -> do
            putStrLn $ "Transform: " ++ t
            putStrLn $ "File Save format: " ++ ext
            putStrLn $ "From File: \"" ++ pathFrom ++ "\""
            putStrLn $ "Save Path: \"" ++ pathTo ++ "\""
            (s . f . convertRGB8) img
            where
                t = case transform of
                    "1" -> "inverseFisheyeTransform"
                    "2" -> "fisheyeToPano"
                    "3" -> "inversePanoToLittlePlanet"
                    "4" -> "panoToLittlePlanet"
                    _   -> error "Invalid transform option"
                s = case ext of
                    "png"   -> writePng pathTo
                    "jpeg"  -> saveJpgImage 100 pathTo . ImageRGB8
                    "tga"   -> writeTga pathTo
                    _       -> error "Invalid extension"
                f = case transform of
                    "1" -> inverseFisheyeTransform
                    "2" -> fisheyeToPano
                    "3" -> inversePanoToLittlePlanet
                    "4" -> panoToLittlePlanet
                    _   -> error "Invalid transform option"
    print "done"

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
                        let (x',y') = longLatDoubleToPixelCoord imageHeight imageWidth $ extractTuple $ evalMu $ toMuExpr $  normFisheyeToLongLat (35/4) $ normalize size size (x,y)
                        if x' >= imageWidth || x' < 0 || y' >= imageHeight || y' < 0 then
                            writePixel mimg x y $ PixelRGB8 0 0 0
                        else
                            writePixel mimg x y $ pixelAt img x' y'
                    else
                        writePixel mimg x y $ PixelRGB8 0 0 0
                    go (x + 1) y
    go 0 0

fisheyeToPano :: Pixel a => Image a -> Image a
fisheyeToPano img@Image {..} = runST $ do
    let size = min imageHeight imageWidth
    mimg <- M.newMutableImage size size
    let go x y  | x >= imageWidth = go 0 $ y + 1
                | y >= imageHeight = M.freezeImage mimg
                | otherwise = do
                    let (x',y') = longLatDoubleToPixelCoord size size $ extractTuple $ evalMu $ toMuExpr $ equiRecFisheyeToLongLat (4/2.8) $ normalize imageHeight imageWidth (x,y)
                    writePixel mimg x' y' $ pixelAt img x y
                    go (x + 1) y
    go 0 0

inversePanoToLittlePlanet :: Image PixelRGB8 -> Image PixelRGB8
inversePanoToLittlePlanet img@Image {..} = runST $ do
    mimg <- M.newMutableImage imageWidth imageHeight
    let go x y  | x >= imageWidth = go 0 $ y + 1
                | y >= imageHeight = M.freezeImage mimg
                | otherwise = do
                    let (x',y') = longLatDoubleToPixelCoord imageHeight imageWidth $ extractTuple $ evalMu $ toMuExpr $ fromRectilinearToStereo (0,scalarToLat $ num_pi/2) $ point2DtoRectilinear $ normalize imageHeight imageWidth (x,y)
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
                    let (x',y') = longLatDoubleToPixelCoord imageHeight imageWidth $ extractTuple $ evalMu $ toMuExpr $ algebraicStereoThroughNeg1 10 $ normalize size size (x,y)
                    if x' >= imageWidth || x' < 0 || y' >= imageHeight || y' < 0 then
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
{-}
opt :: (Num a, Eq a) => Expr a -> Expr a
opt (ExpScalar a) = (ExpScalar a)
opt (ExpSin (ExpScalar 0)) = ExpScalar 0
opt (ExpSin (ExpScalar a)) = ExpSin a
opt (ExpSin a) = opt $ ExpSin $ opt a
opt (ExpCos (ExpScalar 0)) = ExpScalar 1.0
opt (ExpCos (ExpScalar a)) = ExpCos a
opt (ExpCos a) = opt $ ExpSin $ opt a
opt (ExpMul (ExpScalar 0) _) = ExpScalar 0
opt (ExpMul _ (ExpScalar 0)) = ExpScalar 0
opt (ExpMul (ExpScalar 1) a) = ExpId $ opt a
opt (ExpMul a (ExpScalar 1)) = ExpId $ opt a
--opt (ExpMul a b) = opt $ ExpMul (opt $ ExpId a) (opt $ ExpId b)
opt (ExpDiv 0 _) = ExpScalar 0
opt (ExpDiv _ 0) = error "Divide by Zero"
opt (ExpDiv 0 0) = error "Zero over Zero"
opt a = a -}

normalize :: Height -> Width -> PixelCoord -> Point2D
normalize h w (x,y) = (x', y')
    where
        dx = div w 2
        dy = div h 2
        x' = (fromIntegral x / fromIntegral dx) - 1
        y' = (-) 1 $ fromIntegral y / fromIntegral dy

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

--Projection plane at (-1), essentially an equivalent val
projStereoNeg1ToLongLat :: (Longitude, Latitude) -> (Longitude, Latitude)
projStereoNeg1ToLongLat (long, lat) = ((scalarToLong c) * long, (scalarToLat c) * lat)
    where
        c = (1 - T.sin lat) / 2
{-}
normFisheyeToPoint' :: Double -> Double2D -> (Double, Double, Double)
normFisheyeToPoint' ap (x,y) = (p_x, p_y, p_z)
    where
        r   = sqrt $ x*x + y*y
        a   = r * ap / 2
        p_x = (P.sin a) * x / r
        p_y = P.cos a
        p_z = (P.sin a) * y / r

normFisheyeToLongLat :: Scalar -> Point2D -> (Longitude, Latitude)
normFisheyeToLongLat ap (x,y) = (long, lat)
    where
        r   = sqrt $ x*x + y*y
        a   = toRadian $ r * ap / 2
        long= T.acos $ r / (y * (T.sin(a) / T.cos(a)))
        lat = T.asin $ y * T.sin(a) / r

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
longLatDoubleToPixelCoord h w (x,y) = unnormalize h w  (x/pi, y*2/pi)--filterBadBoys (x/pi, y*2/pi)

-- if the abs (x,y) > 1 then put the point in teh top left corner, otherwise continue
filterBadBoys :: Double2D -> Double2D
filterBadBoys (x,y) | abs x > 1.0 || abs y > 1.0 = (-1.0, 1.0)
                    | otherwise = (x,y)

filterRadius :: Double2D -> Double2D
filterRadius (r, t) | r >= pi = (- pi, pi / 2)
                    | otherwise = ((r / pi) * P.cos t, (r/pi) * P.sin t)
