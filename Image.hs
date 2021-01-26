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

smoothImage :: Image PixelRGB8 -> Image PixelRGB8
smoothImage img@Image{..} = generateImage inBoundPixel imageWidth imageHeight
    where
        inBoundPixel x y=if x >= imageWidth - 1
                        then
                            if y == imageHeight - 1
                                then pixelAt img x y
                                else lerpPixelY img (0.5 + (fromIntegral y)) x
                        else if y >= imageHeight - 1
                            then
                                if x == imageWidth - 1
                                    then pixelAt img x y
                                    else lerpPixelX img (0.5 + (fromIntegral x)) y
                            else
                                bilerpPixel img (0.5 + (fromIntegral x)) $ 0.5 + (fromIntegral y)

--bilerp :: Image PixelRGB8 -> Image PixelRGB8
--bilerp img = generateImage (\ x y -> bilerpPixel x y img) (imageWidth img) (imageHeight img)

bilerpPixel :: Image PixelRGB8 -> Double -> Double -> PixelRGB8
bilerpPixel img x y = PixelRGB8 r g b
    where
        fx = floor x
        cx = ceiling x
        fy = floor y
        cy = ceiling y
        (PixelRGB8 r00 g00 b00) = pixelAt img fx fy
        (PixelRGB8 r01 g01 b01) = pixelAt img fx cy
        (PixelRGB8 r10 g10 b10) = pixelAt img cx fy
        (PixelRGB8 r11 g11 b11) = pixelAt img cx cy
        dx = x - (fromIntegral fx)
        dy = y - (fromIntegral fy)
        r = round $ (1 - dx)*(1 - dy)*(fromIntegral r00) + dx*(1 - dy)*(fromIntegral r01) + (1 - dx)*dy*(fromIntegral r10) + dx*dy*(fromIntegral r11)
        g = round $ (1 - dx)*(1 - dy)*(fromIntegral g00) + dx*(1 - dy)*(fromIntegral g01) + (1 - dx)*dy*(fromIntegral g10) + dx*dy*(fromIntegral g11)
        b = round $ (1 - dx)*(1 - dy)*(fromIntegral b00) + dx*(1 - dy)*(fromIntegral b01) + (1 - dx)*dy*(fromIntegral b10) + dx*dy*(fromIntegral b11)

--linear interpolates in the x direction
lerpPixelX :: Image PixelRGB8 -> Double -> Int -> PixelRGB8
lerpPixelX img x y = PixelRGB8 r g b
    where
        fx = floor x
        cx = ceiling x
        (PixelRGB8 r0 g0 b0) = pixelAt img fx y
        (PixelRGB8 r1 g1 b1) = pixelAt img cx y
        dx = x - (fromIntegral fx)
        r = round $ (1 - dx)*(fromIntegral r0) + dx*(fromIntegral r1)
        g = round $ (1 - dx)*(fromIntegral g0) + dx*(fromIntegral g1)
        b = round $ (1 - dx)*(fromIntegral b0) + dx*(fromIntegral b1)

--linear interpolates in the y direction
lerpPixelY :: Image PixelRGB8 -> Double -> Int -> PixelRGB8
lerpPixelY img y x = PixelRGB8 r g b
    where
        fy = floor y
        cy = ceiling y
        (PixelRGB8 r0 g0 b0) = pixelAt img x fy
        (PixelRGB8 r1 g1 b1) = pixelAt img x cy
        dy = y - (fromIntegral fy)
        r = round $ (1 - dy)*(fromIntegral r0) + dy*(fromIntegral r1)
        g = round $ (1 - dy)*(fromIntegral g0) + dy*(fromIntegral g1)
        b = round $ (1 - dy)*(fromIntegral b0) + dy*(fromIntegral b1)


{-}
lerpPixels :: Int -> Int -> Int -> Int -> ImageRGB8 -> PixelRGB8
lerpPixels x1 y1 x2 y2 img =-}

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
{-
unFisheye :: Image PixelRGB8 -> Image PixelRGB8
unFisheye img@Image {..} = runST $ do
    let size = min imageHeight imageWidth
    mimg <- M.newMutableImage size size
    let go x y  | x >= imageWidth = go 0 $ y + 1
                | y >= imageHeight = M.freezeImage mimg
                | otherwise = do
                    --potential example: https://github.com/raboof/dualfisheye2equirectangular/blob/master/projection.c
                    --http://paulbourke.net/dome/fish2/fish2sphere.pdf
                    --let (x',y') = unnormalize imageHeight imageWidth $ extractTuple $ evalMu $ toMuExpr $ targetPtToFishImage (35/4) $ normalize size size (x,y)
                    let (x',y') = (\(a,b) -> (round a, round b)) $ unFisheyeTransform (fromIntegral imageHeight, fromIntegral imageWidth, (35/4), fromIntegral x, fromIntegral y)
                    if x' >= imageWidth || x' < 0 || y' >= imageHeight || y' < 0 then
                        writePixel mimg x y $ PixelRGB8 0 0 0
                    else
                        writePixel mimg x y $ pixelAt img x' y'
                    go (x + 1) y
    go 0 0
-}

unFisheye :: Image PixelRGB8 -> Image PixelRGB8
unFisheye img@Image {..} = runST $ do
    let size = min imageHeight imageWidth
    mimg <- M.newMutableImage imageWidth imageHeight
    let go x y  | x >= imageWidth = go 0 $ y + 1
                | y >= imageHeight = M.freezeImage mimg
                | otherwise = do

                    -- (\(t,p) -> ((p*cos(t)+0.5)*imageWidth, (p*sin(t)+0.5)*imageHeight,)) $ (\(x,y,z) -> (atan2 (-z) x, (acos y) / num_piS) $ longLatToPoint $ ((*) num_piS $ (-) 1 $ (fromIntegral x)/ (fromIntegral imageWidth), (*) num_piS $ (fromIntegral y) / (fromIntegral imageHeight))
                    --potential example: https://github.com/raboof/dualfisheye2equirectangular/blob/master/projection.c
                    --http://paulbourke.net/dome/fish2/fish2sphere.pdf
                    --let (x',y') = unnormalize imageHeight imageWidth $ extractTuple $ evalMu $ toMuExpr $ targetPtToFishImage (35/4) $ normalize size size (x,y)
                    --let (x',y') = (\(a,b) -> (round a, round b)) $ unFisheyeTransform (fromIntegral imageHeight, fromIntegral imageWidth, (35/4), fromIntegral x, fromIntegral y)
                    let (x',y') = placeInBound imageHeight imageWidth $ (\(a,b) -> (round a, round b)) $ unFisheyeTransform (fromIntegral imageHeight, fromIntegral imageWidth, fromIntegral x, fromIntegral y)
                    writePixel mimg x y $ pixelAt img x' y'
                    go (x + 1) y
    go 0 0


rotateOrigin :: Double2D -> DynamicImage -> DynamicImage
rotateOrigin origin img = dynamicPixelMap (remapOrigin p) img
    where
        p = findLambertOrigin (dynHeight img) (dynWidth img) origin

remapOrigin :: Pixel a => PixelCoord -> Image a -> Image a
remapOrigin (x_0, y_0) img@Image {..} = generateImage (\x y -> (\(x',y') -> pixelAt img x' y') $ wrapOrigin imageHeight imageWidth (x - x_0 + mx, y - y_0 + my)) imageWidth imageHeight
    where
        mx = div imageWidth 2
        my = div imageHeight 2

findLambertOrigin :: Int -> Int -> Double2D -> PixelCoord
findLambertOrigin h w (long, lat) = unnormalize h w (long/180, lat/90)

-- PRE: Change origin by x' = x - x_0; y' = y - y_0
wrapOrigin :: Int -> Int -> PixelCoord -> PixelCoord
wrapOrigin h w (x,y)| x < 0  = wrapOrigin h w (w + x, y)
                    | x >= w = wrapOrigin h w (w - x, y)
                    | y < 0  = wrapOrigin h w (x, h + y)
                    | y >= h = wrapOrigin h w (x, h - y)
                    | otherwise = (x,y)

lambertEq2Circle :: Image PixelRGB8 -> Image PixelRGB8
lambertEq2Circle img@Image {..} = runST $ do
    let size = min imageHeight imageWidth
    mimg <- M.newMutableImage size size
    let go x y  | x >= size = go 0 $ y + 1
                | y >= size = M.freezeImage mimg
                | otherwise = do
                    let (x',y') = (\(a,b) -> (round a, round b)) $ lambertEq2CircTransform (fromIntegral imageHeight, fromIntegral imageWidth, fromIntegral size, 2, fromIntegral x, fromIntegral y)
                    if x' < 0 || x' >= imageWidth || y' < 0 || y' >= imageHeight
                    then
                        writePixel mimg x y $ PixelRGB8 0 0 0
                    else
                        writePixel mimg x y $ pixelAt img x' y'
                    go (x + 1) y
    go 0 0

lambertNorm :: Scalar -> Scalar -> Point2D -> Point2D
lambertNorm size s (x,y) = translate (-1) 1 $ normalize'' size size (s * x, s * y)

lambertUnNorm :: Scalar -> Scalar -> Point2D -> Point2D
lambertUnNorm size s (x,y) = unnormalize' size size (s * x, s * y)--translate (-1) 1 $ unnormalize' size size (s * x, s * y)

lambertCircle2Eq :: Image PixelRGB8 -> Image PixelRGB8
lambertCircle2Eq img@Image {..} = runST $ do
    let width = imageHeight*2
    mimg <- M.newMutableImage width imageHeight
    let go x y  | x >= width = go 0 $ y + 1
                | y >= imageHeight = M.freezeImage mimg
                | otherwise = do
                    -- side, height, width, x, y
                    let (x',y') = lambertCirc2EqTransform (fromIntegral imageHeight, fromIntegral imageHeight, fromIntegral width, 1/2, fromIntegral x, fromIntegral y)
                    if x' < 0 || x' >= fromIntegral imageHeight - 1 || y' < 0 || y' >= fromIntegral imageHeight - 1
                    then
                        writePixel mimg x y $ PixelRGB8 0 0 0
                    else
                        writePixel mimg x y $ bilerpPixel img x' y'
                    {-let (x',y') = (\(a,b) -> (round a, round b)) $ lambertCirc2EqTransform (fromIntegral imageHeight, fromIntegral imageHeight, fromIntegral width, 1/2, fromIntegral x, fromIntegral y)

                    if x' < 0 || x' >= imageHeight || y' < 0 || y' >= imageHeight
                    then
                        writePixel mimg x y $ PixelRGB8 0 0 0
                    else
                        writePixel mimg x y $ pixelAt img x' y'-}

                    go (x + 1) y
    go 0 0

-- subtract by the origin, transform from spherical to UV, normalize UV to input image dimensions
-- this cannot work b/c it will create overly negative or positive values. So it must be done from the looping level
{-
lambertUnnorm :: Scalar -> Scalar -> (Longitude, Latitude) -> (Longitude, Latitude) -> Point2D
lambertUnnorm h w (long_0,lat_0) (long, lat) = unnormalize' h w $ (\(x',y') ->((longToScalar x')/num_piS, (latToScalar y') * 2/num_piS)) (long - long_0, lat - lat_0)
-}

degree2radian :: Double2D -> Double2D
degree2radian (long, lat) = (long*pi/180, lat*pi/180)

placeInBound :: Int -> Int -> (Int, Int) -> (Int, Int)
placeInBound h w (x,y)  | x < 0 = placeInBound h w (0,y)
                        | x >= w = placeInBound h w (w - 1, y)
                        | y < 0 = placeInBound h w (x, 0)
                        | y >= h = placeInBound h w (x, h - 1)
                        | otherwise = (x,y)
--new_test :: (Scalar -> Scalar -> Scalar -> Scalar) -> (Scalar, Scalar)
new_test = (\h w x y -> spec_unnorm h w $ (\(px,py,pz) -> (T.atan2 pz x, (T.acos py) / (toRadian num_piS))) $ radToP3 $ spec_norm h w x y)

spec_norm :: Scalar -> Scalar -> Scalar -> Scalar -> (Radian, Radian)
spec_norm h w x y = (t, p)
    where
        t = toRadian $ num_piS * (1 - (x / w))
        p = toRadian $ num_piS * (y / h)

radToP3 :: (Radian, Radian) -> (Scalar, Scalar, Scalar)
radToP3 (t, p) = (T.cos(t) * T.sin(p), T.sin(t) * T.sin(p), T.cos p)

spec_unnorm :: Scalar -> Scalar -> (Radian, Radian) -> (Scalar, Scalar)
spec_unnorm h w (p2,t2) = (x,y)
    where
        x = ((toScalar p2)*T.cos(t2::Radian)+0.5) * w
        y = ((toScalar p2)*T.sin(t2::Radian)+0.5) * h

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

rectilinear2point2D :: Rectilinear -> Point2D
rectilinear2point2D (Rectilinear x y) = (x,y)

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

--http://paulbourke.net/dome/fish2/fish2sphere.pdf
-- f is field of view in radian
unfish' :: Scalar -> (Scalar, Scalar) -> PixelCoordS
unfish' f (x,y) = (x, x*t)
    where
        (long, lat) = (toRadian $ x * num_piS, y*num_piS/2)
        t = T.tan long
        x = (2*lat) / (f*(1+t))
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
