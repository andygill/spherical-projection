
module Main where

import System.Environment
import Codec.Picture

import Prelude hiding (asin, acos, atan, tan, sin, cos, atan2, (^))
import qualified Prelude as P

import Image

main = do
    [transform, ext, pathFrom, pathTo] <- getArgs
    inImage <- readImage pathFrom
    case inImage of
        Left err -> error ("Could not read image: " ++ err)
        --Right img -> putStrLn $ "Height: " ++ show (dynHeight img) ++ " Width: " ++ show (dynWidth img)
        Right img -> do
            putStrLn $ "Transform: " ++ t
            putStrLn $ "File Save format: " ++ ext
            putStrLn $ "From File: \"" ++ pathFrom ++ "\""
            putStrLn $ "Save Path: \"" ++ pathTo ++ "\""
            (s . f) img
            where
                t = case transform of
                    "1" -> "equirecTofisheye"
                    "2" -> "unFisheye"
                    "3" -> "inversePanoToLittlePlanet"
                    "4" -> "panoToLittlePlanet"
                    "5" -> "panoToGnomic"
                    "6" -> "lambertEq2Circle"
                    "7" -> "lambertCircle2Eq"
                    "10" -> "rotateOrigin"
                    "11" -> "smoothImage"
                    _   -> error "Invalid transform option"
                s = case ext of
                    "png"   -> writePng pathTo
                    "jpeg"  -> saveJpgImage 100 pathTo . ImageRGB8
                    "tga"   -> writeTga pathTo
                    _       -> error "Invalid extension"
                f = case transform of
                    "1" -> equirecTofisheye . convertRGB8 . rotateOrigin (0,-45)
                    "2" -> unFisheye . convertRGB8
                    "3" -> inversePanoToLittlePlanet . convertRGB8
                    "4" -> panoToLittlePlanet . convertRGB8
                    "5" -> panoToGnomic . convertRGB8
                    "6" -> lambertEq2Circle . convertRGB8
                    "7" -> lambertCircle2Eq . convertRGB8
                    "10" -> convertRGB8 . rotateOrigin (0,90)
                    "11" -> smoothImage . convertRGB8
                    _   -> error "Invalid transform option"
    print "done"
