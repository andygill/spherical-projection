{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Data.Dynamic
import Data.Reify
import System.Environment
import Codec.Picture

import Prelude hiding (asin, acos, atan, tan, sin, cos, atan2, (^))
import qualified Prelude as P

import Image
import qualified Data.Graph as G

main = do
    [ext, transform, pathFrom, pathTo] <- getArgs
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
                    "2" -> "unFisheye"
                    "3" -> "inversePanoToLittlePlanet"
                    "4" -> "panoToLittlePlanet"
                    "5" -> "panoToGnomic"
                    _   -> error "Invalid transform option"
                s = case ext of
                    "png"   -> writePng pathTo
                    "jpeg"  -> saveJpgImage 100 pathTo . ImageRGB8
                    "tga"   -> writeTga pathTo
                    _       -> error "Invalid extension"
                f = case transform of
                    "1" -> inverseFisheyeTransform
                    "2" -> unFisheye
                    "3" -> inversePanoToLittlePlanet
                    "4" -> panoToLittlePlanet
                    "5" -> panoToGnomic
                    _   -> error "Invalid transform option"
    print "done"
