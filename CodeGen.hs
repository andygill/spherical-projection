
module CodeGen where

import System.IO (writeFile)
import Data.Char (isAlphaNum)

import Expr
import Optimizer
import Image
import Utils
import Types

-- takes an output file path, a Language Data Type, and a list of functions to be reifyed, and prints them to file.
outputCode :: (Var a, ToExpr b) => String -> Language -> [(a -> b)] -> IO ()
outputCode path lang fs = do
    es <- sequence $ map (\f -> do x <- reifyFunction f; return $ muConversion (maxNode x) x) fs
    transliterate path lang es

{-

newtype Fun where
 Fun :: Var a, ToExpr b => (a -> b) -> Fun 
 Fun :: (…) :: [String] -> (a -> b) -> ...
 Fun :: (…) => [String] -> (a -> b) -> Fun 
\ a -> sin a
 \ a -> call a “a” $ sin a 
name :: (Var a, ToExpr b) => a -> String -> b -> b
 \ a -> name a “a” $ sin a
 \ (B a) -> sin a
 newtype B where B :: Var a -> a -> B

TODO∷ Figure out how to name variables before output.
-}
-- takes an output file path, a Language Data Type, and a list of tuples with the functions to be reifyed and a string for its name, and prints them to file.
outputCode' :: (Var a, ToExpr b) => String -> String -> Language -> [(String,(a -> b))] -> IO ()--[((String,[String]),(a -> b))] -> IO ()
outputCode' path modname lang fs = do
    es <- sequence $ map (\(n,f) -> do x <- reifyFunction f; return (n, muConversion (maxNode x) x)) fs
    transliterate' path modname lang es

-- takes an output file path, a Language Data Type, and a list of tuples with the functions to be reifyed, a string for the function name and an ordered list of input variable names (strings), and prints them to file.
outputCode'' :: (Var a, ToExpr b) => String -> String -> Language -> [(String, [String], (a -> b))] -> IO ()--[((String,[String]),(a -> b))] -> IO ()
outputCode'' path modname lang fs = do
    es <- sequence $ map (\(n, as, f) -> do x <- reifyFunction f; return (n, as, muConversion (maxNode x) x)) fs
    transliterate'' path modname lang es

--Takes a list of exprfunctions, optimizes them, and them writes them to file where f_i is the ith function of the list
transliterate :: String -> Language -> [ExprFunction] -> IO ()
transliterate path lang fs = writeFile path output_string
    where
        labelFcn _ [] = []
        labelFcn i (x:xs) = ("f" ++ (show i) ++ " = " ++ x):(labelFcn (i+1) xs)
        output_string = unlines $ labelFcn 1 $ map (\f@(ExprFunction as vs r)-> (showLang lang) $ muConversion (V $ (length as) + length vs) f) fs

-- same as above but can write a function name as well
transliterate' :: String -> String -> Language -> [(String, ExprFunction)] -> IO ()
transliterate' path modname lang fs = writeFile path $ unlines $ (++) modstring $ map fn fs
    where
        modstring = ["module " ++ modname ++ " where"]
        fn = (\(n,f@(ExprFunction as vs r))-> (++) (n ++ " = ") $ (showLang lang) $ muConversion (V $ (length as) + length vs) f)

-- same as above, but also changes the input variables to the strings supplied
transliterate'' :: String -> String -> Language -> [(String, [String], ExprFunction)] -> IO ()
transliterate'' path modname lang fs = writeFile path $ unlines $ (++) modstring $ map fn fs
    where
        modstring = ["module " ++ modname ++ " where"]
        fn = (\(n, names, f@(ExprFunction as vs r))-> checkString names (map show as) $ (++) (n ++ " = ") $ (showLang lang) $ muConversion (V $ (length as) + length vs) f)

-- a utility function that allows for prettier function composition
showLang :: Language -> (ExprFunction -> String)
showLang lang = case lang of
                    JS      -> show . JavaScript
                    HSKL    -> show . Haskell
                    _       -> error "unknown language"

-- A special find and replace function that is used to replace variable names
findAndReplace :: (String, Int, String) -> Int -> String -> String
findAndReplace a@(k,lk,r) sl (s:ss) | lk > sl = (s:ss)
                                    | not $ isAlphaNum s = [s] ++ (findAndReplace a (sl - 1) ss)
                                    | k == take lk (s:ss) = if lk /= sl
                                        then
                                            if not $ isAlphaNum $ (s:ss) !! lk then
                                                r ++ (findAndReplace a (sl - lk) $ drop lk (s:ss))
                                            else --the next character after is not also a part of the variable name
                                                [s] ++ (findAndReplace a (sl - 1) ss)
                                        else --reach end of string and made a match
                                            r
                                    | otherwise = [s] ++ (findAndReplace a (sl - 1) ss)


checkString :: [String] -> [String] -> String -> String
checkString [] _ s = s
checkString _ [] s = s
checkString (n:ns) (a:as) s | n == "" = checkString ns as s
                            | otherwise = checkString ns as $ findAndReplace (a, length a, n) (length s) s
--    where
--        new_s = unwords . (map (\x -> findAndReplace (a, length a, n) (length x) x)) . words

-- used for writing the QuickCheck functions
compareCode :: String -> ExprFunction -> IO ()
compareCode path f@(ExprFunction as vs r) = do
    let vStart = maxNode f
    let args = ["a" ++ (show i) | i <- [1..(length as)]]
    let f1 = Haskell f
    let f2 = Haskell $ muConversion vStart f
    let output_string = unlines $
                        ["import Test.QuickCheck",""] ++
                        ["prop_f (" ++ (castDouble args) ++ ") = f1 " ++ (if null args then "" else "(" ++ (listToTuple' args) ++ ")")  ++ " == f2 " ++ (if null args then "" else "(" ++ (listToTuple' args) ++ ")")] ++
                        ["f1 = " ++ (show f1), "f2 = " ++ (show f2)]
    writeFile path output_string

-- Maaking in input variables doubles for QuickCheck
castDouble :: [String] -> String
castDouble [] = ""
castDouble (a:as)   | null as = a ++ "::Double"
                    | otherwise = a ++ "::Double, " ++ (castDouble as)

-- Used in changing the tuples stored as lists into haskell form tuples
listToTuple' :: [String] -> String
listToTuple' [] = ""
listToTuple' (x:[]) = x
listToTuple' (x:xs) = x ++ ", " ++ (listToTuple' xs)

--------------------------------------------------------------------------------------------------

data Language = JS | HSKL | C

langExt :: Language -> String
langExt lang = case lang of
        JS -> ".js"
        HSKL -> ".hs"
        C -> ".c"
        _ -> error "Unknown Language token"

listToTuple :: Show a => [a] -> String
listToTuple [] = ""
listToTuple (x:[]) = show x
listToTuple (x:xs) = (show x) ++ ", " ++ (listToTuple xs)

newtype JavaScript = JavaScript ExprFunction

-- the specialized function writer for Javascript based off the ExprFunction and how show is implemented in Expr.hs
instance Show JavaScript where
  show (JavaScript (ExprFunction as xs r)) = unlines $
      ["((" ++ (if null as then "" else show as) ++ ") => {"] ++
      map showAssign xs ++
      [ "  return " ++ show r ++ ";"
      , "})"
      ]
    where
      showAssign (v,ExpScalar e)    = "  let " ++ show v ++ " = " ++ show e ++ ";"
      showAssign (v,ExpTuple es)    = "  let " ++ show v ++ " = " ++ show es ++ ";"
      showAssign (v,ExpNorm es)     = "  let " ++ show v ++ " = Math.hypot(" ++ (init (tail (show es))) ++ ");"
      showAssign (v,ExpSin e)       = "  let " ++ show v ++ " = Math.sin(" ++ show e ++ ");"
      showAssign (v,ExpCos e)       = "  let " ++ show v ++ " = Math.cos(" ++ show e ++ ");"
      showAssign (v,ExpTan e)       = "  let " ++ show v ++ " = Math.tan(" ++ show e ++ ");"
      showAssign (v,ExpAsin e)      = "  let " ++ show v ++ " = Math.asin(" ++ show e ++ ");"
      showAssign (v,ExpAcos e)      = "  let " ++ show v ++ " = Math.acos(" ++ show e ++ ");"
      showAssign (v,ExpAtan e)      = "  let " ++ show v ++ " = Math.atan(" ++ show e ++ ");"
      showAssign (v,ExpSqrt e)      = "  let " ++ show v ++ " = Math.sqrt(" ++ show e ++ ");"
      showAssign (v,ExpAbs e)       = "  let " ++ show v ++ " = Math.abs(" ++ show e ++ ");"
      showAssign (v,ExpSignum e)    = "  let " ++ show v ++ " = Math.sign(" ++ show e ++ ");"
      showAssign (v,ExpNeg e)       = "  let " ++ show v ++ " = (-1) * " ++ show e ++ ";"
      showAssign (v,ExpAdd e1 e2)   = "  let " ++ show v ++ " = " ++ show e1 ++ " + " ++ show e2 ++ ";"
      showAssign (v,ExpSub e1 e2)   = "  let " ++ show v ++ " = " ++ show e1 ++ " - " ++ show e2 ++ ";"
      showAssign (v,ExpMul e1 e2)   = "  let " ++ show v ++ " = " ++ show e1 ++ " * " ++ show e2 ++ ";"
      showAssign (v,ExpDiv e1 e2)   = "  let " ++ show v ++ " = " ++ show e1 ++ " / " ++ show e2 ++ ";"
      showAssign (v,ExpPower e1 e2) = "  let " ++ show v ++ " = Math.pow(" ++ show e1 ++ ", " ++ show e2 ++");"
      showAssign (v,ExpAtan2 e1 e2) = "  let " ++ show v ++ " = Math.atan2(" ++ show e1 ++ ", " ++ show e2 ++");"
      showAssign (v,ExpRectilinear e1 e2) = "  let " ++ show v ++ " = { x : " ++ show e1 ++ ", y : " ++ show e2 ++"};"
      showAssign (v,ExpFisheye e1 e2) = "  let " ++ show v ++ " = { r : " ++ show e1 ++ ", t : " ++ show e2 ++"};"
      showAssign (v,ExpIfZero i t e) = "  let " ++ show v ++ " = " ++ show i ++ "?" ++ show t ++ ":" ++ show e ++ ";"
      --showAssign (v,ExpVar i)
      showAssign (v,e) = "  // let " ++ show v ++ " = " ++ show e ++ ";"

newtype Haskell = Haskell ExprFunction


normListToString :: Int -> [V] -> String
normListToString n [] = ""
normListToString n (x:xs) = (show x) ++ "*" ++ (show x) ++ if n == 1 then "" else " + " ++ (normListToString (n-1) xs)

-- the specialized function writer for Haskell based off the ExprFunction and how show is implemented in Expr.hs

instance Show Haskell where
  show (Haskell (ExprFunction as xs r)) = unlines $
      ["(\\(" ++ (listToTuple as) ++ ") -> "] ++
      map showAssign xs ++
      ["    " ++ (show r) ++ ")"]
    where
      showAssign (v,ExpScalar e)    = showHaskell v $ show e
      showAssign (v,ExpTuple es)    = showHaskell v $ "(" ++ listToTuple es ++ ")"
      showAssign (v,ExpNorm es)     = showHaskell v $ "sqrt $ " ++ (normListToString (length es) es)
      showAssign (v,ExpSin e)       = showHaskell v $ "sin " ++ show e
      showAssign (v,ExpCos e)       = showHaskell v $ "cos " ++ show e
      showAssign (v,ExpTan e)       = showHaskell v $ "tan " ++ show e
      showAssign (v,ExpAsin e)      = showHaskell v $ "asin " ++ show e
      showAssign (v,ExpAcos e)      = showHaskell v $ "acos " ++ show e
      showAssign (v,ExpAtan e)      = showHaskell v $ "atan " ++ show e
      showAssign (v,ExpSqrt e)      = showHaskell v $ "sqrt " ++ show e
      showAssign (v,ExpAbs e)       = showHaskell v $ "abs " ++ show e
      showAssign (v,ExpSignum e)    = showHaskell v $ "signum " ++ show e
      showAssign (v,ExpNeg e)       = showHaskell v $ "negate " ++ show e
      showAssign (v,ExpAdd e1 e2)   = showHaskell v $ show e1 ++ " + " ++ show e2
      showAssign (v,ExpSub e1 e2)   = showHaskell v $ show e1 ++ " - " ++ show e2
      showAssign (v,ExpMul e1 e2)   = showHaskell v $ show e1 ++ " * " ++ show e2
      showAssign (v,ExpDiv e1 e2)   = showHaskell v $ show e1 ++ " / " ++ show e2
      showAssign (v,ExpPower e1 e2) = showHaskell v $ show e1 ++ " ^ " ++ show e2
      showAssign (v,ExpAtan2 e1 e2) = showHaskell v $ "atan2 " ++ show e1 ++ " " ++ show e2
      showAssign (v,ExpRectilinear e1 e2)   = showHaskell v $ "(" ++ show e1 ++ ", " ++ show e2 ++ ")"
      showAssign (v,ExpFisheye e1 e2)       = showHaskell v $ "(" ++ show e1 ++ ", " ++ show e2 ++ ")"
      showAssign (v,ExpIfZero i t e)        = showHaskell v $ "if " ++ show i ++ " then " ++ show t ++ " else " ++ show e
      showAssign (v,e) = "  // let " ++ show v ++ " = " ++ show e

showHaskell v se = "    let " ++ show v ++ " = " ++ se ++ " in"

-----------------------------------------------------------------------------------------------

-- This is a writer function that takes a list of functions and their names,runs them through the optimizer, and outputs them in valid Haskell used in image manipulations
regenFunctions :: IO ()
regenFunctions = outputCode' "./funcs.hs" "Funcs" HSKL [unFisheye, inverseFisheye]
    where
        unFisheye = ("unFisheyeTransform", (\ h w f x y _-> unnormalize' h w $ targetPtToFishImage f $ normalize'' h w (x,y)))
        inverseFisheye = ("inverseFisheye", (\ h w s f x y -> unnormalize' h w $ (\(x',y') ->((longToScalar x')/num_piS, (latToScalar y') * 2/num_piS)) $ equiRecFisheyeToLongLat f $ normalize'' s s (x,y)))

-- Same as above, but you can name the input variables
regenFunctions' :: Language -> IO ()
regenFunctions' lang = outputCode'' ("./funcs" ++ (langExt lang)) "Funcs" lang [unFisheye, equirecTofisheyeTransform, sphere2planeStereo, lambertEq2CircTransform, lambertCirc2EqTransform]
    where
--        unFisheye = ("unFisheyeTransform", ["height", "width", "aperture", "x", "y"], (\ h w f x y _-> unnormalize' h w $ targetPtToFishImage f $ normalize'' h w (x,y)))
        unFisheye = ("unFisheyeTransform", ["height", "width", "x", "y"], (\ h w x y _ _-> spec_unnorm h w $ (\(px,py,pz) -> (Types.atan2 pz px, (Types.acos py) / (toRadian num_piS))) $ radToP3 $ spec_norm h w x y))
        equirecTofisheyeTransform = ("equirecTofisheyeTransform", ["side", "r", "height", "width", "x", "y"], (\ s r h w x y -> unnormalize' s s  $ rectilinearToCurvilinear r $ longLatToPoint $ (\(x',y') ->( scalarToLong $ x'*num_piS, scalarToLat $ y' * num_piS/2)) $ normalize'' h w (x,y)))
        sphere2planeStereo = ("sphere2planeStereo", ["phi", "theta"], (\ p t _ _ _ _ -> rectilinear2point2D $ fromSteroToRectilinear (0,0) (scalarToLong p, scalarToLat t)))
        --lambertTransform = ("lambertTransform", ["height", "width", "s", "x", "y"], (\ h w s x y _-> unnormalize' h w $ (\(x',y') ->((longToScalar x')/num_piS, (latToScalar y') * 2/num_piS)) $ lambertFrom2DToSphere $ normalize'' s s (x,y)))
        lambertEq2CircTransform = ("lambertEq2CircTransform", ["height", "width", "side", "s", "x", "y"], (\ h w side s x y -> unnormalize' h w $ (\(x',y') ->((longToScalar x')/num_piS, (latToScalar y') * 2/num_piS)) $ lambertFrom2DToSphere $ lambertNorm side s (x,y)))
        --lambertEq2CircTransform = ("lambertEq2CircTransform", ["height", "width", "side", "s", "x", "y"], (\ h w side s x y ->  unnormalize' h w $ (\(x',y') ->((longToScalar x')/num_piS, (latToScalar y') * 2/num_piS)) $ lambertFrom2DToSphere $ lambertNorm size s (x,y)))
        lambertCirc2EqTransform = ("lambertCirc2EqTransform", ["side", "height", "width", "s", "x", "y"], (\ side h w s x y -> lambertUnNorm side s $ lambertFromSphereto2D  $ (\(x',y') ->(scalarToLong $ x' * num_piS, scalarToLat $ y' * num_piS/2)) $ normalize'' h w (x, y)))
