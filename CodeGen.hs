
module CodeGen where

import System.IO (writeFile)

import Expr
import Optimizer
import Image
import Utils
import Types


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
outputCode' :: (Var a, ToExpr b) => String -> String -> Language -> [(String,(a -> b))] -> IO ()--[((String,[String]),(a -> b))] -> IO ()
outputCode' path modname lang fs = do
    es <- sequence $ map (\(n,f) -> do x <- reifyFunction f; return (n, muConversion (maxNode x) x)) fs
    transliterate' path modname lang es


--Takes a list of exprfunctions, optimizes them, and them writes them to file where f_i is the ith function of the list
transliterate :: String -> Language -> [ExprFunction] -> IO ()
transliterate path lang fs = writeFile path output_string
    where
        labelFcn _ [] = []
        labelFcn i (x:xs) = ("f" ++ (show i) ++ " = " ++ x):(labelFcn (i+1) xs)
        output_string = unlines $ labelFcn 1 $ map (\f@(ExprFunction as vs r)-> (showLang lang) $ muConversion (V $ (length as) + length vs) f) fs

transliterate' :: String -> String -> Language -> [(String, ExprFunction)] -> IO ()
transliterate' path modname lang fs = writeFile path $ unlines $ (++) modstring $ map fn fs
    where
        modstring = ["module " ++ modname ++ " where"]
        fn = (\(n,f@(ExprFunction as vs r))-> (++) (n ++ " = ") $ (showLang lang) $ muConversion (V $ (length as) + length vs) f)

showLang :: Language -> (ExprFunction -> String)
showLang lang = case lang of
                    JS      -> show . JavaScript
                    HSKL    -> show . Haskell
                    _       -> error "unknown language"

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

castDouble :: [String] -> String
castDouble [] = ""
castDouble (a:as)   | null as = a ++ "::Double"
                    | otherwise = a ++ "::Double, " ++ (castDouble as)

listToTuple' :: [String] -> String
listToTuple' [] = ""
listToTuple' (x:[]) = x
listToTuple' (x:xs) = x ++ ", " ++ (listToTuple' xs)

--------------------------------------------------------------------------------------------------

data Language = JS | HSKL | C

listToTuple :: Show a => [a] -> String
listToTuple [] = ""
listToTuple (x:[]) = show x
listToTuple (x:xs) = (show x) ++ ", " ++ (listToTuple xs)

newtype JavaScript = JavaScript ExprFunction

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
      showAssign (v,ExpNeg e)       = showHaskell v $ "neg " ++ show e
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


regenFunctions :: IO ()
regenFunctions = outputCode' "./funcs.hs" "Funcs" HSKL [unFisheye,inverseFisheye]
    where
        unFisheye = ("unFisheyeTransform", (\ h w s f x y -> unnormalize' h w $ rotate (num_piS / 2) $ targetPtToFishImage f $ normalize'' s s (x,y)))
        inverseFisheye = ("inverseFisheye", (\ h w s f x y -> unnormalize' h w $ (\(x',y') ->((longToScalar x')/num_piS, (latToScalar y') * 2/num_piS)) $ equiRecFisheyeToLongLat f $ normalize'' s s (x,y)))
