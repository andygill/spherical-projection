
module CodeGen where

import System.IO (writeFile)

import Expr
import Optimizer

compareCode :: String -> ExprFunction -> IO ()
compareCode path f@(ExprFunction as vs r) = do
    let vStart = (length as) + (length vs)
    let args = ["a" ++ (show i) | i <- [1..(length as)]]
    let f1 = Haskell f
    let f2 = Haskell $ muConversion (V vStart) f
    let output_string = unlines $
                        ["import Test.QuickCheck",""] ++
                        ["prop_f (" ++ (castDouble args) ++ ") = { let f1 = " ++ (show f1) ++ " in let f2 = " ++ (show f2) ++ " in "] ++
                        ["f1 " ++ (if null args then "" else "(" ++ (listToTuple' args) ++ ")")  ++ " == f2 " ++ (if null args then "" else "(" ++ (listToTuple' args) ++ ")")] 
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

--data Language = JavaScript | Haskell

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
      showAssign (v,ExpAtan2 e1 e2) = showHaskell v $ show e1 ++ " " ++ show e2
      showAssign (v,ExpRectilinear e1 e2) = showHaskell v $ "(" ++ show e1 ++ ", " ++ show e2 ++ ")"
      showAssign (v,ExpFisheye e1 e2)= showHaskell v $ "(" ++ show e1 ++ ", " ++ show e2 ++ ")"
      showAssign (v,ExpIfZero i t e) =showHaskell v $ "if " ++ show i ++ " then " ++ show t ++ " else " ++ show e
      --showAssign (v,ExpVar i)
      showAssign (v,e) = "  // let " ++ show v ++ " = " ++ show e

showHaskell v se = "    let " ++ show v ++ " = " ++ se ++ " in"
