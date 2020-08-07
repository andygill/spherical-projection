{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Expr where

import Data.Dynamic
import Data.Reify
import Debug.Trace
import Data.Maybe
import Control.Applicative
import qualified Data.Graph as G
import Data.Function(on)
import Data.List(sortBy)

import Prelude hiding (sin, cos, atan2, (^))
import qualified Prelude as P

data Expr :: * -> * where
  ExpScalar :: Double -> Expr t
  ExpSin    :: t -> Expr t
  ExpCos    :: t -> Expr t
  ExpTan    :: t -> Expr t
  ExpAsin   :: t -> Expr t
  ExpAcos   :: t -> Expr t
  ExpAtan   :: t -> Expr t
  ExpSqrt   :: t -> Expr t
  ExpAbs    :: t -> Expr t
  ExpSignum :: t -> Expr t
  ExpNeg    :: t -> Expr t
  ExpAdd    :: t -> t -> Expr t
  ExpSub    :: t -> t -> Expr t
  ExpMul    :: t -> t -> Expr t
  ExpDiv    :: t -> t -> Expr t
  ExpPower  :: t -> Int -> Expr t
  ExpAtan2  :: t -> t -> Expr t
  ExpLambda :: [Int] -> t -> Expr t
  ExpRectilinear :: t -> t -> Expr t
  ExpFisheye :: t -> t -> Expr t
  ExpIfZero :: t -> t -> t -> Expr t
  ExpTuple  :: [t] -> Expr t
  ExpVar    :: Int -> Expr t
  ExpId     :: t -> Expr t

deriving instance Show t => Show (Expr t)
deriving instance Eq t => Eq (Expr t)
deriving instance Ord t => Ord (Expr t)

instance Functor Expr where
  fmap f (ExpScalar d) = ExpScalar d
  fmap f (ExpSin t1) = ExpSin (f t1)
  fmap f (ExpCos t1) = ExpCos (f t1)
  fmap f (ExpTan t1) = ExpTan (f t1)
  fmap f (ExpAsin t1) = ExpAsin (f t1)
  fmap f (ExpAcos t1) = ExpAcos (f t1)
  fmap f (ExpAtan t1) = ExpAtan (f t1)
  fmap f (ExpSqrt t1) = ExpSqrt (f t1)
  fmap f (ExpAbs t1) = ExpAbs (f t1)
  fmap f (ExpNeg t1) = ExpNeg (f t1)
  fmap f (ExpSignum t1) = ExpSignum (f t1)
  fmap f (ExpAdd t1 t2) = ExpAdd (f t1) (f t2)
  fmap f (ExpSub t1 t2) = ExpSub (f t1) (f t2)
  fmap f (ExpMul t1 t2) = ExpMul (f t1) (f t2)
  fmap f (ExpDiv t1 t2) = ExpDiv (f t1) (f t2)
  fmap f (ExpPower t1 t2) = ExpPower (f t1) t2
  fmap f (ExpAtan2 t1 t2) = ExpAtan2 (f t1) (f t2)
  fmap f (ExpLambda vs t) = ExpLambda vs (f t)
  fmap f (ExpVar i) = ExpVar i
  fmap f (ExpRectilinear t1 t2) = ExpRectilinear (f t1) (f t2)
  fmap f (ExpFisheye t1 t2) = ExpFisheye (f t1) (f t2)
  fmap f (ExpIfZero t1 t2 t3) = ExpIfZero (f t1) (f t2) (f t3)
  fmap f (ExpTuple ts) = ExpTuple $ fmap f ts
  fmap f g = error $ show ("fmap" )
instance Foldable Expr where
  foldr f z (ExpScalar d) = z
  foldr f z (ExpSin t1) = f t1 z
  foldr f z (ExpCos t1) = f t1 z
  foldr f z (ExpTan t1) = f t1 z
  foldr f z (ExpAsin t1) = f t1 z
  foldr f z (ExpAcos t1) = f t1 z
  foldr f z (ExpAtan t1) = f t1 z
  foldr f z (ExpSqrt t1) = f t1 z
  foldr f z (ExpAbs t1) = f t1 z
  foldr f z (ExpSignum t1) = f t1 z
  foldr f z (ExpNeg t1) = f t1 z
  foldr f z (ExpAdd t1 t2) = f t1 (f t2 z)
  foldr f z (ExpSub t1 t2) = f t1 (f t2 z)
  foldr f z (ExpMul t1 t2) = f t1 (f t2 z)
  foldr f z (ExpDiv t1 t2) = f t1 (f t2 z)
  foldr f z (ExpPower t1 t2) = f t1 z
  foldr f z (ExpAtan2 t1 t2) = f t1 (f t2 z)
  foldr f z (ExpRectilinear t1 t2) = f t1 (f t2 z)
  foldr f z (ExpFisheye t1 t2) = f t1 (f t2 z)
  foldr f z (ExpIfZero t1 t2 t3) = f t1 (f t2 (f t3 z))
  foldr f z (ExpTuple ts) = foldr f z ts
  foldr f z (ExpVar _) = z
  foldr f z _ = error "foldr"

instance Traversable Expr where
  traverse f (ExpScalar d) = pure $ ExpScalar d
  traverse f (ExpSin t1) = ExpSin <$> f t1
  traverse f (ExpCos t1) = ExpCos <$> f t1
  traverse f (ExpTan t1) = ExpTan <$> f t1
  traverse f (ExpAsin t1) = ExpAsin <$> f t1
  traverse f (ExpAcos t1) = ExpAcos <$> f t1
  traverse f (ExpAtan t1) = ExpAtan <$> f t1
  traverse f (ExpSqrt t1) = ExpSqrt <$> f t1
  traverse f (ExpAbs t1) = ExpAbs <$> f t1
  traverse f (ExpSignum t1) = ExpSignum <$> f t1
  traverse f (ExpNeg t1) = ExpNeg <$> f t1
  traverse f (ExpAdd t1 t2) = ExpAdd <$> f t1 <*> f t2
  traverse f (ExpSub t1 t2) = ExpSub <$> f t1 <*> f t2
  traverse f (ExpMul t1 t2) = ExpMul <$> f t1 <*> f t2
  traverse f (ExpDiv t1 t2) = ExpDiv <$> f t1 <*> f t2
  traverse f (ExpPower t1 t2) = ExpPower <$> f t1 <*> pure t2
  traverse f (ExpAtan2 t1 t2) = ExpAtan2 <$> f t1 <*> f t2
  traverse f (ExpRectilinear t1 t2) = ExpRectilinear <$> f t1 <*> f t2
  traverse f (ExpFisheye t1 t2) = ExpFisheye <$> f t1 <*> f t2
  traverse f (ExpIfZero t1 t2 t3) = ExpIfZero <$> f t1 <*> f t2 <*> f t3
  traverse f (ExpVar i) = pure $ ExpVar i
  traverse f (ExpTuple ts) = ExpTuple <$> traverse f ts
  traverse _ _ = error "traverse"

newtype Mu a = Mu (a (Mu a))

instance Eq (Mu Expr) where
    Mu a == Mu b = a == b

instance Show (Mu Expr) where
  showsPrec d (Mu (ExpScalar n)) = shows n
  showsPrec d (Mu (ExpSin t)) = showParen (d > 10) $
      showString "sin " . showsPrec 11 t
  showsPrec d (Mu (ExpCos t)) = showParen (d > 10) $
      showString "cos " . showsPrec 11 t
  showsPrec d (Mu (ExpTan t)) = showParen (d > 10) $
      showString "tan " . showsPrec 11 t
  showsPrec d (Mu (ExpAsin t)) = showParen (d > 10) $
      showString "asin " . showsPrec 11 t
  showsPrec d (Mu (ExpAcos t)) = showParen (d > 10) $
      showString "acos " . showsPrec 11 t
  showsPrec d (Mu (ExpAtan t)) = showParen (d > 10) $
      showString "atan " . showsPrec 11 t
  showsPrec d (Mu (ExpSqrt t)) = showParen (d > 10) $
      showString "sqrt " . showsPrec 11 t
  showsPrec d (Mu (ExpAbs t)) = showParen (d > 10) $
      showString "abs " . showsPrec 11 t
  showsPrec d (Mu (ExpSignum t)) = showParen (d > 10) $
      showString "signum " . showsPrec 11 t
  showsPrec d (Mu (ExpAdd n1 n2)) = showParen (d > 6) $
      showsPrec 7 n1  .
      showString " + " .
      showsPrec 7 n2
  showsPrec d (Mu (ExpSub n1 n2)) = showParen (d > 6) $
      showsPrec 7 n1  .
      showString " - " .
      showsPrec 7 n2
  showsPrec d (Mu (ExpMul n1 n2)) = showParen (d > 7) $
      showsPrec 8 n1  .
      showString " * " .
      showsPrec 8 n2
  showsPrec d (Mu (ExpDiv n1 n2)) = showParen (d > 7) $
      showsPrec 8 n1  .
      showString " / " .
      showsPrec 8 n2
  showsPrec d (Mu (ExpPower n1 n2)) = showParen (d > 8) $
      showsPrec 9 n1  .
      showString " ^ " .
      showsPrec 9 n2
  showsPrec d (Mu (ExpAtan2 n1 n2)) = showParen (d > 10) $
      showString "atan2 " .
      showsPrec 11 n1  .
      showString " " .
      showsPrec 11 n2
  showsPrec d (Mu (ExpIfZero a b c)) = showParen (d > 10) $
      showString "ifZero " .
      showsPrec 11 a .
      showString " " .
      showsPrec 11 b .
      showString " " .
      showsPrec 11 c
  showsPrec d (Mu (ExpTuple as)) = showParen (d > 10) $
      showString "ifZero " .
      showsPrec 11 as
  showsPrec d (Mu (ExpRectilinear{})) = showString "Rectilinear"
  showsPrec d (Mu (ExpFisheye{})) = showString "Fisheye"


instance MuRef (Mu Expr) where
  type DeRef (Mu Expr) = Expr
  mapDeRef f (Mu e) = traverse f e

class Var a where
  mkVar :: VarGen a

data VarGen a = VarGen { runVarGen :: Int -> (a,Int) }

singletonVar :: (Int -> a) -> VarGen a
singletonVar f = VarGen $ \ i -> (f i , succ i)

instance Functor VarGen where
  fmap f g = pure f <*> g

instance Applicative VarGen where
  pure a = VarGen $ \ n -> (a,n)
  VarGen f <*> VarGen g = VarGen $ \ n -> case f n of
    (a,n') -> case g n' of
      (b,n'') -> (a b,n'')

class Body a where
  maxVar :: a -> Int

instance (Var a, Body b, MuRef b, DeRef b ~ Expr) =>
    MuRef (a -> b) where
  type DeRef (a -> b) = Expr
  mapDeRef f fn = ExpLambda ns <$> f r
    where
      (ns,r) = maxApp fn

maxApp :: (Body b, Var t) => (t -> b) -> ([Int], b)
maxApp fn = traceShow ("maxapp",n) ([n..n' - 1],r)
    where
      n      = maxVar r + 1
      (v,n') = runVarGen mkVar n
      r      = fn v

instance (Var a, Body b) => Body (a -> b) where
  maxVar = maximum . fst . maxApp

instance Body (Mu Expr) where
  maxVar (Mu e) = case e of
    ExpVar i      -> 0
--    ExpLambda i e -> i  -- This short-cut is vital to avoid
    other -> foldr (+) 0 $ fmap maxVar other

class Eval e where
  eval :: Expr e -> e

-- The universal type, a number or a structure.
data Value where
  Double :: Double -> Value
  Tuple  :: [Value] -> Value
  deriving Show

instance Eval Value where
  eval (ExpScalar n)       = Double n
  eval (ExpSin (Double n)) = Double $ P.sin n
  eval (ExpCos (Double n)) = Double $ P.cos n
  eval (ExpTan (Double n)) = Double $ P.tan n
  eval (ExpAsin (Double n)) = Double $ P.asin n
  eval (ExpAcos (Double n)) = Double $ P.acos n
  eval (ExpAtan (Double n)) = Double $ P.atan n
  eval (ExpAdd (Double a) (Double b)) = Double $ a + b
  eval (ExpSub (Double a) (Double b)) = Double $ a - b
  eval (ExpMul (Double a) (Double b)) = Double $ a * b
  eval (ExpDiv (Double a) (Double b)) = Double $ a / b
  eval (ExpPower (Double a) b) = Double $ a P.^ b
  eval (ExpAtan2 (Double a) (Double b)) = Double $ P.atan2 a b
  eval (ExpSqrt (Double n)) = Double $ sqrt n
  eval (ExpAbs (Double n)) = Double $ abs n
  eval (ExpSignum (Double n)) = Double $ signum n
  eval (ExpNeg (Double n)) = Double $ negate n
  eval (ExpTuple ns) = Tuple ns
  eval (ExpIfZero (Double z) a b)
    | nearZero z = a
    | otherwise  = b
  eval (ExpRectilinear a b) = Tuple [a,b]
  eval (ExpFisheye a b) = Tuple [a,b]
  eval other = error (show other)

evalMu :: Mu Expr -> Value
evalMu (Mu e) = eval $ fmap evalMu e

evalGraph :: Graph Expr -> Value
evalGraph (Graph xs u) = find u
  where
    -- use laziness to tie the bindings together
    es = [ (u,eval $ fmap find e) | (u,e) <- xs ]
    find :: Unique -> Value
    find n = fromMaybe (error (show n ++ " not found")) $
             lookup n es

instance (Var a, Var b) => Var (a,b) where
  mkVar = liftA2 (,) mkVar mkVar

------------------------------------------------------------------------------

reifyFunction :: ToExpr s => s -> IO ExprFunction
reifyFunction = reifyToExprFunction 0

-- Because functions only *ever* occur at the outermost level,
-- we handle them specially at entry.
class ToExpr s where
  reifyToExprFunction :: Int -> s -> IO ExprFunction

class ToMuExpr s where
  toMuExpr :: s -> Mu Expr

instance (Var a, ToExpr b) => ToExpr (a -> b) where
  reifyToExprFunction n f = do
    let (a,n') = runVarGen mkVar n
    let tmpVars = [n..n'-1]
    ExprFunction xs ys zs <- reifyToExprFunction n' (f a)
    let vars = map fst $ sortBy (compare `on` snd)
          [ (t,n)
          | (t,ExpVar n) <- ys, n `elem` tmpVars
          ]
--    print (vars,[n..n'-1])
    if length vars > length [n..n'-1]
    then error "too many vars"
    else do
      let scc = G.stronglyConnComp
            [ (n,n,foldr (:) [] e)
            | (n,e) <- ys
            , not (n `elem` vars) -- remove ExpVar's
            ]
      let find n = fromMaybe (error (show n ++ " not found")) $
                   lookup n ys
      let ys' = [ (t,find t) | G.AcyclicSCC t <- scc ]
      return $ ExprFunction (vars ++ xs) ys' zs

instance ToExpr (Mu Expr) where
  reifyToExprFunction n s = do
    Graph xs n <- reifyGraph s
    return $ ExprFunction [] [(V n,V <$> e) | (n,e) <- xs] $ V n

instance (ToMuExpr a, ToMuExpr b) => ToExpr (a,b) where
  reifyToExprFunction n (a,b) =
    reifyToExprFunction n $ Mu $ ExpTuple [toMuExpr a, toMuExpr b]

instance (ToMuExpr a, ToMuExpr b) => ToMuExpr (a,b) where
  toMuExpr (a,b) = Mu $ ExpTuple [toMuExpr a, toMuExpr b]

newtype V = V Int
  deriving (Eq, Ord)

instance Show V where
  show (V n) = "v" ++ show n

data ExprFunction =
  ExprFunction
    [V]           -- inputs, as a list of scalars
    [(V,Expr V)]  -- static assignments, in lexigraphical order
    V             -- result, might be a vector
--  deriving Show


instance Show ExprFunction where
  show (ExprFunction as xs r) = unlines $
      ["\\ " ++ show as ++ " -> in"] ++
      map showAssign xs ++
      ["in", "  " ++ show r]
    where
      showAssign (v,e) = "  " ++ show v ++ " = " ++ show e

nearZero :: Double -> Bool
nearZero n = abs n < 1e-6

num_pi :: Double
num_pi = 3.141592653589793
------------------------------------------------------------------------------

newtype JavaScript = JavaScript ExprFunction

instance Show JavaScript where
  show (JavaScript (ExprFunction as xs r)) = unlines $
      ["((" ++ show as ++ ") => {"] ++
      map showAssign xs ++
      [ "  return " ++ show r ++ ";"
      , "})"
      ]
    where
      showAssign (v,ExpScalar e) = "  let " ++ show v ++ " = " ++ show e ++ ";"
      showAssign (v,ExpSin e) = "  let " ++ show v ++ " = Math.sin(" ++ show e ++ ");"
      showAssign (v,ExpIfZero i t e) = "  let " ++ show v ++
                 " = " ++ show i ++ "?" ++ show t ++ ":" ++ show e ++ ";"
      showAssign (v,e) = "  // let " ++ show v ++ " = " ++ show e ++ ";"
