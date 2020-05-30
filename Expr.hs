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

import Prelude hiding (sin, cos, atan2, (^))
import qualified Prelude as P

data Expr :: * -> * where
  ExpScalar :: Double -> Expr t
  ExpSin    :: t -> Expr t
  ExpCos    :: t -> Expr t
  ExpSqrt   :: t -> Expr t
  ExpAdd    :: t -> t -> Expr t
  ExpSub    :: t -> t -> Expr t
  ExpMul    :: t -> t -> Expr t
  ExpDiv    :: t -> t -> Expr t
  ExpPower  :: t -> t -> Expr t
  ExpAtan2  :: t -> t -> Expr t
  ExpLambda :: [Int] -> t -> Expr t
  ExpVar    :: Int -> Expr t
  ExpRectilinear :: t -> t -> Expr t
  ExpIfZero :: t -> t -> t -> Expr t    

deriving instance Show t => Show (Expr t)

instance Functor Expr where
  fmap f (ExpScalar d) = ExpScalar d
  fmap f (ExpSin t1) = ExpSin (f t1)
  fmap f (ExpCos t1) = ExpCos (f t1)
  fmap f (ExpSqrt t1) = ExpSqrt (f t1)
  fmap f (ExpAdd t1 t2) = ExpAdd (f t1) (f t2)
  fmap f (ExpSub t1 t2) = ExpSub (f t1) (f t2)
  fmap f (ExpMul t1 t2) = ExpMul (f t1) (f t2)
  fmap f (ExpDiv t1 t2) = ExpDiv (f t1) (f t2)
  fmap f (ExpPower t1 t2) = ExpPower (f t1) (f t2)
  fmap f (ExpAtan2 t1 t2) = ExpAtan2 (f t1) (f t2)
  fmap f (ExpLambda vs t) = ExpLambda vs (f t)
  fmap f (ExpVar i) = ExpVar i
  fmap f (ExpRectilinear t1 t2) = ExpRectilinear (f t1) (f t2)
  fmap f (ExpIfZero t1 t2 t3) = ExpIfZero (f t1) (f t2) (f t3)
  fmap f g = error $ show ("fmap" )
instance Foldable Expr where
  foldr f z (ExpScalar d) = z
  foldr f z (ExpSin t1) = f t1 z
  foldr f z (ExpCos t1) = f t1 z
  foldr f z (ExpSqrt t1) = f t1 z
  foldr f z (ExpAdd t1 t2) = f t1 (f t2 z)
  foldr f z (ExpSub t1 t2) = f t1 (f t2 z)
  foldr f z (ExpMul t1 t2) = f t1 (f t2 z)
  foldr f z (ExpDiv t1 t2) = f t1 (f t2 z)
  foldr f z (ExpPower t1 t2) = f t1 (f t2 z)
  foldr f z (ExpAtan2 t1 t2) = f t1 (f t2 z)
  foldr f z (ExpIfZero t1 t2 t3) = f t1 (f t2 (f t3 z))
  foldr f z _ = error "foldr"
instance Traversable Expr where
  traverse f (ExpScalar d) = pure $ ExpScalar d
  traverse f (ExpSin t1) = ExpSin <$> f t1
  traverse f (ExpCos t1) = ExpCos <$> f t1
  traverse f (ExpSqrt t1) = ExpSqrt <$> f t1
  traverse f (ExpAdd t1 t2) = ExpAdd <$> f t1 <*> f t2
  traverse f (ExpSub t1 t2) = ExpSub <$> f t1 <*> f t2
  traverse f (ExpMul t1 t2) = ExpMul <$> f t1 <*> f t2
  traverse f (ExpDiv t1 t2) = ExpDiv <$> f t1 <*> f t2
  traverse f (ExpPower t1 t2) = ExpPower <$> f t1 <*> f t2
  traverse f (ExpAtan2 t1 t2) = ExpAtan2 <$> f t1 <*> f t2
  traverse f (ExpRectilinear t1 t2) = ExpRectilinear <$> f t1 <*> f t2
  traverse f (ExpIfZero t1 t2 t3) = ExpIfZero <$> f t1 <*> f t2 <*> f t3
  traverse f (ExpVar i) = pure $ ExpVar i
  traverse _ _ = error "traverse"

newtype Mu a = Mu (a (Mu a))

instance Show (Mu Expr) where
  showsPrec d (Mu (ExpScalar n)) = shows n
  showsPrec d (Mu (ExpSin t)) = showParen (d > 10) $
      showString "sin " . showsPrec 11 t
  showsPrec d (Mu (ExpCos t)) = showParen (d > 10) $
      showString "cos " . showsPrec 11 t
  showsPrec d (Mu (ExpSqrt t)) = showParen (d > 10) $
      showString "sqrt " . showsPrec 11 t
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
  showsPrec d (Mu (ExpIfZero a b c)) = showParen (d > 10) $
      showString "ifZero " .
      showsPrec 11 a .
      showString " " .
      showsPrec 11 b .
      showString " " .
      showsPrec 11 c

instance MuRef (Mu Expr) where
  type DeRef (Mu Expr) = Expr
  mapDeRef f (Mu e) = traverse f e 

class Var a where
  mkVar :: Int -> (a,[Int])
  mkVar' :: VarGen a

data VarGen a = VarGen { runVarGen :: Int -> (a,Int) }

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
      (v,n') = runVarGen mkVar' n
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
  eval (ExpAdd (Double a) (Double b)) = Double $ a + b
  eval (ExpIfZero (Double z) a b)
    | z == 0    = a
    | otherwise = b
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
  mkVar n = ((v1,v2),ns1 ++ ns2)
    where
      (v1,ns1) = mkVar n
      (v2,ns2) = mkVar (maximum ns1 + 1)

  mkVar' = liftA2 (,) mkVar' mkVar'
