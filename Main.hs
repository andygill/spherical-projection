{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Data.Dynamic
import Data.Reify
import Debug.Trace

import Prelude hiding (sin, cos, atan2, (^))
import qualified Prelude as P

--class Eval e where
--  eval :: () -> e -> Number

class Conditional e where
  ifZero :: Number -> e -> e -> e

data Expr :: * -> * where
  ExpNumber :: Double -> Expr t
  ExpSin    :: t -> Expr t
  ExpCos    :: t -> Expr t
  ExpSqrt   :: t -> Expr t
  ExpAdd    :: t -> t -> Expr t
  ExpSub    :: t -> t -> Expr t
  ExpMul    :: t -> t -> Expr t
  ExpDiv    :: t -> t -> Expr t
  ExpPower  :: t -> t -> Expr t
  ExpAtan2  :: t -> t -> Expr t
  ExpLambda :: Int -> t -> Expr t
  ExpVar    :: Int -> Expr t
  ExpLambda' :: t -> t -> Expr t
  ExpVar'   :: Dynamic -> Expr t
  ExpRectilinear :: t -> t -> Expr t
  ExpIfZero :: t -> t -> t -> Expr t    

deriving instance Show t => Show (Expr t)

instance Functor Expr where
  fmap f (ExpNumber d) = ExpNumber d
  fmap f (ExpSin t1) = ExpSin (f t1)
  fmap f (ExpCos t1) = ExpCos (f t1)
  fmap f (ExpSqrt t1) = ExpSqrt (f t1)
  fmap f (ExpAdd t1 t2) = ExpAdd (f t1) (f t2)
  fmap f (ExpSub t1 t2) = ExpSub (f t1) (f t2)
  fmap f (ExpMul t1 t2) = ExpMul (f t1) (f t2)
  fmap f (ExpDiv t1 t2) = ExpDiv (f t1) (f t2)
  fmap f (ExpPower t1 t2) = ExpPower (f t1) (f t2)
  fmap f (ExpAtan2 t1 t2) = ExpAtan2 (f t1) (f t2)
  fmap f g = error "fmap"
instance Foldable Expr where
  foldr f z (ExpNumber d) = z
  foldr f z (ExpSin t1) = f t1 z
  foldr f z (ExpCos t1) = f t1 z
  foldr f z (ExpSqrt t1) = f t1 z
  foldr f z (ExpAdd t1 t2) = f t1 (f t2 z)
  foldr f z (ExpSub t1 t2) = f t1 (f t2 z)
  foldr f z (ExpMul t1 t2) = f t1 (f t2 z)
  foldr f z (ExpDiv t1 t2) = f t1 (f t2 z)
  foldr f z (ExpPower t1 t2) = f t1 (f t2 z)
  foldr f z (ExpAtan2 t1 t2) = f t1 (f t2 z)
  foldr f z _ = error "foldr"
instance Traversable Expr where
  traverse f (ExpNumber d) = pure $ ExpNumber d
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
  showsPrec d (Mu (ExpNumber n)) = shows n
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

class Eval e where
  eval :: Expr e -> e

data U = Double Double
       | Tuple U U

instance Eval U where
  eval (ExpNumber n) = Double n
  eval (ExpSin (Double n)) = Double (P.sin n)
  eval (ExpCos (Double n)) = Double (P.cos n)
  eval (ExpIfZero (Double z) a b)
    | z == 0    = a
    | otherwise = b
      
newtype Scalar where
  Scalar :: Mu Expr -> Scalar

instance Show Scalar where
  showsPrec d (Scalar e) = showsPrec d e

instance Num Scalar where
  Scalar a + Scalar b = Scalar (Mu $ ExpAdd a b)
  Scalar a - Scalar b = Scalar (Mu $ ExpSub a b)
  Scalar a * Scalar b = Scalar (Mu $ ExpMul a b)
  fromInteger = Scalar . Mu . ExpNumber . fromInteger
  
instance Fractional Scalar where
  Scalar a / Scalar b = Scalar (Mu $ ExpDiv a b)
  fromRational = Scalar . Mu . ExpNumber . fromRational  

instance Floating Number where
  sqrt (Scalar n) = Scalar (Mu $ ExpSqrt n)

instance Conditional Number where
  ifZero (Scalar a) (Scalar b) (Scalar c) = Scalar $ Mu $ ExpIfZero a b c

infixr 8 ^

(^) :: Scalar -> Scalar -> Scalar
Scalar a ^ Scalar b = Scalar (Mu $ ExpPower a b)

type Number = Scalar

{-
-- Sometimes, they are just floating point values
-- Perhaps use Scalar?
data Number where
  Number :: Double -> Number
  Cos    :: Radian -> Number
  Sin    :: Radian -> Number
  NumAdd :: Number -> Number -> Number
  NumSub :: Number -> Number -> Number
  NumMul :: Number -> Number -> Number
  NumDiv :: Number -> Number -> Number
  Sqrt   :: Number -> Number
  Power  :: Number -> Int -> Number
  NumIfZero
         :: Number -> Number -> Number -> Number
  deriving (Eq, Show)

instance Num Number where
  Number a + Number b = Number (a + b)
  a + b = NumAdd a b

  Number a - Number b = Number (a - b)  
  a - b = NumSub a b
  
  Number a * Number b = Number (a * b)
  a * b = NumMul a b

  fromInteger = Number . fromInteger

instance Fractional Number where
  (/) = NumDiv
  fromRational = Number  . fromRational
  
instance Floating Number where
  sqrt (Number n) = Number (sqrt n)
  sqrt n          = Sqrt n

instance Eval Number where
  eval _   (Number n) = Number n
  eval env (Cos r) = case eval env r of
    Number n -> Number (P.cos n)

instance Conditional Number where
  ifZero = NumIfZero
-}

-- Expressions can be radians
newtype Radian where
  Radian :: Mu Expr -> Radian
{-
data Radian where
  Radian :: Double -> Radian
  Atan2 :: Number -> Number -> Radian
  RadAdd :: Radian -> Radian -> Radian
  RadSub :: Radian -> Radian -> Radian
  deriving (Show)
-}
instance Show Radian where
  showsPrec d (Radian e) = showsPrec d e
  
instance Num Radian where
  Radian r1 + Radian r2 = Radian $ Mu $ ExpAdd r1 r2
  Radian r1 - Radian r2 = Radian $ Mu $ ExpSub r1 r2
  fromInteger = Radian . Mu . ExpNumber . fromInteger

instance Fractional Radian where
  fromRational = Radian . Mu . ExpNumber . fromRational

{-
instance Eval Radian where
  eval _   (Radian n) = Number n
  eval env (Atan2 y x) = case (eval env y, eval env x) of
    (Number n, Number m) -> Number (P.atan2 n m)
-}

class Math radian where
  sin :: radian -> Number
  cos :: radian -> Number
  atan2 :: Number -> Number -> radian

instance Math Radian where
  sin (Radian r) = Scalar $ Mu $ ExpSin r
  cos (Radian r) = Scalar $ Mu $ ExpCos r
  atan2 (Scalar y) (Scalar x) = Radian $ Mu $ ExpAtan2 y x
    
-- Sometimes called elevation
newtype Longitude = Longitude Radian
  deriving (Show)

instance Num Longitude where
  Longitude a + Longitude b = Longitude (a + b)
  Longitude a - Longitude b = Longitude (a - b)

  fromInteger = Longitude . fromInteger 

instance Fractional Longitude where
  fromRational = Longitude . fromRational

-- Sometimes called azimuth
newtype Latitude = Latitude Radian
  deriving (Show)

instance Num Latitude where
  fromInteger = Latitude . fromInteger 

instance Fractional Latitude where
  fromRational = Latitude . fromRational

instance Math Longitude where
  sin (Longitude a) = sin a
  cos (Longitude a) = cos a
  atan2 y x = Longitude $ atan2 y x

instance Math Latitude where
  sin (Latitude a) = sin a
  cos (Latitude a) = cos a  
  atan2 y x = Latitude $ atan2 y x

-- A Coordinate on a sphere
-- 0,0 is looking straight ahead
-- Like rotating in Y, X, then Z?
data Coord = Coord Latitude Longitude

{-
sin :: Radian -> Number
sin = error ""

cos :: Radian -> Number
cos = error ""
-}
-- A Point on a unit sphere
type Point = (Number, Number, Number)

longLatToPoint :: (Longitude, Latitude) -> Point
longLatToPoint (long,lat) =
  ( cos lat * cos long
  , cos lat * sin long
  , sin lat
  )

pointToLatLong :: Point -> (Longitude, Latitude)
pointToLatLong (x,y,z)  = (long,lat)
  where
    long = atan2 y x
    lat  = atan2 z (sqrt (x^2 + y^2))

{-
-- add together two direction.
addDirection :: (Latitude,Longitude)
	     -> (Latitude,Longitude)
	     -> (Latitude,Longitude)
addDirection (lat0,long0) (lat1,long1) = (lat,long)
  where
    lat  = 
    long = 
-}
main = print ()

------------------------------------------------------------------------------

-- Rectilinear projection

data Rectilinear = Rectilinear Number Number
  deriving Show

instance MuRef Rectilinear where
  type DeRef Rectilinear = Expr
  mapDeRef f (Rectilinear (Scalar x) (Scalar y)) = mapDeRef f (Mu $ ExpRectilinear x y)

-- These are from https://mathworld.wolfram.com/GnomonicProjection.html
fromRecilinear :: (Latitude, Longitude) -> Rectilinear -> (Latitude, Longitude)
fromRecilinear = undefined

toRecilinear :: (Latitude, Longitude) -> (Latitude, Longitude) -> Rectilinear
toRecilinear (phi_1,lam_0) (phi,lam) = Rectilinear x y
  where
    cos_c = sin(phi_1) * sin(phi) + cos(phi_1) * cos(phi) * cos (lam - lam_0);
    x = ifZero cos_c 0 $ cos(phi) * sin (lam - lam_0) / cos_c
    y = ifZero cos_c 0 $
          (cos(phi_1) * sin(phi) - sin(phi_1) * cos(phi) * cos (lam - lam_0)) / cos_c

------------------------------------------------------------------------------

instance MuRef Scalar where
  type DeRef Scalar = Expr
  mapDeRef f (Scalar s) = mapDeRef f s

instance MuRef (Mu Expr) where
  type DeRef (Mu Expr) = Expr
  mapDeRef f (Mu e) = traverse f e 

class Var a where
  mkVar :: Int -> a
  mkVar' :: Typeable b => (a -> b) -> a

class Body a where
  maxVar :: a -> Int

instance Var Scalar where
  mkVar = Scalar . Mu . ExpVar
  mkVar' = Scalar . Mu . ExpVar' . toDyn

instance (Var a, Body b, MuRef b, DeRef b ~ Expr)  => MuRef (a -> b) where
  type DeRef (a -> b) = Expr
  mapDeRef f fn = ExpLambda n <$> f r
    where
      (n,r) = maxApp fn

instance Body Scalar where
  maxVar (Scalar e) = maxVar e

maxApp :: (Body b, Var t) => (t -> b) -> (Int, b)
maxApp fn = traceShow ("maxapp",n) (n,r)
    where
      n = maxVar r + 1
      r = fn (mkVar n)

instance (Var a, Body b) => Body (a -> b) where
  maxVar = fst . maxApp 

instance Body (Mu Expr) where
  maxVar (Mu e) = case e of
    ExpVar i      -> 0
    ExpLambda i e -> i  -- This short-cut is vital to avoid 
    other -> foldr (+) 0 $ fmap maxVar other

{-
instance MuRef b => MuRef (a -> b) where
  type DeRef (a -> b) = Expr
  mapDeRef f fn = f (fn (error "arg"))
-}

