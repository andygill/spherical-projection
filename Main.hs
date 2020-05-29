{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Data.Reify

import Prelude hiding (sin, cos, atan2, (^))
import qualified Prelude as P

class Eval e where
  eval :: () -> e -> Number

class Conditional e where
  ifZero :: Number -> e -> e -> e

data Expr :: * -> * where
  ExpNumber :: Double -> Expr t
  ExpCos    :: t -> Expr t
  ExpSin    :: t -> Expr t
  ExpSqrt   :: t -> Expr t
  ExpAdd    :: t -> t -> Expr t
  ExpSub    :: t -> t -> Expr t
  ExpMul    :: t -> t -> Expr t
  ExpDiv    :: t -> t -> Expr t
  ExpPower  :: t -> t -> Expr t
  ExpAtan2  :: t -> t -> Expr t  
  ExpIfZero :: t -> t -> t -> Expr t    

deriving instance Show t => Show (Expr t)

instance Functor Expr where
  fmap f (ExpNumber d) = ExpNumber d
  fmap f g = error "fmap"
instance Foldable Expr where
  foldr f z (ExpNumber d) = z
  foldr f z _ = error "foldr"
instance Traversable Expr where
  traverse f (ExpNumber d) = pure $ ExpNumber d
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
    
newtype Longitude = Longitude Radian
  deriving (Show)

instance Num Longitude where
  Longitude a + Longitude b = Longitude (a + b)
  Longitude a - Longitude b = Longitude (a - b)

  fromInteger = Longitude . fromInteger 

instance Fractional Longitude where
  fromRational = Longitude . fromRational

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

--ifZero :: Number -> Number -> Number -> Number    
--ifZero = undefined

------------------------------------------------------------------------------
{-
newtype Formula a = Formula a

instance Show (Formula Number) where
  showsPrec d (Formula n) = case n of
    Number d -> shows d
    Sin n    -> showParen (d > 10) $
      showString "sin " . showsPrec 11 (Formula n)
    Cos n    -> showParen (d > 10) $
      showString "cos " . showsPrec 11 (Formula n)
    Sqrt n    -> showParen (d > 10) $
     showString "sqrt " . showsPrec 11 (Formula n)
    NumAdd r1 r2 -> showParen (d > 6) $
      showsPrec 7 (Formula r1) .
      showString " + " .
      showsPrec 7 (Formula r2)
    NumSub r1 r2 -> showParen (d > 6) $
      showsPrec 7 (Formula r1) .
      showString " - " .
      showsPrec 7 (Formula r2)
    NumMul r1 r2 -> showParen (d > 7) $
      showsPrec 8 (Formula r1) .
      showString " * " .
      showsPrec 8 (Formula r2)
    NumDiv r1 r2 -> showParen (d > 7) $
      showsPrec 8 (Formula r1) .
      showString " / " .
      showsPrec 8 (Formula r2)
    NumIfZero n1 n2 n3 -> showParen (d > 10) $
      showString "ifZero " .
      showsPrec 11 (Formula n1) .
      showString " " .
      showsPrec 11 (Formula n2) .
      showString " " .
      showsPrec 11 (Formula n3)
      
    other -> error $ show other

instance Show (Formula Radian) where
  showsPrec d (Formula r) = case r of
    Radian d -> shows d
    Atan2 n1 n2 -> showParen (d > 10) $
      showString "atan " .
      showsPrec 11 (Formula n1) .
      showString " " .
      showsPrec 11 (Formula n2)
    RadAdd r1 r2 -> showParen (d > 6) $
      showsPrec 7 (Formula r1) .
      showString " + " .
      showsPrec 7 (Formula r2)
    RadSub r1 r2 -> showParen (d > 6) $
      showsPrec 7 (Formula r1) .
      showString " - " .
      showsPrec 7 (Formula r2)

instance Show (Formula Rectilinear) where
  showsPrec d (Formula (Rectilinear n1 n2)) =
    showParen (d > 10) $
      showString "Rectlinear " .
      showsPrec 11 (Formula n1) .
      showString " " .
      showsPrec 11 (Formula n2)

-}

------------------------------------------------------------------------------

instance MuRef Scalar where
  type DeRef Scalar = Expr
  mapDeRef f (Scalar s) =  mapDeRef f s

instance MuRef (Mu Expr) where
  type DeRef (Mu Expr) = Expr
  mapDeRef f (Mu e) = traverse f e --  pure (ExpNumber d)  
--  mapDeRef f (Mu (ExpNumber d)) = traverse f e --  pure (ExpNumber d)
