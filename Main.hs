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

import Expr
import Types
import qualified Data.Graph as G

main = print ()

