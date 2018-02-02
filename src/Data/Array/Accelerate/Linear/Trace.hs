{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Accelerate.Linear.Trace
-- Copyright   : 2014 Edward Kmett
--               [2015..2018] Trevor L. McDonell
-- License     : BSD-style (see the file LICENSE)
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Simple matrix operations for low-dimensional primitives
----------------------------------------------------------------------------

module Data.Array.Accelerate.Linear.Trace (

  Trace(..),

) where

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Data.Complex

import Data.Array.Accelerate.Linear.Lift
import Data.Array.Accelerate.Linear.Plucker
import Data.Array.Accelerate.Linear.Quaternion
import Data.Array.Accelerate.Linear.Type
import Data.Array.Accelerate.Linear.V0
import Data.Array.Accelerate.Linear.V1
import Data.Array.Accelerate.Linear.V2
import Data.Array.Accelerate.Linear.V3
import Data.Array.Accelerate.Linear.V4

import qualified Linear.Trace                                       as L


class L.Trace m => Trace m where
  -- | Compute the trace of a matrix
  trace :: (A.Num a, Box2 m m a) => Exp (m (m a)) -> Exp a
  trace = lift . L.trace . unlift'

  -- | Compute the diagonal of a matrix
  diagonal :: Box2 m m a => Exp (m (m a)) -> Exp (m a)
  diagonal = lift . L.diagonal . unlift'

instance Trace Complex
instance Trace V0
instance Trace V1
instance Trace V2
instance Trace V3
instance Trace V4
instance Trace Plucker
instance Trace Quaternion

