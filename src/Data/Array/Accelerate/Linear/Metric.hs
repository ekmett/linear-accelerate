{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Accelerate.Linear.Metric
-- Copyright   : 2014 Edward Kmett, Charles Durham,
--               [2015..2018] Trevor L. McDonell
-- License     : BSD-style (see the file LICENSE)
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Free metric spaces
----------------------------------------------------------------------------

module Data.Array.Accelerate.Linear.Metric
  where

import Data.Array.Accelerate                    as A hiding ( pattern V2 )
import Data.Array.Accelerate.Linear.Type
import Data.Array.Accelerate.Linear.Epsilon
import Data.Array.Accelerate.Linear.Vector

import qualified Linear.Metric                  as L

-- $setup
-- >>> :set -XPatternSynonyms
-- >>> import Data.Array.Accelerate.Linear.V2 ( pattern V2_ )
-- >>> import Linear.V2

-- | Free and sparse inner product/metric spaces.
--
class L.Metric f => Metric f where

  -- | Compute the inner product of two vectors or (equivalently) convert a
  -- vector @f a@ into a covector @f a -> a@.
  --
  -- >>> (V2_ 1 2 :: Exp (V2 Int)) `dot` (V2_ 3 4 :: Exp (V2 Int))
  -- 11
  --
  dot :: forall a. (A.Num a, Box f a)
      => Exp (f a)
      -> Exp (f a)
      -> Exp a
  dot = lift2 (L.dot :: f (Exp a) -> f (Exp a) -> Exp a)

  -- | Compute the squared norm. The name quadrance arises from Norman J.
  -- Wildberger's rational trigonometry.
  --
  quadrance
      :: forall a. (A.Num a, Box f a)
      => Exp (f a)
      -> Exp a
  quadrance = lift1 (L.quadrance :: f (Exp a) -> Exp a)

  -- | Compute the 'quadrance' of the difference
  --
  qd :: forall a. (A.Num a, Box f a)
     => Exp (f a)
     -> Exp (f a)
     -> Exp a
  qd = lift2 (L.qd :: f (Exp a) -> f (Exp a) -> Exp a)

  -- | Compute the distance between two vectors in a metric space
  --
  distance
      :: forall a. (A.Floating a, Box f a)
      => Exp (f a)
      -> Exp (f a)
      -> Exp a
  distance = lift2 (L.distance :: f (Exp a) -> f (Exp a) -> Exp a)

  -- | Compute the norm of a vector in a metric space
  --
  norm :: forall a. (A.Floating a, Box f a)
       => Exp (f a)
       -> Exp a
  norm = lift1 (L.norm :: f (Exp a) -> Exp a)

  -- | Convert a non-zero vector to unit vector.
  --
  signorm
      :: forall a. (A.Floating a, Box f a)
      => Exp (f a)
      -> Exp (f a)
  signorm = lift1 (L.signorm :: f (Exp a) -> f (Exp a))


type IsMetric f a = (Metric f, Box f a)


-- | Normalize a 'Metric' functor to have unit 'norm'. This function does not
-- change the functor if its 'norm' is 0 or 1.
--
normalize
    :: (Elt (f a), A.Floating a, IsMetric f a, Epsilon a)
    => Exp (f a)
    -> Exp (f a)
normalize v
  = nearZero l || nearZero (1-l)
  ? ( v, v ^/ sqrt l )
  where
    l = quadrance v

-- | @project u v@ computes the projection of @v@ onto @u@.
--
project
    :: forall f a. (A.Floating a, IsMetric f a)
    => Exp (f a)
    -> Exp (f a)
    -> Exp (f a)
project = lift2 (L.project :: f (Exp a) -> f (Exp a) -> f (Exp a))

