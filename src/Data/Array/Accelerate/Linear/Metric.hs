{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Accelerate.Linear.Metric
-- Copyright   : 2014 Edward Kmett, Charles Durham,
--               2015 Trevor L. McDonell
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

import Data.Array.Accelerate
import qualified Linear.Metric                  as L

-- | Free and sparse inner product/metric spaces.
--
class L.Metric f => Metric f where

  -- | Compute the inner product of two vectors or (equivalently) convert a
  -- vector @f a@ into a covector @f a -> a@.
  --
  -- >>> V2 1 2 `dot` V2 3 4
  -- 11
  --
  dot :: forall a. (Elt a, IsNum a, IsMetric f a)
      => Exp (f a)
      -> Exp (f a)
      -> Exp a
  dot = lift2 (L.dot :: f (Exp a) -> f (Exp a) -> Exp a)

  -- | Compute the squared norm. The name quadrance arises from Norman J.
  -- Wildberger's rational trigonometry.
  --
  quadrance
      :: forall a. (Elt a, IsNum a, IsMetric f a)
      => Exp (f a)
      -> Exp a
  quadrance = lift1 (L.quadrance :: f (Exp a) -> Exp a)

  -- | Compute the 'quadrance' of the difference
  --
  qd :: forall a. (Elt a, IsNum a, IsMetric f a)
     => Exp (f a)
     -> Exp (f a)
     -> Exp a
  qd = lift2 (L.qd :: f (Exp a) -> f (Exp a) -> Exp a)

  -- | Compute the distance between two vectors in a metric space
  --
  distance
      :: forall a. (Elt a, IsFloating a, IsMetric f a)
      => Exp (f a)
      -> Exp (f a)
      -> Exp a
  distance = lift2 (L.distance :: f (Exp a) -> f (Exp a) -> Exp a)

  -- | Compute the norm of a vector in a metric space
  --
  norm :: forall a. (Elt a, IsFloating a, IsMetric f a)
       => Exp (f a)
       -> Exp a
  norm = lift1 (L.norm :: f (Exp a) -> Exp a)

  -- | Convert a non-zero vector to unit vector.
  --
  signorm
      :: forall a. (Elt a, IsFloating a, IsMetric f a)
      => Exp (f a)
      -> Exp (f a)
  signorm = lift1 (L.signorm :: f (Exp a) -> f (Exp a))


-- This type is awful, so at least make some shorthand for it. ugh.
--
type IsMetric f a = (Metric f, Unlift Exp (f (Exp a)), Plain (f (Exp a)) ~ f a)


-- | Normalize a 'Metric' functor to have unit 'norm'. This function does not
-- change the functor if its 'norm' is 0 or 1.
--
-- normalize :: (Floating a, Metric f, Epsilon a) => f a -> f a
-- normalize v = if nearZero l || nearZero (1-l) then v else fmap (/sqrt l) v
--   where l = quadrance v

-- | @project u v@ computes the projection of @v@ onto @u@.
--
project :: forall v a. (Elt a, IsFloating a, IsMetric v a) => Exp (v a) -> Exp (v a) -> Exp (v a)
project = lift2 (L.project :: v (Exp a) -> v (Exp a) -> v (Exp a))

