{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Accelerate.Linear.Projection
-- Copyright   : [2019] Trevor L. McDonell
-- License     : BSD-style (see the file LICENSE)
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Common projection matrices; for example perspective and orthographic
-- transformation matrices.
--
-- @since 0.7.0.0
----------------------------------------------------------------------------

module Data.Array.Accelerate.Linear.Projection
  where

import Data.Array.Accelerate
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Linear.Epsilon
import Data.Array.Accelerate.Linear.Matrix
import Data.Array.Accelerate.Linear.Metric
import Data.Array.Accelerate.Linear.V3
import Data.Array.Accelerate.Linear.V4

import Control.Lens
import qualified Linear.Projection                        as L


-- | Build a look at view matrix
--
lookAt
    :: (Epsilon a, Floating a)
    => Exp (V3 a)     -- ^ Eye
    -> Exp (V3 a)     -- ^ Center
    -> Exp (V3 a)     -- ^ Up
    -> Exp (M44 a)
lookAt eye center up = lift $
  V4 (V4 (xa^._x)  (xa^._y)  (xa^._z)  xd)
     (V4 (ya^._x)  (ya^._y)  (ya^._z)  yd)
     (V4 (-za^._x) (-za^._y) (-za^._z) zd)
     (V4 0         0         0          1)
  where za = normalize $ center - eye
        xa = normalize $ cross za up
        ya = cross xa za
        xd = -dot xa eye
        yd = -dot ya eye
        zd = dot za eye


-- | Build a matrix for a symmetric perspective-view frustum
--
perspective
  :: Floating a
  => Exp a -- ^ FOV
  -> Exp a -- ^ Aspect ratio
  -> Exp a -- ^ Near plane
  -> Exp a -- ^ Far plane
  -> Exp (M44 a)
perspective = lift $$$$ L.perspective


-- | Build a matrix for a symmetric perspective-view frustum with a far
-- plane at infinite
--
infinitePerspective
  :: Floating a
  => Exp a -- ^ FOV
  -> Exp a -- ^ Aspect Ratio
  -> Exp a -- ^ Near plane
  -> Exp (M44 a)
infinitePerspective = lift $$$ L.infinitePerspective


-- | Build an orthographic perspective matrix from 6 clipping planes
--
ortho
  :: Floating a
  => Exp a -- ^ Left
  -> Exp a -- ^ Right
  -> Exp a -- ^ Bottom
  -> Exp a -- ^ Top
  -> Exp a -- ^ Near
  -> Exp a -- ^ Far
  -> Exp (M44 a)
ortho = lift $$$$$$ L.ortho

infixr 0 $$$$$$
($$$$$$) :: (b -> a) -> (c -> d -> e -> f -> g -> h -> b) -> c -> d -> e -> f -> g -> h -> a
(f $$$$$$ g) x y z u v w = f (g x y z u v w)

