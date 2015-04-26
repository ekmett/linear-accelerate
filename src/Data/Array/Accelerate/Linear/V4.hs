{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Accelerate.Linear.V4
-- Copyright   : 2014 Edward Kmett, Charles Durham,
--               2015 Trevor L. McDonell
-- License     : BSD-style (see the file LICENSE)
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- 4-D Vectors
----------------------------------------------------------------------------

module Data.Array.Accelerate.Linear.V4 (

  V4(..), vector, point, normalizePoint,

) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Array.Sugar

import Data.Array.Accelerate.Linear.Metric
import Data.Array.Accelerate.Linear.Vector
import Data.Array.Accelerate.Linear.V3

import Linear.V4                                ( V4(..) )
import qualified Linear.V4                      as L


-- | Convert a 3-dimensional affine vector into a 4-dimensional homogeneous
-- vector.
--
vector :: (Elt a, IsNum a) => Exp (V3 a) -> Exp (V4 a)
vector = lift1 L.vector

-- | Convert a 3-dimensional affine point into a 4-dimensional homogeneous
-- vector.
--
point :: (Elt a, IsNum a) => Exp (V3 a) -> Exp (V4 a)
point = lift1 L.point

-- | Convert 4-dimensional projective coordinates to a 3-dimensional point. This
-- operation may be denoted, @euclidean [x:y:z:w] = (x\/w, y\/w, z\/w)@ where
-- the projective, homogenous, coordinate @[x:y:z:w]@ is one of many associated
-- with a single point @(x\/w, y\/w, z\/w)@.
--
normalizePoint :: (Elt a, IsFloating a) => Exp (V4 a) -> Exp (V3 a)
normalizePoint = lift1 L.normalizePoint


-- Instances
-- ---------

instance Metric V4
instance Additive V4

type instance EltRepr (V4 a) = EltRepr (a, a, a, a)

instance Elt a => Elt (V4 a) where
  eltType _ = eltType (undefined :: (a,a,a,a))
  toElt p = case toElt p of
     (x, y, z, w) -> V4 x y z w
  fromElt (V4 x y z w) = fromElt (x, y, z, w)

instance cst a => IsProduct cst (V4 a) where
  type ProdRepr (V4 a) = ProdRepr (a,a,a,a)
  fromProd p (V4 x y z w) = fromProd p (x,y,z,w)
  toProd p t = case toProd p t of
     (x, y, z, w) -> V4 x y z w
  prod p _ = prod p (undefined :: (a,a,a,a))

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (V4 a) where
  type Plain (V4 a) = V4 (Plain a)
  --  lift = Exp . Tuple . F.foldl SnocTup NilTup
  lift (V4 x y z w) =
    Exp $ Tuple $ NilTup `SnocTup` lift x
                         `SnocTup` lift y
                         `SnocTup` lift z
                         `SnocTup` lift w

instance (Elt a, e ~ Exp a) => Unlift Exp (V4 e) where
  unlift t = V4 (Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` t)
                (Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t)
                (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                (Exp $ ZeroTupIdx `Prj` t)

