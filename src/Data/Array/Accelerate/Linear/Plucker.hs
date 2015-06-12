{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Accelerate.Linear.Plucker
-- Copyright   : 2014 Edward Kmett, Charles Durham,
--               2015 Trevor L. McDonell
-- License     : BSD-style (see the file LICENSE)
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Plücker coordinates for lines in 3d homogeneous space.
----------------------------------------------------------------------------

module Data.Array.Accelerate.Linear.Plucker (

  Plucker(..), squaredError, (><), plucker, plucker3D,

) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Array.Sugar

import Data.Array.Accelerate.Linear.Lift
import Data.Array.Accelerate.Linear.Metric
import Data.Array.Accelerate.Linear.Vector
import Data.Array.Accelerate.Linear.V3
import Data.Array.Accelerate.Linear.V4

import Data.Typeable
import Control.Lens
import Linear.Plucker                           ( Plucker(..) )
import qualified Linear.Plucker                 as L


-- | Valid Plücker coordinates @p@ will have @'squaredError' p '==' 0@
--
-- That said, floating point makes a mockery of this claim, so you may want to
-- use 'nearZero'.
--
squaredError :: forall a. (Elt a, IsNum a) => Exp (Plucker a) -> Exp a
squaredError = lift1 (L.squaredError :: Plucker (Exp a) -> Exp a)

-- | This isn't the actual metric because this bilinear form gives rise to an
-- isotropic quadratic space
--
infixl 5 ><
(><) :: forall a. (Elt a, IsNum a) => Exp (Plucker a) -> Exp (Plucker a) -> Exp a
(><) = lift2 ((L.><) :: Plucker (Exp a) -> Plucker (Exp a) -> Exp a)

-- | Given a pair of points represented by homogeneous coordinates generate
-- Plücker coordinates for the line through them, directed from the second
-- towards the first.
--
plucker :: forall a. (Elt a, IsNum a) => Exp (V4 a) -> Exp (V4 a) -> Exp (Plucker a)
plucker = lift2 (L.plucker :: V4 (Exp a) -> V4 (Exp a) -> Plucker (Exp a))

-- | Given a pair of 3D points, generate Plücker coordinates for the line
-- through them, directed from the second towards the first.
--
plucker3D :: forall a. (Elt a, IsNum a) => Exp (V3 a) -> Exp (V3 a) -> Exp (Plucker a)
plucker3D = lift2 (L.plucker3D :: V3 (Exp a) -> V3 (Exp a) -> Plucker (Exp a))


-- Instances
-- ---------

deriving instance Typeable Plucker

instance Metric Plucker
instance Additive Plucker

type instance EltRepr (Plucker a) = EltRepr (a, a, a, a, a, a)

instance Elt a => Elt (Plucker a) where
  eltType _ = eltType (undefined :: (a,a,a,a,a,a))
  toElt p = case toElt p of
     (x, y, z, w, u, v) -> Plucker x y z w u v
  fromElt (Plucker x y z w u v) = fromElt (x, y, z, w, u, v)

instance cst a => IsProduct cst (Plucker a) where
  type ProdRepr (Plucker a) = ProdRepr (a,a,a,a,a,a)
  fromProd p (Plucker x y z w u v) = fromProd p (x, y, z, w, u, v)
  toProd p t = case toProd p t of
     (x, y, z, w, u, v) -> Plucker x y z w u v
  prod p _ = prod p (undefined :: (a,a,a,a,a,a))

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Plucker a) where
  type Plain (Plucker a) = Plucker (Plain a)
  -- lift = Exp . Tuple . F.foldl SnocTup NilTup
  lift (Plucker x y z w u v) =
    Exp $ Tuple $
      NilTup `SnocTup` lift x
             `SnocTup` lift y
             `SnocTup` lift z
             `SnocTup` lift w
             `SnocTup` lift u
             `SnocTup` lift v

instance Elt a => Unlift Exp (Plucker (Exp a)) where
  unlift t = Plucker
    (Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))) `Prj` t)
    (Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Prj` t)
    (Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` t)
    (Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t)
    (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
    (Exp $ ZeroTupIdx `Prj` t)

instance (Elt a, Elt b) => Each (Exp (Plucker a)) (Exp (Plucker b)) (Exp a) (Exp b) where
  each = liftLens (each :: Traversal (Plucker (Exp a)) (Plucker (Exp b)) (Exp a) (Exp b))

