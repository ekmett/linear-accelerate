{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Accelerate.Linear.V3
-- Copyright   : 2014 Edward Kmett, Charles Durham,
--               2015 Trevor L. McDonell
-- License     : BSD-style (see the file LICENSE)
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- 3-D Vectors
----------------------------------------------------------------------------

module Data.Array.Accelerate.Linear.V3 (

  V3(..), cross, triple,

) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Array.Sugar

import Data.Array.Accelerate.Linear.Metric
import Data.Array.Accelerate.Linear.Vector

import Linear.V3                                ( V3(..) )
import qualified Linear.V3                      as L
import qualified Data.Foldable                  as F


-- | cross product
--
cross :: (Elt a, IsNum a) => Exp (V3 a) -> Exp (V3 a) -> Exp (V3 a)
cross = lift2 L.cross

-- | scalar triple product
--
triple :: (Elt a, IsNum a) => Exp (V3 a) -> Exp (V3 a) -> Exp (V3 a) -> Exp a
triple a b c = dot a (cross b c)


-- Instances
-- ---------

instance Metric V3
instance Additive V3

type instance EltRepr (V3 a) = EltRepr (a, a, a)

instance Elt a => Elt (V3 a) where
  eltType _ = eltType (undefined :: (a,a,a))
  toElt p = case toElt p of
     (x, y, z) -> V3 x y z
  fromElt (V3 x y z) = fromElt (x, y, z)

instance cst a => IsProduct cst (V3 a) where
  type ProdRepr (V3 a) = ProdRepr (a,a,a)
  fromProd p (V3 x y z) = fromProd p (x,y,z)
  toProd p t = case toProd p t of
     (x, y, z) -> V3 x y z
  prod p _ = prod p (undefined :: (a,a,a))

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (V3 a) where
  type Plain (V3 a) = V3 (Plain a)
  --lift = Exp . Tuple . F.foldl SnocTup NilTup
  lift (V3 x y z) = Exp $ Tuple $ NilTup `SnocTup` lift x `SnocTup` lift y `SnocTup` lift z

instance (Elt a, e ~ Exp a) => Unlift Exp (V3 e) where
  unlift t = V3 (Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t)
                (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                (Exp $ ZeroTupIdx `Prj` t)

type instance ArrRepr (V3 a) = ArrRepr (Vector a)

instance Elt a => Arrays (V3 a) where
  arrays _ = arrays (undefined :: Vector a)
  toArr arr = case toList arr of
    [a,b,c] -> V3 a b c
    _       -> error "shape mismatch"
  fromArr = fromList (Z :. 3) . F.toList
  flavour _ = error "https://github.com/AccelerateHS/accelerate/issues/263"

-- $liftAcc
--
-- In theory we could support lifting these to 'Acc' array types as well, however
-- since the class associated type for that ignores one of its arguments, this requires
--
-- @
-- type 'Plain' ('V3' a) = 'Vector' a
-- @
--
-- while in order to instantiate the @'Lift' 'Exp` (V3 a)@ above we need
--
-- @
-- type 'Plain' ('V3' a) = V3 ('Plain' a)
-- @
--
-- so due to limitations in the accelerate API, we can't support both!

{-
instance Elt a => Lift Acc (V3 a) where
  type Plain (V3 a) = Vector a
  lift = lift . toArr'
-}

