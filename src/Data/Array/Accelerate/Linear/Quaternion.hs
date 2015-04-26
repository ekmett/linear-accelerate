{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Accelerate.Linear.Quaternion
-- Copyright   : 2014 Edward Kmett, Charles Durham,
--               2015 Trevor L. McDonell
-- License     : BSD-style (see the file LICENSE)
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Quaternions
----------------------------------------------------------------------------

module Data.Array.Accelerate.Linear.Quaternion (

  Quaternion(..),

) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Array.Sugar

import Data.Array.Accelerate.Linear.Metric
import Data.Array.Accelerate.Linear.V3

import Linear.Quaternion                        ( Quaternion(..) )


-- Instances
-- ---------

instance Metric Quaternion

type instance EltRepr (Quaternion a) = EltRepr (a, a, a, a)

instance Elt a => Elt (Quaternion a) where
  eltType _ = eltType (undefined :: (a,a,a,a))
  toElt p = case toElt p of
     (x, y, z, w) -> Quaternion x (V3 y z w)
  fromElt (Quaternion x (V3 y z w)) = fromElt (x, y, z, w)

instance cst a => IsProduct cst (Quaternion a) where
  type ProdRepr (Quaternion a) = ProdRepr (a,a,a,a)
  fromProd p (Quaternion x (V3 y z w)) = fromProd p (x,y,z,w)
  toProd p t = case toProd p t of
     (x, y, z, w) -> Quaternion x (V3 y z w)
  prod p _ = prod p (undefined :: (a,a,a,a))

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Quaternion a) where
  type Plain (Quaternion a) = Quaternion (Plain a)
  --lift = Exp . Tuple . F.foldl SnocTup NilTup
  lift (Quaternion x (V3 y z w)) = Exp $ Tuple $ NilTup `SnocTup`
                                   lift x `SnocTup`
                                   lift y `SnocTup`
                                   lift z `SnocTup`
                                   lift w

instance (Elt a, e ~ Exp a) => Unlift Exp (Quaternion e) where
  unlift t = Quaternion (Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` t)
                    (V3 (Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t)
                        (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                        (Exp $ ZeroTupIdx `Prj` t))

