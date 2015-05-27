{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE IncoherentInstances   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Accelerate.Linear.V0
-- Copyright   : 2014 Edward Kmett, Charles Durham,
--               2015 Trevor L. McDonell
-- License     : BSD-style (see the file LICENSE)
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- 0-D Vectors
----------------------------------------------------------------------------

module Data.Array.Accelerate.Linear.V0 (

  V0(..),

) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Array.Sugar

import Data.Array.Accelerate.Linear.Metric
import Data.Array.Accelerate.Linear.Vector

import Control.Lens
import Linear.V0                                ( V0(..) )


-- Instances
-- ---------

instance Metric V0
instance Additive V0

type instance EltRepr (V0 a) = ()

instance Elt a => Elt (V0 a) where
  eltType _ = eltType ()
  toElt () = V0
  fromElt V0 = ()

instance IsProduct cst (V0 a) where
  type ProdRepr (V0 a) = ()
  fromProd _ V0 = ()
  toProd _ () = V0
  prod _ _ = ProdRunit

instance Lift Exp (V0 a) where
  type Plain (V0 a) = ()
  lift V0 = Exp (Tuple NilTup)

instance Unlift Exp (V0 a) where
  unlift _ = V0

instance (Elt a, IsNum a) => Num (Exp (V0 a)) where
  _ + _ = constant V0
  _ - _ = constant V0
  _ * _ = constant V0
  abs _ = constant V0
  signum _ = constant V0
  fromInteger _ = constant V0

instance (Elt a, Elt b) => Each (Exp (V0 a)) (Exp (V0 b)) (Exp a) (Exp b) where
  each _ _ = pure (constant V0)

