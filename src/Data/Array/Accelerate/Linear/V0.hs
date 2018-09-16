{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Accelerate.Linear.V0
-- Copyright   : 2014 Edward Kmett, Charles Durham,
--               [2015..2018] Trevor L. McDonell
-- License     : BSD-style (see the file LICENSE)
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- 0-D Vectors
----------------------------------------------------------------------------

module Data.Array.Accelerate.Linear.V0 (

  V0(..), pattern V0',

) where

import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.Data.Functor       as A
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Array.Sugar

import Data.Array.Accelerate.Linear.Metric
import Data.Array.Accelerate.Linear.Vector

import Linear.V0                                ( V0(..) )
import Control.Lens
import Control.Applicative
import Prelude                                  as P


-- Instances
-- ---------

pattern V0' :: Elt a => Exp (V0 a)
pattern V0' = Pattern ()

instance Metric V0
instance Additive V0
instance Elt a => Elt (V0 a)
instance Elt a => IsProduct Elt (V0 a)

instance Lift Exp (V0 a) where
  type Plain (V0 a) = ()
  lift V0 = Exp (Tuple NilTup)

instance Unlift Exp (V0 a) where
  unlift _ = V0

instance (Elt a, Elt b) => Each (Exp (V0 a)) (Exp (V0 b)) (Exp a) (Exp b) where
  each _ _ = pure (constant V0)

instance A.Num a => P.Num (Exp (V0 a)) where
  _ + _         = constant V0
  _ - _         = constant V0
  _ * _         = constant V0
  abs _         = constant V0
  signum _      = constant V0
  fromInteger _ = constant V0

instance Elt a => A.Eq (V0 a) where
  _ == _ = constant True
  _ /= _ = constant False

instance Elt a => A.Ord (V0 a) where
  _ < _   = constant False
  _ > _   = constant False
  _ <= _  = constant True
  _ >= _  = constant True
  min _ _ = constant V0
  max _ _ = constant V0

instance Elt a => P.Bounded (Exp (V0 a)) where
  minBound = constant V0
  maxBound = constant V0

instance A.Functor V0 where
  fmap _ _ = constant V0
  _ <$ _   = constant V0

