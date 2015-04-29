{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Accelerate.Linear.V2
-- Copyright   : 2014 Edward Kmett, Charles Durham,
--               2015 Trevor L. McDonell
-- License     : BSD-style (see the file LICENSE)
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- 2-D Vectors
----------------------------------------------------------------------------

module Data.Array.Accelerate.Linear.V2 (

  V2(..), R1(..), R2(..),
  _yx,
  ex, ey,
  perp, angle,

) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Array.Sugar

import Data.Array.Accelerate.Linear.Metric
import Data.Array.Accelerate.Linear.V1
import Data.Array.Accelerate.Linear.Vector

import Control.Lens
import Linear.V2                                ( V2(..) )
import qualified Linear.V2                      as L


-- | the counter-clockwise perpendicular vector
--
-- >>> perp $ V2 10 20
-- V2 (-20) 10
--
perp :: (Elt a, IsNum a) => Exp (V2 a) -> Exp (V2 a)
perp = lift1 L.perp


-- | Unit vector with given phase angle (modulo 2*'pi')
--
angle :: (Elt a, IsFloating a) => Exp a -> Exp (V2 a)
angle = lift . L.angle


-- | A space that distinguishes 2 orthogonal basis vectors '_x' and '_y', but
-- may have more.
--
class R1 t => R2 t where
  -- |
  -- >>> V2 1 2 ^._y
  -- 2
  --
  -- >>> V2 1 2 & _y .~ 3
  -- V2 1 3
  --
  _y :: Elt a => Lens' (Exp (t a)) (Exp a)
  _y = _xy._y

  _xy :: forall a. Elt a => Lens' (Exp (t a)) (Exp (V2 a))


-- |
-- >>> V2 1 2 ^. _yx
-- V2 2 1
--
_yx :: (R2 t, Elt a) => Lens' (Exp (t a)) (Exp (V2 a))
_yx f = _xy $ \(unlift -> V2 a b) -> f (lift (V2 b a)) <&> \(unlift -> V2 b' a') -> lift (V2 a' b')


ey :: R2 t => E t
ey = E _y


-- Instances
-- ---------

instance Metric V2
instance Additive V2

instance R1 V2 where
  _x f (unlift -> V2 a b) = (\a' -> lift $ V2 a' b) <$> f a

instance R2 V2 where
  _y f (unlift -> V2 a b) = (\b' -> lift $ V2 a b') <$> f b
  _xy = id

type instance EltRepr (V2 a) = EltRepr (a, a)

instance Elt a => Elt (V2 a) where
  eltType _ = eltType (undefined :: (a,a))
  toElt p = case toElt p of
     (x, y) -> V2 x y
  fromElt (V2 x y) = fromElt (x, y)

instance cst a => IsProduct cst (V2 a) where
  type ProdRepr (V2 a) = ProdRepr (a,a)
  fromProd p (V2 x y) = fromProd p (x,y)
  toProd p t = case toProd p t of
     (x, y) -> V2 x y
  prod p _ = prod p (undefined :: (a,a))

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (V2 a) where
  type Plain (V2 a) = V2 (Plain a)
  --lift = Exp . Tuple . F.foldl SnocTup NilTup
  lift (V2 x y) = Exp $ Tuple $ NilTup `SnocTup` lift x `SnocTup` lift y

instance (Elt a, e ~ Exp a) => Unlift Exp (V2 e) where
  unlift t = V2 (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                (Exp $ ZeroTupIdx `Prj` t)

