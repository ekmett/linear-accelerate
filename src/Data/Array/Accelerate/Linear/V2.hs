{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE IncoherentInstances   #-}
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

import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Array.Sugar

import Data.Array.Accelerate.Linear.Lift
import Data.Array.Accelerate.Linear.Metric
import Data.Array.Accelerate.Linear.Type
import Data.Array.Accelerate.Linear.V1
import Data.Array.Accelerate.Linear.Vector

import Control.Lens
import Linear.V2                                ( V2(..) )
import qualified Linear.V2                      as L
import Prelude                                  as P


-- | the counter-clockwise perpendicular vector
--
-- >>> perp $ V2 10 20
-- V2 (-20) 10
--
perp :: forall a. A.Num a => Exp (V2 a) -> Exp (V2 a)
perp = lift1 (L.perp :: V2 (Exp a) -> V2 (Exp a))


-- | Unit vector with given phase angle (modulo 2*'pi')
--
angle :: A.Floating a => Exp a -> Exp (V2 a)
angle = lift . L.angle


-- | A space that distinguishes 2 orthogonal basis vectors '_x' and '_y', but
-- may have more.
--
class (L.R2 t, R1 t) => R2 t where
  -- |
  -- >>> V2 1 2 ^._y
  -- 2
  --
  -- >>> V2 1 2 & _y .~ 3
  -- V2 1 3
  --
  _y :: (Elt a, Box t a) => Lens' (Exp (t a)) (Exp a)
  _y = liftLens (L._y :: Lens' (t (Exp a)) (Exp a))

  _xy :: (Elt a, Box t a) => Lens' (Exp (t a)) (Exp (V2 a))
  _xy = liftLens (L._xy :: Lens' (t (Exp a)) (V2 (Exp a)))


-- |
-- >>> V2 1 2 ^. _yx
-- V2 2 1
--
_yx :: forall t a. (R2 t, Elt a, Box t a) => Lens' (Exp (t a)) (Exp (V2 a))
_yx = liftLens (L._yx :: Lens' (t (Exp a)) (V2 (Exp a)))


ey :: R2 t => E t
ey = E _y


-- Instances
-- ---------

instance Metric V2
instance Additive V2
instance R1 V2
instance R2 V2

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
  lift (V2 x y) = Exp $ Tuple $ NilTup `SnocTup` lift x `SnocTup` lift y

instance Elt a => Unlift Exp (V2 (Exp a)) where
  unlift t = V2 (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                (Exp $ ZeroTupIdx `Prj` t)

instance A.Num a => P.Num (Exp (V2 a)) where
  (+)             = lift2 ((+) :: V2 (Exp a) -> V2 (Exp a) -> V2 (Exp a))
  (-)             = lift2 ((-) :: V2 (Exp a) -> V2 (Exp a) -> V2 (Exp a))
  (*)             = lift2 ((*) :: V2 (Exp a) -> V2 (Exp a) -> V2 (Exp a))
  negate          = lift1 (negate :: V2 (Exp a) -> V2 (Exp a))
  signum          = lift1 (signum :: V2 (Exp a) -> V2 (Exp a))
  abs             = lift1 (signum :: V2 (Exp a) -> V2 (Exp a))
  fromInteger x   = lift (fromInteger x :: V2 (Exp a))

instance A.Floating a => P.Fractional (Exp (V2 a)) where
  (/)             = lift2 ((/) :: V2 (Exp a) -> V2 (Exp a) -> V2 (Exp a))
  recip           = lift1 (recip :: V2 (Exp a) -> V2 (Exp a))
  fromRational x  = lift (fromRational x :: V2 (Exp a))

instance A.Floating a => P.Floating (Exp (V2 a)) where
  pi              = lift (pi :: V2 (Exp a))
  log             = lift1 (log :: V2 (Exp a) -> V2 (Exp a))
  exp             = lift1 (exp :: V2 (Exp a) -> V2 (Exp a))
  sin             = lift1 (sin :: V2 (Exp a) -> V2 (Exp a))
  cos             = lift1 (cos :: V2 (Exp a) -> V2 (Exp a))
  tan             = lift1 (tan :: V2 (Exp a) -> V2 (Exp a))
  sinh            = lift1 (sinh :: V2 (Exp a) -> V2 (Exp a))
  cosh            = lift1 (cosh :: V2 (Exp a) -> V2 (Exp a))
  tanh            = lift1 (tanh :: V2 (Exp a) -> V2 (Exp a))
  asin            = lift1 (asin :: V2 (Exp a) -> V2 (Exp a))
  acos            = lift1 (acos :: V2 (Exp a) -> V2 (Exp a))
  atan            = lift1 (atan :: V2 (Exp a) -> V2 (Exp a))
  asinh           = lift1 (asinh :: V2 (Exp a) -> V2 (Exp a))
  acosh           = lift1 (acosh :: V2 (Exp a) -> V2 (Exp a))
  atanh           = lift1 (atanh :: V2 (Exp a) -> V2 (Exp a))

instance (Elt a, Elt b) => Each (Exp (V2 a)) (Exp (V2 b)) (Exp a) (Exp b) where
  each = liftLens (each :: Traversal (V2 (Exp a)) (V2 (Exp b)) (Exp a) (Exp b))

