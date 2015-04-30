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
  R1(..),
  R2(..),
  _yx,
  R3(..),
  _xz, _yz, _zx, _zy,
  _xzy, _yxz, _yzx, _zxy, _zyx,
  R4(..),
  _xw, _yw, _zw, _wx, _wy, _wz,
  _xyw, _xzw, _xwy, _xwz, _yxw, _yzw, _ywx, _ywz, _zxw, _zyw, _zwx, _zwy,
  _wxy, _wxz, _wyx, _wyz, _wzx, _wzy,
  _xywz, _xzyw, _xzwy, _xwyz, _xwzy, _yxzw , _yxwz, _yzxw, _yzwx, _ywxz,
  _ywzx, _zxyw, _zxwy, _zyxw, _zywx, _zwxy, _zwyx, _wxyz, _wxzy, _wyxz,
  _wyzx, _wzxy, _wzyx,
  ex, ey, ez, ew,

) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Array.Sugar

import Data.Array.Accelerate.Linear.Metric
import Data.Array.Accelerate.Linear.Type
import Data.Array.Accelerate.Linear.V1
import Data.Array.Accelerate.Linear.V2
import Data.Array.Accelerate.Linear.V3
import Data.Array.Accelerate.Linear.Vector

import Control.Lens
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

-- | A space that distinguishes orthogonal basis vectors '_x', '_y', '_z', and '_w'.
-- (Although it may have more.)
--
class (L.R4 t, R3 t) => R4 t where
  -- |
  -- >>> V4 1 2 3 4 ^._w
  -- 4
  --
  _w :: forall a. (Elt a, Box t a) => Lens' (Exp (t a)) (Exp a)
  _w = liftLens (L._w :: Lens' (t (Exp a)) (Exp a))

  _xyzw :: forall a. (Elt a, Box t a) => Lens' (Exp (t a)) (Exp (V4 a))
  _xyzw f = liftLens (L._xyzw :: Lens' (t (Exp a)) (V4 (Exp a))) (fsink1 f)


_xw, _yw, _zw, _wx, _wy, _wz
    :: forall t a. (R4 t, Elt a, Box t a)
    => Lens' (Exp (t a)) (Exp (V2 a))
_xw f = liftLens (L._xw :: Lens' (t (Exp a)) (V2 (Exp a))) (fsink1 f)
_yw f = liftLens (L._yw :: Lens' (t (Exp a)) (V2 (Exp a))) (fsink1 f)
_zw f = liftLens (L._zw :: Lens' (t (Exp a)) (V2 (Exp a))) (fsink1 f)
_wx f = liftLens (L._wx :: Lens' (t (Exp a)) (V2 (Exp a))) (fsink1 f)
_wy f = liftLens (L._wy :: Lens' (t (Exp a)) (V2 (Exp a))) (fsink1 f)
_wz f = liftLens (L._wz :: Lens' (t (Exp a)) (V2 (Exp a))) (fsink1 f)

_xyw, _xzw, _xwy, _xwz, _yxw, _yzw, _ywx, _ywz, _zxw, _zyw, _zwx, _zwy, _wxy, _wxz, _wyx, _wyz, _wzx, _wzy
    :: forall t a. (R4 t, Elt a, Box t a)
    => Lens' (Exp (t a)) (Exp (V3 a))
_xyw f = liftLens (L._xyw :: Lens' (t (Exp a)) (V3 (Exp a))) (fsink1 f)
_xzw f = liftLens (L._xzw :: Lens' (t (Exp a)) (V3 (Exp a))) (fsink1 f)
_xwy f = liftLens (L._xwy :: Lens' (t (Exp a)) (V3 (Exp a))) (fsink1 f)
_xwz f = liftLens (L._xwz :: Lens' (t (Exp a)) (V3 (Exp a))) (fsink1 f)
_yxw f = liftLens (L._yxw :: Lens' (t (Exp a)) (V3 (Exp a))) (fsink1 f)
_yzw f = liftLens (L._yzw :: Lens' (t (Exp a)) (V3 (Exp a))) (fsink1 f)
_ywx f = liftLens (L._ywx :: Lens' (t (Exp a)) (V3 (Exp a))) (fsink1 f)
_ywz f = liftLens (L._ywz :: Lens' (t (Exp a)) (V3 (Exp a))) (fsink1 f)
_zxw f = liftLens (L._zxw :: Lens' (t (Exp a)) (V3 (Exp a))) (fsink1 f)
_zyw f = liftLens (L._zyw :: Lens' (t (Exp a)) (V3 (Exp a))) (fsink1 f)
_zwx f = liftLens (L._zwx :: Lens' (t (Exp a)) (V3 (Exp a))) (fsink1 f)
_zwy f = liftLens (L._zwy :: Lens' (t (Exp a)) (V3 (Exp a))) (fsink1 f)
_wxy f = liftLens (L._wxy :: Lens' (t (Exp a)) (V3 (Exp a))) (fsink1 f)
_wxz f = liftLens (L._wxz :: Lens' (t (Exp a)) (V3 (Exp a))) (fsink1 f)
_wyx f = liftLens (L._wyx :: Lens' (t (Exp a)) (V3 (Exp a))) (fsink1 f)
_wyz f = liftLens (L._wyz :: Lens' (t (Exp a)) (V3 (Exp a))) (fsink1 f)
_wzx f = liftLens (L._wzx :: Lens' (t (Exp a)) (V3 (Exp a))) (fsink1 f)
_wzy f = liftLens (L._wzy :: Lens' (t (Exp a)) (V3 (Exp a))) (fsink1 f)

_xywz, _xzyw, _xzwy, _xwyz, _xwzy, _yxzw , _yxwz, _yzxw, _yzwx, _ywxz , _ywzx, _zxyw,
    _zxwy, _zyxw, _zywx, _zwxy, _zwyx, _wxyz, _wxzy, _wyxz, _wyzx, _wzxy, _wzyx
  :: forall t a. (R4 t, Elt a, Box t a)
  => Lens' (Exp (t a)) (Exp (V4 a))
_xywz f = liftLens (L._xywz :: Lens' (t (Exp a)) (V4 (Exp a))) (fsink1 f)
_xzyw f = liftLens (L._xzyw :: Lens' (t (Exp a)) (V4 (Exp a))) (fsink1 f)
_xzwy f = liftLens (L._xzwy :: Lens' (t (Exp a)) (V4 (Exp a))) (fsink1 f)
_xwyz f = liftLens (L._xwyz :: Lens' (t (Exp a)) (V4 (Exp a))) (fsink1 f)
_xwzy f = liftLens (L._xwzy :: Lens' (t (Exp a)) (V4 (Exp a))) (fsink1 f)
_yxzw f = liftLens (L._yxzw :: Lens' (t (Exp a)) (V4 (Exp a))) (fsink1 f)
_yxwz f = liftLens (L._yxwz :: Lens' (t (Exp a)) (V4 (Exp a))) (fsink1 f)
_yzxw f = liftLens (L._yzxw :: Lens' (t (Exp a)) (V4 (Exp a))) (fsink1 f)
_yzwx f = liftLens (L._yzwx :: Lens' (t (Exp a)) (V4 (Exp a))) (fsink1 f)
_ywxz f = liftLens (L._ywxz :: Lens' (t (Exp a)) (V4 (Exp a))) (fsink1 f)
_ywzx f = liftLens (L._ywzx :: Lens' (t (Exp a)) (V4 (Exp a))) (fsink1 f)
_zxyw f = liftLens (L._zxyw :: Lens' (t (Exp a)) (V4 (Exp a))) (fsink1 f)
_zxwy f = liftLens (L._zxwy :: Lens' (t (Exp a)) (V4 (Exp a))) (fsink1 f)
_zyxw f = liftLens (L._zyxw :: Lens' (t (Exp a)) (V4 (Exp a))) (fsink1 f)
_zywx f = liftLens (L._zywx :: Lens' (t (Exp a)) (V4 (Exp a))) (fsink1 f)
_zwxy f = liftLens (L._zwxy :: Lens' (t (Exp a)) (V4 (Exp a))) (fsink1 f)
_zwyx f = liftLens (L._zwyx :: Lens' (t (Exp a)) (V4 (Exp a))) (fsink1 f)
_wxyz f = liftLens (L._wxyz :: Lens' (t (Exp a)) (V4 (Exp a))) (fsink1 f)
_wxzy f = liftLens (L._wxzy :: Lens' (t (Exp a)) (V4 (Exp a))) (fsink1 f)
_wyxz f = liftLens (L._wyxz :: Lens' (t (Exp a)) (V4 (Exp a))) (fsink1 f)
_wyzx f = liftLens (L._wyzx :: Lens' (t (Exp a)) (V4 (Exp a))) (fsink1 f)
_wzxy f = liftLens (L._wzxy :: Lens' (t (Exp a)) (V4 (Exp a))) (fsink1 f)
_wzyx f = liftLens (L._wzyx :: Lens' (t (Exp a)) (V4 (Exp a))) (fsink1 f)


ew :: R4 t => E t
ew = E _w


-- Instances
-- ---------

instance Metric V4
instance Additive V4
instance R1 V4
instance R2 V4
instance R3 V4
instance R4 V4

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

instance (Elt a, IsNum a) => Num (Exp (V4 a)) where
  (+)           = lift2 ((+) :: V4 (Exp a) -> V4 (Exp a) -> V4 (Exp a))
  (-)           = lift2 ((-) :: V4 (Exp a) -> V4 (Exp a) -> V4 (Exp a))
  (*)           = lift2 ((*) :: V4 (Exp a) -> V4 (Exp a) -> V4 (Exp a))
  negate        = lift1 (negate :: V4 (Exp a) -> V4 (Exp a))
  signum        = lift1 (signum :: V4 (Exp a) -> V4 (Exp a))
  abs           = lift1 (signum :: V4 (Exp a) -> V4 (Exp a))
  fromInteger   = constant . fromInteger

instance (Elt a, IsFloating a) => Fractional (Exp (V4 a)) where
  (/)           = lift2 ((/) :: V4 (Exp a) -> V4 (Exp a) -> V4 (Exp a))
  recip         = lift1 (recip :: V4 (Exp a) -> V4 (Exp a))
  fromRational  = constant . fromRational

instance (Elt a, IsFloating a) => Floating (Exp (V4 a)) where
  pi            = lift (pi :: V4 (Exp a))
  log           = lift1 (log :: V4 (Exp a) -> V4 (Exp a))
  exp           = lift1 (exp :: V4 (Exp a) -> V4 (Exp a))
  sin           = lift1 (sin :: V4 (Exp a) -> V4 (Exp a))
  cos           = lift1 (cos :: V4 (Exp a) -> V4 (Exp a))
  tan           = lift1 (tan :: V4 (Exp a) -> V4 (Exp a))
  sinh          = lift1 (sinh :: V4 (Exp a) -> V4 (Exp a))
  cosh          = lift1 (cosh :: V4 (Exp a) -> V4 (Exp a))
  tanh          = lift1 (tanh :: V4 (Exp a) -> V4 (Exp a))
  asin          = lift1 (asin :: V4 (Exp a) -> V4 (Exp a))
  acos          = lift1 (acos :: V4 (Exp a) -> V4 (Exp a))
  atan          = lift1 (atan :: V4 (Exp a) -> V4 (Exp a))
  asinh         = lift1 (asinh :: V4 (Exp a) -> V4 (Exp a))
  acosh         = lift1 (acosh :: V4 (Exp a) -> V4 (Exp a))
  atanh         = lift1 (atanh :: V4 (Exp a) -> V4 (Exp a))

