{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Accelerate.Linear.V4
-- Copyright   : 2014 Edward Kmett, Charles Durham,
--               [2015..2018] Trevor L. McDonell
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

import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.Data.Functor       as A
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Array.Sugar

import Data.Array.Accelerate.Linear.Epsilon
import Data.Array.Accelerate.Linear.Lift
import Data.Array.Accelerate.Linear.Metric
import Data.Array.Accelerate.Linear.Type
import Data.Array.Accelerate.Linear.V1
import Data.Array.Accelerate.Linear.V2
import Data.Array.Accelerate.Linear.V3
import Data.Array.Accelerate.Linear.Vector

import Control.Lens
import Data.Function
import Linear.V4                                ( V4(..) )
import Prelude                                  as P
import qualified Linear.V4                      as L

-- $setup
-- >>> import Data.Array.Accelerate.Interpreter
-- >>> :{
--   let test :: Elt e => Exp e -> e
--       test e = indexArray (run (unit e)) Z
-- :}


-- | Convert a 3-dimensional affine vector into a 4-dimensional homogeneous
-- vector.
--
vector :: forall a. A.Num a => Exp (V3 a) -> Exp (V4 a)
vector = lift1 (L.vector :: V3 (Exp a) -> V4 (Exp a))

-- | Convert a 3-dimensional affine point into a 4-dimensional homogeneous
-- vector.
--
point :: forall a. A.Num a => Exp (V3 a) -> Exp (V4 a)
point = lift1 (L.point :: V3 (Exp a) -> V4 (Exp a))

-- | Convert 4-dimensional projective coordinates to a 3-dimensional point. This
-- operation may be denoted, @euclidean [x:y:z:w] = (x\/w, y\/w, z\/w)@ where
-- the projective, homogenous, coordinate @[x:y:z:w]@ is one of many associated
-- with a single point @(x\/w, y\/w, z\/w)@.
--
normalizePoint :: forall a. A.Fractional a => Exp (V4 a) -> Exp (V3 a)
normalizePoint = lift1 (L.normalizePoint :: V4 (Exp a) -> V3 (Exp a))

-- | A space that distinguishes orthogonal basis vectors '_x', '_y', '_z', and '_w'.
-- (Although it may have more.)
--
class (L.R4 t, R3 t) => R4 t where
  -- |
  -- >>> test $ lift (V4 1 2 3 4 :: V4 Int) ^._w
  -- 4
  --
  -- >>> test $ lift (V4 1 2 3 4 :: V4 Int) & _w .~ 42
  -- V4 1 2 3 42
  --
  _w :: forall a. (Elt a, Box t a) => Lens' (Exp (t a)) (Exp a)
  _w = liftLens (L._w :: Lens' (t (Exp a)) (Exp a))

  _xyzw :: forall a. (Elt a, Box t a) => Lens' (Exp (t a)) (Exp (V4 a))
  _xyzw = liftLens (L._xyzw :: Lens' (t (Exp a)) (V4 (Exp a)))


_xw, _yw, _zw, _wx, _wy, _wz
    :: forall t a. (R4 t, Elt a, Box t a)
    => Lens' (Exp (t a)) (Exp (V2 a))
_xw = liftLens (L._xw :: Lens' (t (Exp a)) (V2 (Exp a)))
_yw = liftLens (L._yw :: Lens' (t (Exp a)) (V2 (Exp a)))
_zw = liftLens (L._zw :: Lens' (t (Exp a)) (V2 (Exp a)))
_wx = liftLens (L._wx :: Lens' (t (Exp a)) (V2 (Exp a)))
_wy = liftLens (L._wy :: Lens' (t (Exp a)) (V2 (Exp a)))
_wz = liftLens (L._wz :: Lens' (t (Exp a)) (V2 (Exp a)))

_xyw, _xzw, _xwy, _xwz, _yxw, _yzw, _ywx, _ywz, _zxw, _zyw, _zwx, _zwy, _wxy, _wxz, _wyx, _wyz, _wzx, _wzy
    :: forall t a. (R4 t, Elt a, Box t a)
    => Lens' (Exp (t a)) (Exp (V3 a))
_xyw = liftLens (L._xyw :: Lens' (t (Exp a)) (V3 (Exp a)))
_xzw = liftLens (L._xzw :: Lens' (t (Exp a)) (V3 (Exp a)))
_xwy = liftLens (L._xwy :: Lens' (t (Exp a)) (V3 (Exp a)))
_xwz = liftLens (L._xwz :: Lens' (t (Exp a)) (V3 (Exp a)))
_yxw = liftLens (L._yxw :: Lens' (t (Exp a)) (V3 (Exp a)))
_yzw = liftLens (L._yzw :: Lens' (t (Exp a)) (V3 (Exp a)))
_ywx = liftLens (L._ywx :: Lens' (t (Exp a)) (V3 (Exp a)))
_ywz = liftLens (L._ywz :: Lens' (t (Exp a)) (V3 (Exp a)))
_zxw = liftLens (L._zxw :: Lens' (t (Exp a)) (V3 (Exp a)))
_zyw = liftLens (L._zyw :: Lens' (t (Exp a)) (V3 (Exp a)))
_zwx = liftLens (L._zwx :: Lens' (t (Exp a)) (V3 (Exp a)))
_zwy = liftLens (L._zwy :: Lens' (t (Exp a)) (V3 (Exp a)))
_wxy = liftLens (L._wxy :: Lens' (t (Exp a)) (V3 (Exp a)))
_wxz = liftLens (L._wxz :: Lens' (t (Exp a)) (V3 (Exp a)))
_wyx = liftLens (L._wyx :: Lens' (t (Exp a)) (V3 (Exp a)))
_wyz = liftLens (L._wyz :: Lens' (t (Exp a)) (V3 (Exp a)))
_wzx = liftLens (L._wzx :: Lens' (t (Exp a)) (V3 (Exp a)))
_wzy = liftLens (L._wzy :: Lens' (t (Exp a)) (V3 (Exp a)))

_xywz, _xzyw, _xzwy, _xwyz, _xwzy, _yxzw , _yxwz, _yzxw, _yzwx, _ywxz , _ywzx, _zxyw,
    _zxwy, _zyxw, _zywx, _zwxy, _zwyx, _wxyz, _wxzy, _wyxz, _wyzx, _wzxy, _wzyx
  :: forall t a. (R4 t, Elt a, Box t a)
  => Lens' (Exp (t a)) (Exp (V4 a))
_xywz = liftLens (L._xywz :: Lens' (t (Exp a)) (V4 (Exp a)))
_xzyw = liftLens (L._xzyw :: Lens' (t (Exp a)) (V4 (Exp a)))
_xzwy = liftLens (L._xzwy :: Lens' (t (Exp a)) (V4 (Exp a)))
_xwyz = liftLens (L._xwyz :: Lens' (t (Exp a)) (V4 (Exp a)))
_xwzy = liftLens (L._xwzy :: Lens' (t (Exp a)) (V4 (Exp a)))
_yxzw = liftLens (L._yxzw :: Lens' (t (Exp a)) (V4 (Exp a)))
_yxwz = liftLens (L._yxwz :: Lens' (t (Exp a)) (V4 (Exp a)))
_yzxw = liftLens (L._yzxw :: Lens' (t (Exp a)) (V4 (Exp a)))
_yzwx = liftLens (L._yzwx :: Lens' (t (Exp a)) (V4 (Exp a)))
_ywxz = liftLens (L._ywxz :: Lens' (t (Exp a)) (V4 (Exp a)))
_ywzx = liftLens (L._ywzx :: Lens' (t (Exp a)) (V4 (Exp a)))
_zxyw = liftLens (L._zxyw :: Lens' (t (Exp a)) (V4 (Exp a)))
_zxwy = liftLens (L._zxwy :: Lens' (t (Exp a)) (V4 (Exp a)))
_zyxw = liftLens (L._zyxw :: Lens' (t (Exp a)) (V4 (Exp a)))
_zywx = liftLens (L._zywx :: Lens' (t (Exp a)) (V4 (Exp a)))
_zwxy = liftLens (L._zwxy :: Lens' (t (Exp a)) (V4 (Exp a)))
_zwyx = liftLens (L._zwyx :: Lens' (t (Exp a)) (V4 (Exp a)))
_wxyz = liftLens (L._wxyz :: Lens' (t (Exp a)) (V4 (Exp a)))
_wxzy = liftLens (L._wxzy :: Lens' (t (Exp a)) (V4 (Exp a)))
_wyxz = liftLens (L._wyxz :: Lens' (t (Exp a)) (V4 (Exp a)))
_wyzx = liftLens (L._wyzx :: Lens' (t (Exp a)) (V4 (Exp a)))
_wzxy = liftLens (L._wzxy :: Lens' (t (Exp a)) (V4 (Exp a)))
_wzyx = liftLens (L._wzyx :: Lens' (t (Exp a)) (V4 (Exp a)))


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

instance Elt a => Elt (V4 a) where
  type EltRepr (V4 a) = EltRepr (a, a, a, a)
  eltType = eltType @(a, a, a, a)
  toElt t = let (x, y, z, w) = toElt t in V4 x y z w
  fromElt (V4 x y z w) = fromElt (x, y, z, w)

instance cst a => IsProduct cst (V4 a)

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (V4 a) where
  type Plain (V4 a) = V4 (Plain a)
  lift (V4 x y z w) =
    Exp $ Tuple $ NilTup `SnocTup` lift x
                         `SnocTup` lift y
                         `SnocTup` lift z
                         `SnocTup` lift w

instance Elt a => Unlift Exp (V4 (Exp a)) where
  unlift t = V4 (Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` t)
                (Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t)
                (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                (Exp $ ZeroTupIdx `Prj` t)

instance (Elt a, Elt b) => Each (Exp (V4 a)) (Exp (V4 b)) (Exp a) (Exp b) where
  each = liftLens (each :: Traversal (V4 (Exp a)) (V4 (Exp b)) (Exp a) (Exp b))

instance A.Eq a => A.Eq (V4 a) where
  (==) = (A.==) `on` t4
  (/=) = (A./=) `on` t4

instance A.Ord a => A.Ord (V4 a) where
  (<)  = (A.<) `on` t4
  (>)  = (A.>) `on` t4
  (<=) = (A.<=) `on` t4
  (>=) = (A.>=) `on` t4
  min  = v4 $$ on A.min t4
  max  = v4 $$ on A.max t4

t4 :: Elt a => Exp (V4 a) -> Exp (a,a,a,a)
t4 (unlift -> V4 x y z w) = tup4 (x,y,z,w)

v4 :: Elt a => Exp (a,a,a,a) -> Exp (V4 a)
v4 (untup4 -> (x,y,z,w)) = lift (V4 x y z w)

instance A.Bounded a => P.Bounded (Exp (V4 a)) where
  minBound = lift (V4 (minBound :: Exp a) (minBound :: Exp a) (minBound :: Exp a) (minBound :: Exp a))
  maxBound = lift (V4 (maxBound :: Exp a) (maxBound :: Exp a) (maxBound :: Exp a) (maxBound :: Exp a))

instance A.Num a => P.Num (Exp (V4 a)) where
  (+)             = lift2 ((+) :: V4 (Exp a) -> V4 (Exp a) -> V4 (Exp a))
  (-)             = lift2 ((-) :: V4 (Exp a) -> V4 (Exp a) -> V4 (Exp a))
  (*)             = lift2 ((*) :: V4 (Exp a) -> V4 (Exp a) -> V4 (Exp a))
  negate          = lift1 (negate :: V4 (Exp a) -> V4 (Exp a))
  signum          = lift1 (signum :: V4 (Exp a) -> V4 (Exp a))
  abs             = lift1 (signum :: V4 (Exp a) -> V4 (Exp a))
  fromInteger x   = lift (P.fromInteger x :: V4 (Exp a))

instance A.Floating a => P.Fractional (Exp (V4 a)) where
  (/)             = lift2 ((/) :: V4 (Exp a) -> V4 (Exp a) -> V4 (Exp a))
  recip           = lift1 (recip :: V4 (Exp a) -> V4 (Exp a))
  fromRational x  = lift (P.fromRational x :: V4 (Exp a))

instance A.Floating a => P.Floating (Exp (V4 a)) where
  pi              = lift (pi :: V4 (Exp a))
  log             = lift1 (log :: V4 (Exp a) -> V4 (Exp a))
  exp             = lift1 (exp :: V4 (Exp a) -> V4 (Exp a))
  sin             = lift1 (sin :: V4 (Exp a) -> V4 (Exp a))
  cos             = lift1 (cos :: V4 (Exp a) -> V4 (Exp a))
  tan             = lift1 (tan :: V4 (Exp a) -> V4 (Exp a))
  sinh            = lift1 (sinh :: V4 (Exp a) -> V4 (Exp a))
  cosh            = lift1 (cosh :: V4 (Exp a) -> V4 (Exp a))
  tanh            = lift1 (tanh :: V4 (Exp a) -> V4 (Exp a))
  asin            = lift1 (asin :: V4 (Exp a) -> V4 (Exp a))
  acos            = lift1 (acos :: V4 (Exp a) -> V4 (Exp a))
  atan            = lift1 (atan :: V4 (Exp a) -> V4 (Exp a))
  asinh           = lift1 (asinh :: V4 (Exp a) -> V4 (Exp a))
  acosh           = lift1 (acosh :: V4 (Exp a) -> V4 (Exp a))
  atanh           = lift1 (atanh :: V4 (Exp a) -> V4 (Exp a))

instance Epsilon a => Epsilon (V4 a) where
  nearZero = nearZero . quadrance

instance A.Functor V4 where
  fmap f (unlift -> V4 x y z w) = lift (V4 (f x) (f y) (f z) (f w))
  x <$ _                        = lift (V4 x x x x)

