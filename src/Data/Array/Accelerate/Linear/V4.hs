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
class R3 t => R4 t where
  -- |
  -- >>> V4 1 2 3 4 ^._w
  -- 4
  --
  _w :: Elt a => Lens' (Exp (t a)) (Exp a)
  _xyzw :: Lens' (Exp (t a)) (Exp (V4 a))

_xw, _yw, _zw, _wx, _wy, _wz :: (R4 t, Elt a) => Lens' (Exp (t a)) (Exp (V2 a))
_xw f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V2 a d)) <&> lift1 (\(V2 a' d') -> V4 a' b c d')
_yw f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V2 b d)) <&> lift1 (\(V2 b' d') -> V4 a b' c d')
_zw f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V2 c d)) <&> lift1 (\(V2 c' d') -> V4 a b c' d')
_wx f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V2 d a)) <&> lift1 (\(V2 d' a') -> V4 a' b c d')
_wy f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V2 d b)) <&> lift1 (\(V2 d' b') -> V4 a b' c d')
_wz f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V2 d c)) <&> lift1 (\(V2 d' c') -> V4 a b c' d')

_xyw, _xzw, _xwy, _xwz, _yxw, _yzw, _ywx, _ywz, _zxw, _zyw, _zwx, _zwy, _wxy, _wxz, _wyx, _wyz, _wzx, _wzy :: (R4 t, Elt a) => Lens' (Exp (t a)) (Exp (V3 a))
_xyw f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V3 a b d)) <&> lift1 (\(V3 a' b' d') -> V4 a' b' c d')
_xzw f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V3 a c d)) <&> lift1 (\(V3 a' c' d') -> V4 a' b c' d')
_xwy f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V3 a d b)) <&> lift1 (\(V3 a' d' b') -> V4 a' b' c d')
_xwz f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V3 a d c)) <&> lift1 (\(V3 a' d' c') -> V4 a' b c' d')
_yxw f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V3 b a d)) <&> lift1 (\(V3 b' a' d') -> V4 a' b' c d')
_yzw f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V3 b c d)) <&> lift1 (\(V3 b' c' d') -> V4 a b' c' d')
_ywx f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V3 b d a)) <&> lift1 (\(V3 b' d' a') -> V4 a' b' c d')
_ywz f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V3 b d c)) <&> lift1 (\(V3 b' d' c') -> V4 a b' c' d')
_zxw f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V3 c a d)) <&> lift1 (\(V3 c' a' d') -> V4 a' b c' d')
_zyw f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V3 c b d)) <&> lift1 (\(V3 c' b' d') -> V4 a b' c' d')
_zwx f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V3 c d a)) <&> lift1 (\(V3 c' d' a') -> V4 a' b c' d')
_zwy f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V3 c d b)) <&> lift1 (\(V3 c' d' b') -> V4 a b' c' d')
_wxy f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V3 d a b)) <&> lift1 (\(V3 d' a' b') -> V4 a' b' c d')
_wxz f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V3 d a c)) <&> lift1 (\(V3 d' a' c') -> V4 a' b c' d')
_wyx f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V3 d b a)) <&> lift1 (\(V3 d' b' a') -> V4 a' b' c d')
_wyz f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V3 d b c)) <&> lift1 (\(V3 d' b' c') -> V4 a b' c' d')
_wzx f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V3 d c a)) <&> lift1 (\(V3 d' c' a') -> V4 a' b c' d')
_wzy f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V3 d c b)) <&> lift1 (\(V3 d' c' b') -> V4 a b' c' d')

_xywz, _xzyw, _xzwy, _xwyz, _xwzy, _yxzw , _yxwz, _yzxw, _yzwx, _ywxz
  , _ywzx, _zxyw, _zxwy, _zyxw, _zywx, _zwxy, _zwyx, _wxyz, _wxzy, _wyxz
  , _wyzx, _wzxy, _wzyx :: (R4 t, Elt a) => Lens' (Exp (t a)) (Exp (V4 a))
_xywz f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V4 a b d c)) <&> lift1 (\(V4 a' b' d' c') -> V4 a' b' c' d')
_xzyw f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V4 a c b d)) <&> lift1 (\(V4 a' c' b' d') -> V4 a' b' c' d')
_xzwy f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V4 a c d b)) <&> lift1 (\(V4 a' c' d' b') -> V4 a' b' c' d')
_xwyz f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V4 a d b c)) <&> lift1 (\(V4 a' d' b' c') -> V4 a' b' c' d')
_xwzy f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V4 a d c b)) <&> lift1 (\(V4 a' d' c' b') -> V4 a' b' c' d')
_yxzw f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V4 b a c d)) <&> lift1 (\(V4 b' a' c' d') -> V4 a' b' c' d')
_yxwz f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V4 b a d c)) <&> lift1 (\(V4 b' a' d' c') -> V4 a' b' c' d')
_yzxw f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V4 b c a d)) <&> lift1 (\(V4 b' c' a' d') -> V4 a' b' c' d')
_yzwx f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V4 b c d a)) <&> lift1 (\(V4 b' c' d' a') -> V4 a' b' c' d')
_ywxz f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V4 b d a c)) <&> lift1 (\(V4 b' d' a' c') -> V4 a' b' c' d')
_ywzx f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V4 b d c a)) <&> lift1 (\(V4 b' d' c' a') -> V4 a' b' c' d')
_zxyw f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V4 c a b d)) <&> lift1 (\(V4 c' a' b' d') -> V4 a' b' c' d')
_zxwy f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V4 c a d b)) <&> lift1 (\(V4 c' a' d' b') -> V4 a' b' c' d')
_zyxw f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V4 c b a d)) <&> lift1 (\(V4 c' b' a' d') -> V4 a' b' c' d')
_zywx f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V4 c b d a)) <&> lift1 (\(V4 c' b' d' a') -> V4 a' b' c' d')
_zwxy f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V4 c d a b)) <&> lift1 (\(V4 c' d' a' b') -> V4 a' b' c' d')
_zwyx f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V4 c d b a)) <&> lift1 (\(V4 c' d' b' a') -> V4 a' b' c' d')
_wxyz f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V4 d a b c)) <&> lift1 (\(V4 d' a' b' c') -> V4 a' b' c' d')
_wxzy f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V4 d a c b)) <&> lift1 (\(V4 d' a' c' b') -> V4 a' b' c' d')
_wyxz f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V4 d b a c)) <&> lift1 (\(V4 d' b' a' c') -> V4 a' b' c' d')
_wyzx f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V4 d b c a)) <&> lift1 (\(V4 d' b' c' a') -> V4 a' b' c' d')
_wzxy f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V4 d c a b)) <&> lift1 (\(V4 d' c' a' b') -> V4 a' b' c' d')
_wzyx f = _xyzw $ \(unlift -> V4 a b c d) -> f (lift (V4 d c b a)) <&> lift1 (\(V4 d' c' b' a') -> V4 a' b' c' d')

ew :: R4 t => E t
ew = E _w


-- Instances
-- ---------

instance Metric V4
instance Additive V4

instance R1 V4 where
  _x f (unlift -> V4 a b c d) = lift . (\a' -> V4 a' b c d) <$> f a

instance R2 V4 where
  _y  f (unlift -> V4 a b c d) = lift. (\b' -> V4 a b' c d) <$> f b
  _xy f (unlift -> V4 a b c d) = lift1 (\(V2 a' b') -> V4 a' b' c d) <$> f (lift (V2 a b))

instance R3 V4 where
  _z   f (unlift -> V4 a b c d) = lift. (\c' -> V4 a b c' d) <$> f c
  _xyz f (unlift -> V4 a b c d) = lift1 (\(V3 a' b' c') -> V4 a' b' c' d) <$> f (lift (V3 a b c))

instance R4 V4 where
  _w f (unlift -> V4 a b c d) = lift . V4 a b c <$> f d
  _xyzw = id

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

