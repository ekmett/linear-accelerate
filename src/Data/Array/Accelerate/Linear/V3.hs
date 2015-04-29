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
  R1(..), R2(..), R3(..),
  _yx,
  _xz, _yz, _zx, _zy,
  _xzy, _yxz, _yzx, _zxy, _zyx,
  ex, ey, ez,

) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Array.Sugar

import Data.Array.Accelerate.Linear.Metric
import Data.Array.Accelerate.Linear.V1
import Data.Array.Accelerate.Linear.V2
import Data.Array.Accelerate.Linear.Vector

import Control.Lens
import Linear.V3                                ( V3(..) )
import qualified Linear.V3                      as L


-- | cross product
--
cross :: (Elt a, IsNum a) => Exp (V3 a) -> Exp (V3 a) -> Exp (V3 a)
cross = lift2 L.cross

-- | scalar triple product
--
triple :: (Elt a, IsNum a) => Exp (V3 a) -> Exp (V3 a) -> Exp (V3 a) -> Exp a
triple a b c = dot a (cross b c)


-- | A space that distinguishes 3 orthogonal basis vectors: '_x', '_y', and
-- '_z'. (Although it may have more)
--
class R2 t => R3 t where
  -- |
  -- >>> V3 1 2 3 ^. _z
  -- 3
  --
  _z :: Elt a => Lens' (Exp (t a)) (Exp a)

  _xyz :: Elt a => Lens' (Exp (t a)) (Exp (V3 a))

_xz, _yz, _zx, _zy :: (R3 t, Elt a) => Lens' (Exp (t a)) (Exp (V2 a))
_xz f = _xyz $ \(unlift -> V3 a b c) -> f (lift (V2 a c)) <&> lift1 (\(V2 a' c') -> V3 a' b c')
_yz f = _xyz $ \(unlift -> V3 a b c) -> f (lift (V2 b c)) <&> lift1 (\(V2 b' c') -> V3 a b' c')
_zx f = _xyz $ \(unlift -> V3 a b c) -> f (lift (V2 c a)) <&> lift1 (\(V2 c' a') -> V3 a' b c')
_zy f = _xyz $ \(unlift -> V3 a b c) -> f (lift (V2 c b)) <&> lift1 (\(V2 c' b') -> V3 a b' c')

_xzy, _yxz, _yzx, _zxy, _zyx :: (R3 t, Elt a) => Lens' (Exp (t a)) (Exp (V3 a))
_xzy f = _xyz $ \(unlift -> V3 a b c) -> f (lift (V3 a c b)) <&> lift1 (\(V3 a' c' b') -> V3 a' b' c')
_yxz f = _xyz $ \(unlift -> V3 a b c) -> f (lift (V3 b a c)) <&> lift1 (\(V3 b' a' c') -> V3 a' b' c')
_yzx f = _xyz $ \(unlift -> V3 a b c) -> f (lift (V3 b c a)) <&> lift1 (\(V3 b' c' a') -> V3 a' b' c')
_zxy f = _xyz $ \(unlift -> V3 a b c) -> f (lift (V3 c a b)) <&> lift1 (\(V3 c' a' b') -> V3 a' b' c')
_zyx f = _xyz $ \(unlift -> V3 a b c) -> f (lift (V3 c b a)) <&> lift1 (\(V3 c' b' a') -> V3 a' b' c')

ez :: R3 t => E t
ez = E _z


-- Instances
-- ---------

instance Metric V3
instance Additive V3

instance R1 V3 where
  _x f (unlift -> V3 a b c) = (\a' -> lift $ V3 a' b c) <$> f a

instance R2 V3 where
  _y  f (unlift -> V3 a b c) = lift. (\b' -> V3 a b' c) <$> f b
  _xy f (unlift -> V3 a b c) = lift1 (\(V2 a' b') -> V3 a' b' c) <$> f (lift $ V2 a b)

instance R3 V3 where
  _z f (unlift -> V3 a b c) = lift . V3 a b <$> f c
  _xyz = id

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
{--
type instance ArrRepr (V3 a) = ArrRepr (Vector a)

instance Elt a => Arrays (V3 a) where
  arrays _ = arrays (undefined :: Vector a)
  toArr arr = case toList arr of
    [a,b,c] -> V3 a b c
    _       -> error "shape mismatch"
  fromArr = fromList (Z :. 3) . F.toList
  flavour _ = error "https://github.com/AccelerateHS/accelerate/issues/263"

instance Elt a => Lift Acc (V3 a) where
  type Plain (V3 a) = Vector a
  lift = lift . toArr'
--}

