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
  R1(..),
  R2(..),
  _yx,
  R3(..),
  _xz, _yz, _zx, _zy,
  _xzy, _yxz, _yzx, _zxy, _zyx,
  ex, ey, ez,

) where

import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Array.Sugar

import Data.Array.Accelerate.Linear.Lift
import Data.Array.Accelerate.Linear.Metric
import Data.Array.Accelerate.Linear.Type
import Data.Array.Accelerate.Linear.V1
import Data.Array.Accelerate.Linear.V2
import Data.Array.Accelerate.Linear.Vector

import Control.Lens
import Linear.V3                                ( V3(..) )
import qualified Linear.V3                      as L
import Prelude                                  as P


-- | cross product
--
cross :: forall a. A.Num a => Exp (V3 a) -> Exp (V3 a) -> Exp (V3 a)
cross = lift2 (L.cross :: V3 (Exp a) -> V3 (Exp a) -> V3 (Exp a))

-- | scalar triple product
--
triple :: forall a. A.Num a => Exp (V3 a) -> Exp (V3 a) -> Exp (V3 a) -> Exp a
triple = lift3 (L.triple :: V3 (Exp a) -> V3 (Exp a) -> V3 (Exp a) -> Exp a)


-- | A space that distinguishes 3 orthogonal basis vectors: '_x', '_y', and '_z'.
-- (Although it may have more)
--
class (L.R3 t, R2 t) => R3 t where
  -- |
  -- >>> lift (V3 1 2 3 :: V3 Int) ^. _z
  -- 3
  --
  _z :: forall a. (Elt a, Box t a) => Lens' (Exp (t a)) (Exp a)
  _z = liftLens (L._z :: Lens' (t (Exp a)) (Exp a))

  _xyz :: forall a. (Elt a, Box t a) => Lens' (Exp (t a)) (Exp (V3 a))
  _xyz = liftLens (L._xyz :: Lens' (t (Exp a)) (V3 (Exp a)))


_xz, _yz, _zx, _zy :: forall t a. (R3 t, Elt a, Box t a) => Lens' (Exp (t a)) (Exp (V2 a))
_xz = liftLens (L._xz :: Lens' (t (Exp a)) (V2 (Exp a)))
_yz = liftLens (L._yz :: Lens' (t (Exp a)) (V2 (Exp a)))
_zx = liftLens (L._zx :: Lens' (t (Exp a)) (V2 (Exp a)))
_zy = liftLens (L._zy :: Lens' (t (Exp a)) (V2 (Exp a)))

_xzy, _yxz, _yzx, _zxy, _zyx :: forall t a. (R3 t, Elt a, Box t a) => Lens' (Exp (t a)) (Exp (V3 a))
_xzy = liftLens (L._xzy :: Lens' (t (Exp a)) (V3 (Exp a)))
_yxz = liftLens (L._yxz :: Lens' (t (Exp a)) (V3 (Exp a)))
_yzx = liftLens (L._yzx :: Lens' (t (Exp a)) (V3 (Exp a)))
_zxy = liftLens (L._zxy :: Lens' (t (Exp a)) (V3 (Exp a)))
_zyx = liftLens (L._zyx :: Lens' (t (Exp a)) (V3 (Exp a)))


ez :: R3 t => E t
ez = E _z


-- Instances
-- ---------

instance Metric V3
instance Additive V3
instance R1 V3
instance R2 V3
instance R3 V3

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
  lift (V3 x y z) = Exp $ Tuple $ NilTup `SnocTup` lift x `SnocTup` lift y `SnocTup` lift z

instance Elt a => Unlift Exp (V3 (Exp a)) where
  unlift t = V3 (Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t)
                (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                (Exp $ ZeroTupIdx `Prj` t)

instance A.Num a => P.Num (Exp (V3 a)) where
  (+)             = lift2 ((+) :: V3 (Exp a) -> V3 (Exp a) -> V3 (Exp a))
  (-)             = lift2 ((-) :: V3 (Exp a) -> V3 (Exp a) -> V3 (Exp a))
  (*)             = lift2 ((*) :: V3 (Exp a) -> V3 (Exp a) -> V3 (Exp a))
  negate          = lift1 (negate :: V3 (Exp a) -> V3 (Exp a))
  signum          = lift1 (signum :: V3 (Exp a) -> V3 (Exp a))
  abs             = lift1 (signum :: V3 (Exp a) -> V3 (Exp a))
  fromInteger x   = lift (fromInteger x :: V3 (Exp a))

instance A.Floating a => P.Fractional (Exp (V3 a)) where
  (/)             = lift2 ((/) :: V3 (Exp a) -> V3 (Exp a) -> V3 (Exp a))
  recip           = lift1 (recip :: V3 (Exp a) -> V3 (Exp a))
  fromRational x  = lift (fromRational x :: V3 (Exp a))

instance A.Floating a => P.Floating (Exp (V3 a)) where
  pi              = lift (pi :: V3 (Exp a))
  log             = lift1 (log :: V3 (Exp a) -> V3 (Exp a))
  exp             = lift1 (exp :: V3 (Exp a) -> V3 (Exp a))
  sin             = lift1 (sin :: V3 (Exp a) -> V3 (Exp a))
  cos             = lift1 (cos :: V3 (Exp a) -> V3 (Exp a))
  tan             = lift1 (tan :: V3 (Exp a) -> V3 (Exp a))
  sinh            = lift1 (sinh :: V3 (Exp a) -> V3 (Exp a))
  cosh            = lift1 (cosh :: V3 (Exp a) -> V3 (Exp a))
  tanh            = lift1 (tanh :: V3 (Exp a) -> V3 (Exp a))
  asin            = lift1 (asin :: V3 (Exp a) -> V3 (Exp a))
  acos            = lift1 (acos :: V3 (Exp a) -> V3 (Exp a))
  atan            = lift1 (atan :: V3 (Exp a) -> V3 (Exp a))
  asinh           = lift1 (asinh :: V3 (Exp a) -> V3 (Exp a))
  acosh           = lift1 (acosh :: V3 (Exp a) -> V3 (Exp a))
  atanh           = lift1 (atanh :: V3 (Exp a) -> V3 (Exp a))

instance (Elt a, Elt b) => Each (Exp (V3 a)) (Exp (V3 b)) (Exp a) (Exp b) where
  each = liftLens (each :: Traversal (V3 (Exp a)) (V3 (Exp b)) (Exp a) (Exp b))

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

