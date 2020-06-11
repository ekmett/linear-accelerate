{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Accelerate.Linear.V3
-- Copyright   : 2014 Edward Kmett, Charles Durham,
--               [2015..2018] Trevor L. McDonell
-- License     : BSD-style (see the file LICENSE)
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- 3-D Vectors
----------------------------------------------------------------------------

module Data.Array.Accelerate.Linear.V3 (

  V3(..), pattern V3_,
  cross, triple,
  R1(..),
  R2(..),
  _yx,
  R3(..),
  _xz, _yz, _zx, _zy,
  _xzy, _yxz, _yzx, _zxy, _zyx,
  ex, ey, ez,

) where

import Data.Array.Accelerate                    as A hiding ( pattern V2, pattern V3 )
import Data.Array.Accelerate.Data.Functor       as A
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Array.Sugar

import Data.Array.Accelerate.Linear.Epsilon
import Data.Array.Accelerate.Linear.Lift
import Data.Array.Accelerate.Linear.Metric
import Data.Array.Accelerate.Linear.Type
import Data.Array.Accelerate.Linear.V1
import Data.Array.Accelerate.Linear.V2
import Data.Array.Accelerate.Linear.Vector

import Control.Lens
import Data.Function
import Linear.V3                                ( V3(..) )
import Prelude                                  as P
import qualified Linear.V3                      as L

-- $setup
-- >>> import Data.Array.Accelerate.Interpreter
-- >>> :{
--   let test :: Elt e => Exp e -> e
--       test e = indexArray (run (unit e)) Z
-- :}


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
  -- >>> test $ lift (V3 1 2 3 :: V3 Int) ^. _z
  -- 3
  --
  -- >>> test $ lift (V3 1 2 3 :: V3 Int) & _z .~ 42
  -- V3 1 2 42
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

pattern V3_ :: Elt a => Exp a -> Exp a -> Exp a -> Exp (V3 a)
pattern V3_ x y z = Pattern (x,y,z)
{-# COMPLETE V3_ #-}

instance Metric V3
instance Additive V3
instance R1 V3
instance R2 V3
instance R3 V3
instance Elt a => Elt (V3 a)

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (V3 a) where
  type Plain (V3 a) = V3 (Plain a)
  lift (V3 x y z) = V3_ (lift x) (lift y) (lift z)

instance Elt a => Unlift Exp (V3 (Exp a)) where
  unlift (V3_ x y z) = V3 x y z

instance (Elt a, Elt b) => Each (Exp (V3 a)) (Exp (V3 b)) (Exp a) (Exp b) where
  each = liftLens (each :: Traversal (V3 (Exp a)) (V3 (Exp b)) (Exp a) (Exp b))

instance A.Eq a => A.Eq (V3 a) where
  (==) = (A.==) `on` t3
  (/=) = (A./=) `on` t3

instance A.Ord a => A.Ord (V3 a) where
  (<)  = (A.<) `on` t3
  (>)  = (A.>) `on` t3
  (<=) = (A.<=) `on` t3
  (>=) = (A.>=) `on` t3
  min  = v3 $$ on A.min t3
  max  = v3 $$ on A.max t3

t3 :: Exp (V3 a) -> Exp (a, a, a)
t3 (Exp e) = Exp e

v3 :: Exp (a, a, a) -> Exp (V3 a)
v3 (Exp e) = Exp e

instance A.Bounded a => P.Bounded (Exp (V3 a)) where
  minBound = lift (V3 (minBound :: Exp a) (minBound :: Exp a) (minBound :: Exp a))
  maxBound = lift (V3 (maxBound :: Exp a) (maxBound :: Exp a) (maxBound :: Exp a))

instance A.Num a => P.Num (Exp (V3 a)) where
  (+)             = lift2 ((+) :: V3 (Exp a) -> V3 (Exp a) -> V3 (Exp a))
  (-)             = lift2 ((-) :: V3 (Exp a) -> V3 (Exp a) -> V3 (Exp a))
  (*)             = lift2 ((*) :: V3 (Exp a) -> V3 (Exp a) -> V3 (Exp a))
  negate          = lift1 (negate :: V3 (Exp a) -> V3 (Exp a))
  signum          = lift1 (signum :: V3 (Exp a) -> V3 (Exp a))
  abs             = lift1 (signum :: V3 (Exp a) -> V3 (Exp a))
  fromInteger x   = lift (P.fromInteger x :: V3 (Exp a))

instance A.Floating a => P.Fractional (Exp (V3 a)) where
  (/)             = lift2 ((/) :: V3 (Exp a) -> V3 (Exp a) -> V3 (Exp a))
  recip           = lift1 (recip :: V3 (Exp a) -> V3 (Exp a))
  fromRational x  = lift (P.fromRational x :: V3 (Exp a))

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

instance Epsilon a => Epsilon (V3 a) where
  nearZero = nearZero . quadrance

instance A.Functor V3 where
  fmap f (unlift -> V3 x y z) = lift (V3 (f x) (f y) (f z))
  x <$ _                      = lift (V3 x x x)

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

