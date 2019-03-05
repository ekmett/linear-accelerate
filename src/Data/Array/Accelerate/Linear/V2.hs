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
-- Module      : Data.Array.Accelerate.Linear.V2
-- Copyright   : 2014 Edward Kmett, Charles Durham,
--               [2015..2018] Trevor L. McDonell
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
import Data.Array.Accelerate.Data.Functor       as A
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Array.Sugar

import Data.Array.Accelerate.Linear.Epsilon
import Data.Array.Accelerate.Linear.Lift
import Data.Array.Accelerate.Linear.Metric
import Data.Array.Accelerate.Linear.Type
import Data.Array.Accelerate.Linear.V1
import Data.Array.Accelerate.Linear.Vector

import Control.Lens
import Data.Function
import Linear.V2                                ( V2(..) )
import Prelude                                  as P
import qualified Linear.V2                      as L

-- $setup
-- >>> import Data.Array.Accelerate.Interpreter
-- >>> :{
--   let test :: Elt e => Exp e -> e
--       test e = indexArray (run (unit e)) Z
-- :}


-- | the counter-clockwise perpendicular vector
--
-- >>> test $ perp $ lift (V2 10 20 :: V2 Int)
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
  -- >>> test $ lift (V2 1 2 :: V2 Int) ^._y
  -- 2
  --
  -- >>> test $ lift (V2 1 2 :: V2 Int) & _y .~ 3
  -- V2 1 3
  --
  _y :: (Elt a, Box t a) => Lens' (Exp (t a)) (Exp a)
  _y = liftLens (L._y :: Lens' (t (Exp a)) (Exp a))

  _xy :: (Elt a, Box t a) => Lens' (Exp (t a)) (Exp (V2 a))
  _xy = liftLens (L._xy :: Lens' (t (Exp a)) (V2 (Exp a)))


-- |
-- >>> test $ lift (V2 1 2 :: V2 Int) ^. _yx
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

instance Elt a => Elt (V2 a) where
  type EltRepr (V2 a) = EltRepr (a, a)
  eltType = eltType @(a, a)
  toElt t = let (x, y) = toElt t in V2 x y
  fromElt (V2 x y) = fromElt (x, y)

instance cst a => IsProduct cst (V2 a)

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (V2 a) where
  type Plain (V2 a) = V2 (Plain a)
  lift (V2 x y) = Exp $ Tuple $ NilTup `SnocTup` lift x `SnocTup` lift y

instance Elt a => Unlift Exp (V2 (Exp a)) where
  unlift t = V2 (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                (Exp $ ZeroTupIdx `Prj` t)

instance (Elt a, Elt b) => Each (Exp (V2 a)) (Exp (V2 b)) (Exp a) (Exp b) where
  each = liftLens (each :: Traversal (V2 (Exp a)) (V2 (Exp b)) (Exp a) (Exp b))

instance A.Eq a => A.Eq (V2 a) where
  (==) = (A.==) `on` t2
  (/=) = (A./=) `on` t2

instance A.Ord a => A.Ord (V2 a) where
  (<)  = (A.<) `on` t2
  (>)  = (A.>) `on` t2
  (<=) = (A.<=) `on` t2
  (>=) = (A.>=) `on` t2
  min  = v2 $$ on A.min t2
  max  = v2 $$ on A.max t2

t2 :: Elt a => Exp (V2 a) -> Exp (a,a)
t2 (unlift -> V2 x y) = tup2 (x,y)

v2 :: Elt a => Exp (a,a) -> Exp (V2 a)
v2 (untup2 -> (x,y)) = lift (V2 x y)

instance A.Bounded a => P.Bounded (Exp (V2 a)) where
  minBound = lift (V2 (minBound :: Exp a) (minBound :: Exp a))
  maxBound = lift (V2 (maxBound :: Exp a) (maxBound :: Exp a))

instance A.Num a => P.Num (Exp (V2 a)) where
  (+)             = lift2 ((+) :: V2 (Exp a) -> V2 (Exp a) -> V2 (Exp a))
  (-)             = lift2 ((-) :: V2 (Exp a) -> V2 (Exp a) -> V2 (Exp a))
  (*)             = lift2 ((*) :: V2 (Exp a) -> V2 (Exp a) -> V2 (Exp a))
  negate          = lift1 (negate :: V2 (Exp a) -> V2 (Exp a))
  signum          = lift1 (signum :: V2 (Exp a) -> V2 (Exp a))
  abs             = lift1 (signum :: V2 (Exp a) -> V2 (Exp a))
  fromInteger x   = lift (P.fromInteger x :: V2 (Exp a))

instance A.Floating a => P.Fractional (Exp (V2 a)) where
  (/)             = lift2 ((/) :: V2 (Exp a) -> V2 (Exp a) -> V2 (Exp a))
  recip           = lift1 (recip :: V2 (Exp a) -> V2 (Exp a))
  fromRational x  = lift (P.fromRational x :: V2 (Exp a))

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

instance Epsilon a => Epsilon (V2 a) where
  nearZero = nearZero . quadrance

instance A.Functor V2 where
  fmap f (unlift -> V2 x y) = lift (V2 (f x) (f y))
  x <$ _                    = lift (V2 x x)

