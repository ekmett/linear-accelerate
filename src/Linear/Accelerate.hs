{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Linear.Accelerate () where

import Data.Array.Accelerate
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Data.Complex ()
import qualified Data.Foldable as F
import Linear
--import Linear.Plucker (Plucker(..))

--------------------------------------------------------------------------------
-- * V0
--------------------------------------------------------------------------------

type instance EltRepr (V0 a) = ()

instance Elt a => Elt (V0 a) where
  eltType _ = eltType ()
  toElt () = V0
  fromElt V0 = ()

instance IsProduct cst (V0 a) where
  type ProdRepr (V0 a) = ()
  fromProd _ V0 = ()
  toProd _ () = V0
  prod _ _ = ProdRunit

instance Lift Exp (V0 a) where
  type Plain (V0 a) = ()
  lift V0 = Exp (Tuple NilTup)

instance Unlift Exp (V0 a) where
  unlift _ = V0

--------------------------------------------------------------------------------
-- * V1
--------------------------------------------------------------------------------

type instance EltRepr (V1 a) = EltRepr a

instance Elt a => Elt (V1 a) where
  eltType _ = eltType (undefined :: a)
  toElt = V1 . toElt
  fromElt (V1 a) = fromElt a

instance cst a => IsProduct cst (V1 a) where
  type ProdRepr (V1 a) = ((), a)
  fromProd _ (V1 x) = ((), x)
  toProd _ ((), x) = V1 x
  prod _ _ = ProdRsnoc ProdRunit

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (V1 a) where
  type Plain (V1 a) = V1 (Plain a)
  lift (V1 x) = Exp . Tuple $ NilTup `SnocTup` lift x

instance (Elt a, e ~ Exp a) => Unlift Exp (V1 e) where
  unlift t = V1 $ Exp $ ZeroTupIdx `Prj` t

--------------------------------------------------------------------------------
-- * V2
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- * V3
--------------------------------------------------------------------------------

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

type instance ArrRepr (V3 a) = ArrRepr (Vector a)

instance Elt a => Arrays (V3 a) where
  arrays _ = arrays (undefined :: Vector a)
  toArr arr = case toList arr of
    [a,b,c] -> V3 a b c
    _       -> error "shape mismatch"
  fromArr = fromList (Z :. 3) . F.toList

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

{-
instance Elt a => Lift Acc (V3 a) where
  type Plain (V3 a) = Vector a
  lift = lift . toArr'
-}

--------------------------------------------------------------------------------
-- * V4
--------------------------------------------------------------------------------

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
  lift (V4 x y z w) = Exp $ Tuple $ NilTup `SnocTup`
                      lift x `SnocTup`
                      lift y `SnocTup`
                      lift z `SnocTup`
                      lift w

instance (Elt a, e ~ Exp a) => Unlift Exp (V4 e) where
  unlift t = V4 (Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` t)
                (Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t)
                (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                (Exp $ ZeroTupIdx `Prj` t)

--------------------------------------------------------------------------------
-- * Quaternion
--------------------------------------------------------------------------------

type instance EltRepr (Quaternion a) = EltRepr (a, a, a, a)

instance Elt a => Elt (Quaternion a) where
  eltType _ = eltType (undefined :: (a,a,a,a))
  toElt p = case toElt p of
     (x, y, z, w) -> Quaternion x (V3 y z w)
  fromElt (Quaternion x (V3 y z w)) = fromElt (x, y, z, w)

instance cst a => IsProduct cst (Quaternion a) where
  type ProdRepr (Quaternion a) = ProdRepr (a,a,a,a)
  fromProd p (Quaternion x (V3 y z w)) = fromProd p (x,y,z,w)
  toProd p t = case toProd p t of
     (x, y, z, w) -> Quaternion x (V3 y z w)
  prod p _ = prod p (undefined :: (a,a,a,a))

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Quaternion a) where
  type Plain (Quaternion a) = Quaternion (Plain a)
  --lift = Exp . Tuple . F.foldl SnocTup NilTup
  lift (Quaternion x (V3 y z w)) = Exp $ Tuple $ NilTup `SnocTup`
                                   lift x `SnocTup`
                                   lift y `SnocTup`
                                   lift z `SnocTup`
                                   lift w

instance (Elt a, e ~ Exp a) => Unlift Exp (Quaternion e) where
  unlift t = Quaternion (Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` t)
                    (V3 (Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t)
                        (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                        (Exp $ ZeroTupIdx `Prj` t))

--------------------------------------------------------------------------------
-- * Plücker
--------------------------------------------------------------------------------
{-
type instance EltRepr (Plucker a) = EltRepr (a, a, a, a, a, a)

instance Elt a => Elt (Plucker a) where
  eltType _ = eltType (undefined :: (a,a,a,a,a,a))
  toElt p = case toElt p of
     (x, y, z, w, u, v) -> Plucker x y z w u v
  fromElt (Plucker x y z w u v) = fromElt (x, y, z, w, u, v)

instance IsProduct cst (Plucker a) where
  type ProdRepr (Plucker a) = ProdRepr (a,a,a,a)
  fromProd (Plucker x y z w u v) = fromProd (x, y, z, w, u, v)
  toProd t = case toProd t of
     (x, y, z, w, u, v) -> Plucker x y z w u v

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Plucker a) where
  type Plain (Plucker a) = Plucker (Plain a)
  lift = Exp . Tuple . F.foldl SnocTup NilTup

instance (Elt a, e ~ Exp a) => Unlift Exp (Plucker e) where
  unlift t = Plucker
    (Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))) `Prj` t)
    (Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Prj` t)
    (Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` t)
    (Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t)
    (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
    (Exp $ ZeroTupIdx `Prj` t)
-}

