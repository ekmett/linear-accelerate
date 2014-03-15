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
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Array.Sugar
import Linear

{- V0 Accelerate Instances -}

type instance EltRepr (V0 a)  = ()
type instance EltRepr' (V0 a) = ()

instance Elt a => Elt (V0 a) where
  eltType _ = eltType ()
  toElt () = V0
  fromElt V0 = ()

  eltType' _ = eltType' ()
  toElt' () = V0
  fromElt' V0 = ()

instance IsTuple (V0 a) where
  type TupleRepr (V0 a) = ()
  fromTuple V0 = ()
  toTuple () = V0

instance Lift Exp (V0 a) where
  type Plain (V0 a) = ()
  lift V0 = Exp (Tuple NilTup)

instance Unlift Exp (V0 a) where
  unlift _ = V0

{- V1 Accelerate Instances -}

type instance EltRepr (V1 a) = EltRepr a
type instance EltRepr' (V1 a) = EltRepr' a

instance Elt a => Elt (V1 a) where
  eltType _ = eltType (undefined :: a)
  toElt = V1 . toElt
  fromElt (V1 a) = fromElt a

  eltType' _ = eltType' (undefined :: a)
  toElt' = V1 . toElt'
  fromElt' (V1 a) = fromElt' a

instance IsTuple (V1 a) where
  type TupleRepr (V1 a) = ((), a)
  fromTuple (V1 x) = ((), x)
  toTuple ((), x) = V1 x

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (V1 a) where
  type Plain (V1 a) = V1 (Plain a)
  lift (V1 x) = Exp . Tuple $ NilTup `SnocTup` lift x

instance (Elt a, e ~ Exp a) => Unlift Exp (V1 e) where
  unlift t = V1 $ Exp $ ZeroTupIdx `Prj` t

{- V2 Accelerate Instances -}

type instance EltRepr (V2 a)  = EltRepr (a, a)
type instance EltRepr' (V2 a) = EltRepr' (a, a)

instance Elt a => Elt (V2 a) where
  eltType _ = eltType (undefined :: (a,a))
  toElt p = case toElt p of
     (x, y) -> V2 x y
  fromElt (V2 x y) = fromElt (x, y)

  eltType' _ = eltType' (undefined :: (a,a))
  toElt' p = case toElt' p of
     (x, y) -> V2 x y
  fromElt' (V2 x y) = fromElt' (x, y)

instance IsTuple (V2 a) where
  type TupleRepr (V2 a) = TupleRepr (a,a)
  fromTuple (V2 x y) = fromTuple (x,y)
  toTuple t = case toTuple t of
     (x, y) -> V2 x y

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (V2 a) where
  type Plain (V2 a) = V2 (Plain a)
  lift (V2 x y) = Exp $ Tuple $ NilTup `SnocTup` lift x `SnocTup` lift y

instance (Elt a, e ~ Exp a) => Unlift Exp (V2 e) where
  unlift t = V2 (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                (Exp $ ZeroTupIdx `Prj` t)

{- V3 Accelerate Instances -}

type instance EltRepr (V3 a)  = EltRepr (a, a, a)
type instance EltRepr' (V3 a) = EltRepr' (a, a, a)

instance Elt a => Elt (V3 a) where
  eltType _ = eltType (undefined :: (a,a,a))
  toElt p = case toElt p of
     (x, y, z) -> V3 x y z
  fromElt (V3 x y z) = fromElt (x, y, z)

  eltType' _ = eltType' (undefined :: (a,a,a))
  toElt' p = case toElt' p of
     (x, y, z) -> V3 x y z
  fromElt' (V3 x y z) = fromElt' (x, y, z)

instance IsTuple (V3 a) where
  type TupleRepr (V3 a) = TupleRepr (a,a,a)
  fromTuple (V3 x y z) = fromTuple (x,y,z)
  toTuple t = case toTuple t of
     (x, y, z) -> V3 x y z

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (V3 a) where
  type Plain (V3 a) = V3 (Plain a)
  lift (V3 x y z) = Exp $ Tuple $ NilTup `SnocTup` lift x `SnocTup` lift y `SnocTup` lift z

instance (Elt a, e ~ Exp a) => Unlift Exp (V3 e) where
  unlift t = V3 (Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t)
                (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                (Exp $ ZeroTupIdx `Prj` t)

{- V4 Accelerate Instances -}

type instance EltRepr (V4 a)  = EltRepr (a, a, a, a)
type instance EltRepr' (V4 a) = EltRepr' (a, a, a, a)

instance Elt a => Elt (V4 a) where
  eltType _ = eltType (undefined :: (a,a,a,a))
  toElt p = case toElt p of
     (x, y, z, w) -> V4 x y z w
  fromElt (V4 x y z w) = fromElt (x, y, z, w)

  eltType' _ = eltType' (undefined :: (a,a,a,a))
  toElt' p = case toElt' p of
     (x, y, z, w) -> V4 x y z w
  fromElt' (V4 x y z w) = fromElt' (x, y, z, w)

instance IsTuple (V4 a) where
  type TupleRepr (V4 a) = TupleRepr (a,a,a,a)
  fromTuple (V4 x y z w) = fromTuple (x,y,z,w)
  toTuple t = case toTuple t of
     (x, y, z, w) -> V4 x y z w

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (V4 a) where
  type Plain (V4 a) = V4 (Plain a)
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

type instance EltRepr (Quaternion a)  = EltRepr (a, a, a, a)
type instance EltRepr' (Quaternion a) = EltRepr' (a, a, a, a)

instance Elt a => Elt (Quaternion a) where
  eltType _ = eltType (undefined :: (a,a,a,a))
  toElt p = case toElt p of
     (x, y, z, w) -> Quaternion x (V3 y z w)
  fromElt (Quaternion x (V3 y z w)) = fromElt (x, y, z, w)

  eltType' _ = eltType' (undefined :: (a,a,a,a))
  toElt' p = case toElt' p of
     (x, y, z, w) -> Quaternion x (V3 y z w)
  fromElt' (Quaternion x (V3 y z w)) = fromElt' (x, y, z, w)

instance IsTuple (Quaternion a) where
  type TupleRepr (Quaternion a) = TupleRepr (a,a,a,a)
  fromTuple (Quaternion x (V3 y z w)) = fromTuple (x,y,z,w)
  toTuple t = case toTuple t of
     (x, y, z, w) -> Quaternion x (V3 y z w)

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Quaternion a) where
  type Plain (Quaternion a) = Quaternion (Plain a)
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

