{-# LANGUAGE ScopedTypeVariables #-}
module Linear.Accelerate
  (
  ) where

import Linear
import Data.Array.Accelerate
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Array.Sugar

type instance EltRepr (V0 a)  = ()
type instance EltRepr' (V0 a) = ()

instance Elt (V0 a) where
  eltType _ = eltType ()
  toElt V0 = ()
  fromElt () = V0

  eltType' _ = eltType' ()
  toElt' V0 = ()
  fromElt' () = V0

instance IsTuple (V0 a) where
  type TupleRepr (V0 a) = ()
  fromTuple V0 = ()
  toTuple () = V0

instance Lift Exp (V0 a) where
  type Plain (V0 a) = ()
  lift V0 = Exp (Tuple NilTup)

instance Unlift Exp (V0 a) where
  unlift t = V0

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

instance Lift Exp a => Lift Exp (V1 a) where
  type Plain (V1 a) = V1 (Plain a)
  lift (V1 a) = lift a

instance Unlift Exp a => Unlift Exp (V1 a) where
  unlift = V1 . unlift

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
