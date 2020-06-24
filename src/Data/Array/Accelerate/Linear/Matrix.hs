{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Accelerate.Linear.Matrix
-- Copyright   : 2014 Edward Kmett, Charles Durham,
--               [2015..2018] Trevor L. McDonell
-- License     : BSD-style (see the file LICENSE)
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Simple matrix operations for low-dimensional primitives
----------------------------------------------------------------------------

module Data.Array.Accelerate.Linear.Matrix (

  (!*!), (!+!), (!-!), (!*), (*!), (!!*), (*!!), (!!/),
  M22, M23, M24, M32, M33, M34, M42, M43, M44,
  m33_to_m44, m43_to_m44,
  det22, det33, det44,
  inv22, inv33, inv44,
  identity,
  transpose,
  Trace(..),
  fromQuaternion,
  mkTransformation,
  mkTransformationMat,

) where

import Data.Array.Accelerate                    as A
                                         hiding ( transpose
                                                , pattern V2
                                                , pattern V3
                                                , pattern V4
                                                )

import Data.Array.Accelerate.Linear.Lift
import Data.Array.Accelerate.Linear.Trace
import Data.Array.Accelerate.Linear.Type
import Data.Array.Accelerate.Linear.Quaternion
import Data.Array.Accelerate.Linear.V3
import Data.Array.Accelerate.Linear.Vector

import Linear.Matrix                            ( M22, M23, M24, M32, M33, M34, M42, M43, M44 )
import qualified Linear.Matrix                  as L

import Data.Distributive
import Data.Foldable
import Data.Traversable
import Control.Applicative
import Prelude                                  as P

-- $setup
-- >>> import Data.Array.Accelerate.Interpreter
-- >>> import Data.Array.Accelerate.Linear.V2
-- >>> import Data.Array.Accelerate.Linear.V3
-- >>> import Data.Array.Accelerate.Linear.V4
-- >>> :{
--   let test :: Elt e => Exp e -> e
--       test e = indexArray (run (unit e)) Z
-- :}


infixl 7 !*!
-- | Matrix product. This can compute any combination of sparse and dense multiplication.
--
-- >>> test $ (V2_ (V3_ 1 2 3) (V3_ 4 5 6) :: Exp (M23 Int)) !*! (V3_ (V2_ 1 2) (V2_ 3 4) (V2_ 4 5) :: Exp (M32 Int))
-- V2 (V2 19 25) (V2 43 58)
--
(!*!) :: (Functor m, Foldable t, Additive t, Additive n, A.Num a, Box2 m t a, Box2 t n a, Box2 m n a)
      => Exp (m (t a))
      -> Exp (t (n a))
      -> Exp (m (n a))
f !*! g = lift (unlift' f L.!*! unlift' g)


infixl 6 !+!
-- | Entry-wise matrix addition.
--
-- >>> test $ (V2_ (V3_ 1 2 3) (V3_ 4 5 6) :: Exp (M23 Int)) !+! (V2_ (V3_ 7 8 9) (V3_ 1 2 3) :: Exp (M23 Int))
-- V2 (V3 8 10 12) (V3 5 7 9)
--
(!+!) :: (Additive m, Additive n, A.Num a, Box2 m n a)
      => Exp (m (n a))
      -> Exp (m (n a))
      -> Exp (m (n a))
f !+! g = lift (unlift' f L.!+! unlift' g)


infixl 6 !-!
-- | Entry-wise matrix subtraction.
--
-- >>> test $ (V2_ (V3_ 1 2 3) (V3_ 4 5 6) :: Exp (M23 Int)) !-! (V2_ (V3_ 7 8 9) (V3_ 1 2 3) :: Exp (M23 Int))
-- V2 (V3 (-6) (-6) (-6)) (V3 3 3 3)
--
(!-!) :: (Additive m, Additive n, A.Num a, Box2 m n a)
      => Exp (m (n a))
      -> Exp (m (n a))
      -> Exp (m (n a))
f !-! g = lift (unlift' f L.!-! unlift' g)


infixl 7 !*
-- | Matrix * column vector
--
-- >>> test $ (V2_ (V3_ 1 2 3) (V3_ 4 5 6) :: Exp (M23 Int)) !* (V3_ 7 8 9 :: Exp (V3 Int))
-- V2 50 122
--
(!*) :: (Functor m, Foldable r, Additive r, A.Num a, Box2 m r a, Box m a)
     => Exp (m (r a))
     -> Exp (r a)
     -> Exp (m a)
m !* v = lift (unlift' m L.!* unlift v)


infixl 7 *!
-- | Row vector * matrix
--
-- >>> test $ (V2_ 1 2 :: Exp (V2 Int)) *! (V2_ (V3_ 3 4 5) (V3_ 6 7 8) :: Exp (M23 Int))
-- V3 15 18 21

-- (*!) :: (Metric r, Additive n, Num a) => r a -> r (n a) -> n a
-- f *! g = dot f <$> distribute g
--
(*!) :: (Foldable t, Additive f, Additive t, A.Num a, Box t a, Box f a, Box2 t f a)
     => Exp (t a)
     -> Exp (t (f a))
     -> Exp (f a)
f *! g = lift (unlift f L.*! unlift' g)


infixl 7 *!!
-- | Scalar-matrix product
--
-- >>> test $ 5 *!! (V2_ (V2_ 1 2) (V2_ 3 4) :: Exp (M22 Int))
-- V2 (V2 5 10) (V2 15 20)
--
(*!!) :: (Functor m, Functor r, A.Num a, Box2 m r a)
      => Exp a
      -> Exp (m (r a))
      -> Exp (m (r a))
s *!! m = lift (unlift s L.*!! unlift' m)


infixl 7 !!*
-- | Matrix-scalar product
--
-- >>> test $ (V2_ (V2_ 1 2) (V2_ 3 4) :: Exp (M22 Int)) !!* 5
-- V2 (V2 5 10) (V2 15 20)
--
(!!*) :: (Functor m, Functor r, A.Num a, Box2 m r a)
      => Exp (m (r a))
      -> Exp a
      -> Exp (m (r a))
(!!*) = flip (*!!)


infixl 7 !!/
-- | Matrix-scalar division
--
(!!/) :: (Functor m, Functor r, A.Floating a, Box2 m r a)
      => Exp (m (r a))
      -> Exp a
      -> Exp (m (r a))
m !!/ s = lift (unlift' m L.!!/ unlift s)


-- |The identity matrix for any dimension vector.
--
-- >>> test $ (identity :: Exp (M44 Int))
-- V4 (V4 1 0 0 0) (V4 0 1 0 0) (V4 0 0 1 0) (V4 0 0 0 1)
--
-- >>> test $ (identity :: Exp (V3 (V3 Int)))
-- V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)
--
identity :: forall t a. (Traversable t, Applicative t, A.Num a, Box2 t t a) => Exp (t (t a))
identity = lift (L.identity :: t (t (Exp a)))


-- | 'transpose' is just an alias for 'distribute'
--
-- >>> test $ transpose $ (V3_ (V2_ 1 2) (V2_ 3 4) (V2_ 5 6) :: Exp (M32 Int))
-- V2 (V3 1 3 5) (V3 2 4 6)
--
transpose
    :: (Distributive g, Functor f, Box2 f g a, Box2 g f a)
    => Exp (f (g a))
    -> Exp (g (f a))
transpose = lift . L.transpose . unlift'


-- | Build a rotation matrix from a unit 'Quaternion'
--
fromQuaternion :: forall a. A.Num a => Exp (Quaternion a) -> Exp (M33 a)
fromQuaternion = lift1 (L.fromQuaternion :: Quaternion (Exp a) -> M33 (Exp a))

-- | Build a transformation matrix from a rotation expressed as a 'Quaternion'
-- and a translation vector.
--
mkTransformation :: forall a. A.Num a => Exp (Quaternion a) -> Exp (V3 a) -> Exp (M44 a)
mkTransformation = lift2 (L.mkTransformation :: Quaternion (Exp a) -> V3 (Exp a) -> M44 (Exp a))

-- | Build a transformation matrix from a rotation matrix and a translation
-- vector.
--
mkTransformationMat :: A.Num a => Exp (M33 a) -> Exp (V3 a) -> Exp (M44 a)
mkTransformationMat m v =
  let r = L.mkTransformationMat (unlift' m) (unlift v)
  in  lift r

-- | Convert a 4x3 matrix to a 4x4 matrix, extending it with @[ 0 0 0 1 ]@
-- column vector
--
m43_to_m44 :: A.Num a => Exp (M43 a) -> Exp (M44 a)
m43_to_m44 m43 =
  let m44 = L.m43_to_m44 (unlift' m43)
  in  lift m44

-- | Convert a 3x3 matrix to a 4x4 matrix extending it with zeros in the new row
-- and column.
--
m33_to_m44 :: A.Num a => Exp (M33 a) -> Exp (M44 a)
m33_to_m44 m33 =
  let m44 = L.m33_to_m44 (unlift' m33)
  in  lift m44

-- | 2x2 matrix determinant
--
det22 :: A.Num a => Exp (M22 a) -> Exp a
det22 = L.det22 . unlift'

-- | 3x3 matrix determinant
--
det33 :: A.Num a => Exp (M33 a) -> Exp a
det33 = L.det33 . unlift'

-- | 4x4 matrix determinant
--
det44 :: A.Num a => Exp (M44 a) -> Exp a
det44 = L.det44 . unlift'

-- | 2x2 matrix inverse
--
inv22 :: A.Fractional a => Exp (M22 a) -> Exp (M22 a)
inv22 m =
  let r = L.inv22 (unlift' m)
  in  lift r

-- | 3x3 matrix inverse
--
inv33 :: A.Fractional a => Exp (M33 a) -> Exp (M33 a)
inv33 m =
  let r = L.inv33 (unlift' m)
  in  lift r

-- | 4x4 matrix inverse
--
inv44 :: A.Fractional a => Exp (M44 a) -> Exp (M44 a)
inv44 m =
  let r = L.inv44 (unlift' m)
  in  lift r
