{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Accelerate.Linear.Matrix
-- Copyright   : 2014 Edward Kmett, Charles Durham,
--               2015 Trevor L. McDonell
-- License     : BSD-style (see the file LICENSE)
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Simple matrix operations for low-dimensional primitives
----------------------------------------------------------------------------

module Data.Array.Accelerate.Linear.Matrix (

  M22, M23, M24, M32, M33, M34, M42, M43, M44,
  (!*!), (!+!), (!-!), (!*), (*!), (!!*), (*!!), (!!/),
  transpose,
  identity,
  Trace(..),

) where

import Data.Array.Accelerate                    as A hiding ( transpose )
import Data.Array.Accelerate.Data.Complex

import Data.Array.Accelerate.Linear.Lift
import Data.Array.Accelerate.Linear.Plucker
import Data.Array.Accelerate.Linear.Quaternion
import Data.Array.Accelerate.Linear.Type
import Data.Array.Accelerate.Linear.V0
import Data.Array.Accelerate.Linear.V1
import Data.Array.Accelerate.Linear.V2
import Data.Array.Accelerate.Linear.V3
import Data.Array.Accelerate.Linear.V4
import Data.Array.Accelerate.Linear.Vector

import Linear.Matrix                            ( M22, M23, M24, M32, M33, M34, M42, M43, M44 )
import qualified Linear.Matrix                  as L

import Data.Distributive
import Data.Foldable
import Data.Traversable
import Control.Applicative
import Prelude                                  as P



infixl 7 !*!
-- | Matrix product. This can compute any combination of sparse and dense multiplication.
--
-- >>> lift (V2 (V3 1 2 3) (V3 4 5 6) :: M23 Int) !*! lift (V3 (V2 1 2) (V2 3 4) (V2 4 5) :: M32 Int)
-- ((19,25),(43,58))
--
(!*!) :: (Functor m, Foldable t, Additive t, Additive n, A.Num a, Box2 m t a, Box2 t n a, Box2 m n a)
      => Exp (m (t a))
      -> Exp (t (n a))
      -> Exp (m (n a))
f !*! g = lift (unlift' f L.!*! unlift' g)


infixl 6 !+!
-- | Entry-wise matrix addition.
--
-- >>> lift (V2 (V3 1 2 3) (V3 4 5 6) :: M23 Int) !+! lift (V2 (V3 7 8 9) (V3 1 2 3) :: M23 Int)
-- ((8,10,12),(5,7,9))
--
(!+!) :: (Additive m, Additive n, A.Num a, Box2 m n a)
      => Exp (m (n a))
      -> Exp (m (n a))
      -> Exp (m (n a))
f !+! g = lift (unlift' f L.!+! unlift' g)


infixl 6 !-!
-- | Entry-wise matrix subtraction.
--
-- >>> lift (V2 (V3 1 2 3) (V3 4 5 6) :: M23 Int) !-! lift (V2 (V3 7 8 9) (V3 1 2 3) :: M23 Int)
-- ((-6,-6,-6),(3,3,3))
--
(!-!) :: (Additive m, Additive n, A.Num a, Box2 m n a)
      => Exp (m (n a))
      -> Exp (m (n a))
      -> Exp (m (n a))
f !-! g = lift (unlift' f L.!-! unlift' g)


infixl 7 !*
-- | Matrix * column vector
--
-- >>> lift (V2 (V3 1 2 3) (V3 4 5 6) :: M23 Int) !* lift (V3 7 8 9 :: V3 Int)
-- (50,122)
--
(!*) :: (Functor m, Foldable r, Additive r, A.Num a, Box2 m r a, Box m a)
     => Exp (m (r a))
     -> Exp (r a)
     -> Exp (m a)
m !* v = lift (unlift' m L.!* unlift v)


infixl 7 *!
-- | Row vector * matrix
--
-- >>> lift (V2 1 2 :: V2 Int) *! lift (V2 (V3 3 4 5) (V3 6 7 8) :: M23 Int)
-- (15,18,21)

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
-- >>> 5 *!! lift (V2 (V2 1 2) (V2 3 4) :: M22 Int)
-- ((5,10),(15,20))
--
(*!!) :: (Functor m, Functor r, A.Num a, Box2 m r a)
      => Exp a
      -> Exp (m (r a))
      -> Exp (m (r a))
s *!! m = lift (unlift s L.*!! unlift' m)


infixl 7 !!*
-- | Matrix-scalar product
--
-- >>> lift (V2 (V2 1 2) (V2 3 4) :: M22 Int) !!* 5
-- ((5,10),(15,20))
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
-- >>> identity :: Exp (M44 Int)
-- let x0 = 1 in
-- let x1 = 0
-- in ((x0,x1,x1,x1),(x1,x0,x1,x1),(x1,x1,x0,x1),(x1,x1,x1,x0))
--
-- >>> identity :: Exp (V3 (V3 Int))
-- let x0 = 1 in
-- let x1 = 0
-- in ((x0,x1,x1),(x1,x0,x1),(x1,x1,x0))
--
identity :: forall t a. (Traversable t, Applicative t, A.Num a, Box2 t t a) => Exp (t (t a))
identity = lift (L.identity :: t (t (Exp a)))


-- | 'transpose' is just an alias for 'distribute'
--
-- > transpose (V3 (V2 1 2) (V2 3 4) (V2 5 6))
-- V2 (V3 1 3 5) (V3 2 4 6)
--
transpose
    :: (Distributive g, Functor f, Box2 f g a, Box2 g f a)
    => Exp (f (g a))
    -> Exp (g (f a))
transpose = lift . L.transpose . unlift'


class L.Trace m => Trace m where
  -- | Compute the trace of a matrix
  trace :: (A.Num a, Box2 m m a) => Exp (m (m a)) -> Exp a
  trace = lift . L.trace . unlift'

  -- | Compute the diagonal of a matrix
  diagonal :: Box2 m m a => Exp (m (m a)) -> Exp (m a)
  diagonal = lift . L.diagonal . unlift'

instance Trace Complex
instance Trace V0
instance Trace V1
instance Trace V2
instance Trace V3
instance Trace V4
instance Trace Plucker
instance Trace Quaternion

