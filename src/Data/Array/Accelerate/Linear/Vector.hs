{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Accelerate.Linear.Vector
-- Copyright   : 2014 Edward Kmett, Charles Durham,
--               2015 Trevor L. McDonell
-- License     : BSD-style (see the file LICENSE)
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Operations on free vector spaces
----------------------------------------------------------------------------

module Data.Array.Accelerate.Linear.Vector
  where

import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.Linear.Type

import Control.Lens
import Prelude                                  as P
import qualified Linear.Vector                  as L

infixl 6 ^+^, ^+, +^, ^-^, ^-, -^
infixl 7 ^*, *^, ^/, /^

-- $setup
-- >>> import Data.Array.Accelerate.Linear.V2 ()
-- >>> import Linear.V2

-- | A vector is an additive group with additional structure.
--
-- TODO: Support both 'Exp' and 'Acc'
--
class L.Additive f => Additive f where

  -- | The zero vector
  --
  zero :: (Elt (f a), P.Num a) => Exp (f a)
  zero = constant (L.zero)

  -- | Compute the sum of two vectors
  --
  -- >>> lift (V2 1 2 :: V2 Int) ^+^ lift (V2 3 4 :: V2 Int)
  -- (4,6)
  --
  (^+^) :: forall a. (A.Num a, Box f a)
        => Exp (f a)
        -> Exp (f a)
        -> Exp (f a)
  (^+^) = lift2 ((L.^+^) :: f (Exp a) -> f (Exp a) -> f (Exp a))

  -- | Compute the difference between two vectors
  --
  -- >>> lift (V2 4 5 :: V2 Int) ^-^ lift (V2 3 1 :: V2 Int)
  -- (1,4)
  --
  (^-^) :: forall a. (A.Num a, Box f a)
        => Exp (f a)
        -> Exp (f a)
        -> Exp (f a)
  (^-^) = lift2 ((L.^-^) :: f (Exp a) -> f (Exp a) -> f (Exp a))

  -- | Linearly interpolate between two vectors
  --
  lerp :: forall a. (A.Num a, Box f a)
       => Exp a
       -> Exp (f a)
       -> Exp (f a)
       -> Exp (f a)
  lerp = lift3 (L.lerp :: Exp a -> f (Exp a) -> f (Exp a) -> f (Exp a))


type IsAdditive f a = (Additive f, Box f a)


-- | Basis element
--
newtype E t = E {
    el :: forall a. (Elt a, Box t a) => Lens' (Exp (t a)) (Exp a)
  }


-- | Compute the negation of a vector
--
-- >>> negated (lift (V2 2 4 :: V2 Int))
-- (-2,-4)
--
negated
    :: forall f a. (Functor f, A.Num a, Box f a)
    => Exp (f a)
    -> Exp (f a)
negated = lift1 (L.negated :: f (Exp a) -> f (Exp a))

-- | Compute the left scalar product
--
-- >>> 2 *^ lift (V2 3 4 :: V2 Int)
-- (6,8)
--
(*^) :: forall f a. (Functor f, A.Num a, Box f a)
     => Exp a
     -> Exp (f a)
     -> Exp (f a)
(*^) = lift2 ((L.*^) :: Exp a -> f (Exp a) -> f (Exp a))

-- | Compute the right scalar product
--
-- >>> lift (V2 3 4 :: V2 Int) ^* 2
-- (6,8)
--
(^*) :: forall f a. (Functor f, A.Num a, Box f a)
     => Exp (f a)
     -> Exp a
     -> Exp (f a)
(^*) = lift2 ((L.^*) :: f (Exp a) -> Exp a -> f (Exp a))

-- | Compute division by a scalar on the right
--
-- lift (V2 4 6 :: V2 Double) ^/ 2
-- V2 2 3
--
(^/) :: forall f a. (Functor f, A.Fractional a, Box f a)
     => Exp (f a)
     -> Exp a
     -> Exp (f a)
(^/) = lift2 ((L.^/) :: f (Exp a) -> Exp a -> f (Exp a))

-- | Compute division of a scalar on the left
--
-- >>> 4 /^ lift (V2 2 4 :: V2 Double)
-- (2.0,1.0)
--
(/^) :: forall f a. (Functor f, A.Fractional a, Box f a)
     => Exp a
     -> Exp (f a)
     -> Exp (f a)
(/^) = lift2 ((\a f -> fmap (a/) f) :: Exp a -> f (Exp a) -> f (Exp a))

-- | Addition with a scalar on the left
--
-- >>> 2 +^ lift (V2 3 4 :: V2 Int)
-- (5,6)
--
(+^) :: forall f a. (Functor f, A.Num a, Box f a)
     => Exp a
     -> Exp (f a)
     -> Exp (f a)
(+^) = lift2 ((\a f -> fmap (+a) f) :: Exp a -> f (Exp a) -> f (Exp a))

-- | Addition with a scalar on the right
--
-- >>> lift (V2 1 2 :: V2 Int) ^+ 3
-- (4,5)
--
(^+) :: forall f a. (Functor f, A.Num a, Box f a)
     => Exp (f a)
     -> Exp a
     -> Exp (f a)
(^+) = lift2 ((\f a -> fmap (a+) f) :: f (Exp a) -> Exp a -> f (Exp a))

-- | Subtraction with a scalar on the left
--
-- >>> 2 -^ lift (V2 3 4 :: V2 Int)
-- (1,2)
--
(-^) :: forall f a. (Functor f, A.Num a, Box f a)
     => Exp a
     -> Exp (f a)
     -> Exp (f a)
(-^) = lift2 ((\a f -> fmap (A.subtract a) f) :: Exp a -> f (Exp a) -> f (Exp a))

-- | Subtraction with a scalar on the right
--
-- >>> lift (V2 1 2 :: V2 Int) ^- 3
-- (2,1)
--
(^-) :: forall f a. (Functor f, A.Num a, Box f a)
     => Exp (f a)
     -> Exp a
     -> Exp (f a)
(^-) = lift2 ((\f a -> fmap (a-) f) :: f (Exp a) -> Exp a -> f (Exp a))

