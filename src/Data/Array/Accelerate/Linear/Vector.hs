{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
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

import Data.Array.Accelerate
import Data.Array.Accelerate.Linear.Type

import Control.Lens
import qualified Linear.Vector                  as L

infixl 6 ^+^, ^-^
infixl 7 ^*, *^, ^/

-- | A vector is an additive group with additional structure.
--
-- TODO: Support both 'Exp' and 'Acc'
--
class L.Additive f => Additive f where

  -- | The zero vector
  --
  zero :: (Elt (f a), IsNum a) => Exp (f a)
  zero = constant (L.zero)

  -- | Compute the sum of two vectors
  --
  -- >>> V2 1 2 ^+^ V2 3 4
  -- V2 4 6
  --
  (^+^) :: forall a. (Elt a, IsNum a, Box f a)
        => Exp (f a)
        -> Exp (f a)
        -> Exp (f a)
  (^+^) = lift2 ((L.^+^) :: f (Exp a) -> f (Exp a) -> f (Exp a))

  -- | Compute the difference between two vectors
  --
  -- >>> V2 4 5 - V2 3 1
  -- V2 1 4
  --
  (^-^) :: forall a. (Elt a, IsNum a, Box f a)
        => Exp (f a)
        -> Exp (f a)
        -> Exp (f a)
  (^-^) = lift2 ((L.^-^) :: f (Exp a) -> f (Exp a) -> f (Exp a))

  -- | Linearly interpolate between two vectors
  --
  lerp :: forall a. (Elt a, IsNum a, Box f a)
       => Exp a
       -> Exp (f a)
       -> Exp (f a)
       -> Exp (f a)
  lerp = lift3 (L.lerp :: Exp a -> f (Exp a) -> f (Exp a) -> f (Exp a))


type IsAdditive f a = (Additive f, Box f a)


-- | Basis element
--
newtype E t = E {
    el :: forall a. Elt a => Lens' (Exp (t a)) (Exp a)
  }

-- | Lift a 'Lens'' into 'Exp' terms
--
liftLens
    :: (Elt a, Elt b, IsLens s t a b)
    => Lens s t a b
    -> Lens (Exp (Plain s)) (Exp (Plain t)) (Exp (Plain a)) (Exp (Plain b))
liftLens l f = fmap lift . l (fsink1 f) . unlift

fsink1
    :: (Functor f, Unlift Exp b, Lift Exp a)
    => (Exp (Plain a) -> f (Exp (Plain b)))
    -> a
    -> f b
fsink1 f = fmap unlift . f . lift


-- | Compute the negation of a vector
--
-- >>> negated (V2 2 4)
-- V2 (-2) (-4)
--
negated
    :: forall f a. (Elt a, Functor f, IsNum a, Box f a)
    => Exp (f a)
    -> Exp (f a)
negated = lift1 (L.negated :: f (Exp a) -> f (Exp a))

-- | Compute the left scalar product
--
-- >>> 2 *^ V2 3 4
-- V2 6 8
--
(*^) :: forall f a. (Elt a, Functor f, IsNum a, Box f a)
     => Exp a
     -> Exp (f a)
     -> Exp (f a)
(*^) = lift2 ((L.*^) :: Exp a -> f (Exp a) -> f (Exp a))

-- | Compute the right scalar product
--
-- >>> V2 3 4 ^* 2
-- V2 6 8
--
(^*) :: forall f a. (Elt a, Functor f, IsNum a, Box f a)
     => Exp (f a)
     -> Exp a
     -> Exp (f a)
(^*) = lift2 ((L.^*) :: f (Exp a) -> Exp a -> f (Exp a))

-- | Compute division by a scalar on the right.
--
(^/) :: forall f a. (Elt a, Functor f, IsFloating a, Box f a)
     => Exp (f a)
     -> Exp a
     -> Exp (f a)
(^/) = lift2 ((L.^/) :: f (Exp a) -> Exp a -> f (Exp a))

