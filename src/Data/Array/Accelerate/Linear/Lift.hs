{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Accelerate.Linear.Lift
-- Copyright   : 2015 Trevor L. McDonell
-- License     : BSD-style (see the file LICENSE)
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
--
----------------------------------------------------------------------------

module Data.Array.Accelerate.Linear.Lift ( liftLens, unlift' )
  where

import Data.Array.Accelerate
import Data.Array.Accelerate.Linear.Type


-- Lift a 'Lens' into Accelerate terms
--
liftLens
    :: (Functor f, Unlift box s, Unlift box t, Unlift box b, Lift box a)
    => ((a -> f b) -> s -> f t)                         -- Lens s t a b
    -> (box (Plain a) -> f (box (Plain b)))
    -> box (Plain s)
    -> f (box (Plain t))
liftLens l f x = lift `fmap` l (fsink1 f) (unlift x)


-- | Sink a unary functor from Accelerate terms
--
fsink1 :: (Functor f, Unlift box b, Lift box a)
       => (box (Plain a) -> f (box (Plain b)))
       -> a
       -> f b
fsink1 f = fmap unlift . f . lift


-- | 'unlift' through two surface types
--
unlift' :: forall f g a. (Functor f, Box f (g a), Box g a)
        => Exp (f (g a))
        -> f (g (Exp a))
unlift' x = fmap unlift (unlift x :: f (Exp (g a)))

