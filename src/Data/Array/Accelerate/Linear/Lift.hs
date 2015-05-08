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

module Data.Array.Accelerate.Linear.Lift ( liftLens )
  where

import Data.Array.Accelerate


-- Lift a 'Lens' into Accelerate terms
--
liftLens
    :: (Functor f, Unlift box s, Unlift box t, Unlift box b, Lift box a)
    => ((a -> f b) -> s -> f t)
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

