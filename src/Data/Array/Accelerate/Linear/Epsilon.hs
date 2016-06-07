-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Accelerate.Linear.Epsilon
-- Copyright   : 2014 Edward Kmett, Charles Durham,
--               2015 Trevor L. McDonell
-- License     : BSD-style (see the file LICENSE)
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Free metric spaces
----------------------------------------------------------------------------

module Data.Array.Accelerate.Linear.Epsilon
  where

import Foreign.C.Types
import Data.Array.Accelerate
import Prelude                                  as P


-- | Provides a fairly subjective test to see if a quantity is near zero.
--
-- >>> nearZero (1e-11 :: Double)
-- False
--
-- >>> nearZero (1e-17 :: Double)
-- True
--
-- >>> nearZero (1e-5 :: Float)
-- False
--
-- >>> nearZero (1e-7 :: Float)
-- True
--
class P.Num a => Epsilon a where
  -- | Determine if a quantity is near zero.
  nearZero :: Exp a -> Exp Bool

-- | @'abs' a '<=' 1e-6@
--
instance Epsilon Float where
  nearZero a = abs a <=* 1.0e-6

-- | @'abs' a '<=' 1e-12@
--
instance Epsilon Double where
  nearZero a = abs a <=* 1.0e-12

-- | @'abs' a '<=' 1e-6@
--
instance Epsilon CFloat where
  nearZero a = abs a <=* 1.0e-6

-- | @'abs' a '<=' 1e-12@
--
instance Epsilon CDouble where
  nearZero a = abs a <=* 1.0e-12

