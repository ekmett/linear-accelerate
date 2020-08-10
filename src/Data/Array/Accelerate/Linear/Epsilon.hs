{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Accelerate.Linear.Epsilon
-- Copyright   : 2014 Edward Kmett, Charles Durham,
--               [2015..2018] Trevor L. McDonell
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

import Data.Array.Accelerate


-- | Provides a fairly subjective test to see if a quantity is near zero.
--
-- >>> nearZero (1e-11 :: Exp Double)
-- (0, ())
--
-- >>> nearZero (1e-17 :: Exp Double)
-- (1, ())
--
-- >>> nearZero (1e-5 :: Exp Float)
-- (0, ())
--
-- >>> nearZero (1e-7 :: Exp Float)
-- (1, ())
--
class Num a => Epsilon a where
  -- | Determine if a quantity is near zero.
  nearZero :: Exp a -> Exp Bool

-- | @'abs' a '<=' 1e-6@
--
instance Epsilon Float where
  nearZero a = abs a <= 1.0e-6

-- | @'abs' a '<=' 1e-12@
--
instance Epsilon Double where
  nearZero a = abs a <= 1.0e-12

-- | @'abs' a '<=' 1e-6@
--
instance Epsilon CFloat where
  nearZero a = abs a <= 1.0e-6

-- | @'abs' a '<=' 1e-12@
--
instance Epsilon CDouble where
  nearZero a = abs a <= 1.0e-12

