{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Accelerate.Linear.Type
-- Copyright   : 2015 Trevor L. McDonell
-- License     : BSD-style (see the file LICENSE)
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
----------------------------------------------------------------------------

module Data.Array.Accelerate.Linear.Type
  where

import Data.Array.Accelerate

type Box f a            = (Unlift Exp (f (Exp a)), Plain (f (Exp a)) ~ f a)

-- type IsLens' s a        = IsLens s s a a
-- type IsLens s t a b     = (Lift Exp t, Lift Exp a, Unlift Exp s, Unlift Exp b)

