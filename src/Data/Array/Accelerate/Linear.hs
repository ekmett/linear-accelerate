-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Accelerate.Linear
-- Copyright   : 2014 Edward Kmett, Charles Durham,
--               2015 Trevor L. McDonell
-- License     : BSD-style (see the file LICENSE)
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- This module simply re-exports everything from the various modules
-- that make up the linear package, lifted to Accelerate.
----------------------------------------------------------------------------

module Data.Array.Accelerate.Linear (

  module Data.Array.Accelerate.Linear.Box,
  module Data.Array.Accelerate.Linear.Epsilon,
  module Data.Array.Accelerate.Linear.Metric,
  module Data.Array.Accelerate.Linear.V0,
  module Data.Array.Accelerate.Linear.V1,
  module Data.Array.Accelerate.Linear.V2,
  module Data.Array.Accelerate.Linear.V3,
  module Data.Array.Accelerate.Linear.Plucker,
  module Data.Array.Accelerate.Linear.Quaternion,

) where

import Data.Array.Accelerate.Linear.Box
import Data.Array.Accelerate.Linear.Epsilon
import Data.Array.Accelerate.Linear.Metric
import Data.Array.Accelerate.Linear.V0
import Data.Array.Accelerate.Linear.V1
import Data.Array.Accelerate.Linear.V2
import Data.Array.Accelerate.Linear.V3
import Data.Array.Accelerate.Linear.Plucker
import Data.Array.Accelerate.Linear.Quaternion

