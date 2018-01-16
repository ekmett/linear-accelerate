{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Accelerate.Linear.Conjugate
-- Copyright   : 2018 Trevor L. McDonell
-- License     : BSD-style (see the file LICENSE)
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Involutive rings
----------------------------------------------------------------------------

module Data.Array.Accelerate.Linear.Conjugate (

  Conjugate(..),
  TrivialConjugate,

) where

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Data.Complex                           ( Complex(..) )

import Linear.Conjugate

instance Conjugate (Exp Int)
instance Conjugate (Exp Int64)
instance Conjugate (Exp Int32)
instance Conjugate (Exp Int16)
instance Conjugate (Exp Int8)
instance Conjugate (Exp Word)
instance Conjugate (Exp Word64)
instance Conjugate (Exp Word32)
instance Conjugate (Exp Word16)
instance Conjugate (Exp Word8)
instance Conjugate (Exp Double)
instance Conjugate (Exp Float)
instance Conjugate (Exp CFloat)
instance Conjugate (Exp CDouble)

instance (Conjugate (Exp a), A.RealFloat a) => Conjugate (Exp (Complex a)) where
  conjugate (unlift -> a :+ b :: Complex (Exp a)) = lift $ conjugate a :+ negate b

instance TrivialConjugate (Exp Int)
instance TrivialConjugate (Exp Int64)
instance TrivialConjugate (Exp Int32)
instance TrivialConjugate (Exp Int16)
instance TrivialConjugate (Exp Int8)
instance TrivialConjugate (Exp Word)
instance TrivialConjugate (Exp Word64)
instance TrivialConjugate (Exp Word32)
instance TrivialConjugate (Exp Word16)
instance TrivialConjugate (Exp Word8)
instance TrivialConjugate (Exp Double)
instance TrivialConjugate (Exp Float)
instance TrivialConjugate (Exp CFloat)
instance TrivialConjugate (Exp CDouble)

