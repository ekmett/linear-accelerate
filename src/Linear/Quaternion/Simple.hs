{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Linear.Quaternion.Simple where

import Control.Applicative

import Data.Functor.Bind
import Data.Functor.Rep

import GHC.Arr (Ix(..))

import Linear.Metric
import Linear.V3
import Linear.Vector

import qualified Linear.Quaternion as Q

newtype Quaternion a = Quaternion { unQ :: (Q.Quaternion a) }

deriving instance Functor Quaternion
deriving instance Apply Quaternion
deriving instance Applicative Quaternion
deriving instance Additive Quaternion
deriving instance Bind Quaternion
deriving instance Monad Quaternion
--deriving instance Ix a => Ix (Quaternion a)

deriving instance Metric Quaternion

instance Floating a => Num (Quaternion a) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (-) = liftA2 (-)
  {-# INLINE (-) #-}
  negate = fmap negate
  {-# INLINE negate #-}
  Quaternion (Q.Quaternion s1 v1) * Quaternion (Q.Quaternion s2 v2) = 
    Quaternion (Q.Quaternion s3 v3)
    where s3 = s1 * s2 - (v1 `dot` v2)
          v3 = (v1 `cross` v2) + s1*^v2 + s2*^v1
  {-# INLINE (*) #-}
  fromInteger x = Quaternion $ Q.Quaternion (fromInteger x) 0
  {-# INLINE fromInteger #-}
  abs z = Quaternion $ Q.Quaternion (norm z) 0
  {-# INLINE abs #-}

  signum = undefined 
  --on hold
  
instance Floating a => Fractional (Quaternion a) where
  Quaternion (Q.Quaternion q0 (V3 q1 q2 q3)) / Quaternion (Q.Quaternion r0 (V3 r1 r2 r3)) = 
    Quaternion $ Q.Quaternion 
    (r0*q0+r1*q1+r2*q2+r3*q3)
    (V3 (r0*q1-r1*q0-r2*q3+r3*q2)
        (r0*q2+r1*q3-r2*q0-r3*q1)
        (r0*q3-r1*q2+r2*q1-r3*q0))
    ^/ (r0*r0 + r1*r1 + r2*r2 + r3*r3)
  {-# INLINE (/) #-}
  recip q = q ^/ quadrance q
  {-# INLINE recip #-}
  fromRational x = Quaternion $ Q.Quaternion (fromRational x) 0
  {-# INLINE fromRational #-}
  
