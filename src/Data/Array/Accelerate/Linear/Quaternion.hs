{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Accelerate.Linear.Quaternion
-- Copyright   : 2014 Edward Kmett, Charles Durham,
--               [2015..2020] Trevor L. McDonell
-- License     : BSD-style (see the file LICENSE)
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Quaternions
----------------------------------------------------------------------------

module Data.Array.Accelerate.Linear.Quaternion (

  Quaternion(..), pattern Quaternion_,

  slerp,
  asinq,
  acosq,
  atanq,
  asinhq,
  acoshq,
  atanhq,
  absi,
  pow,
  rotate,
  axisAngle,

) where

import Data.Array.Accelerate                    hiding ( pattern V3 )
import Data.Array.Accelerate.Data.Complex       hiding ( conjugate )
import Data.Array.Accelerate.Data.Functor
import Data.Array.Accelerate.Smart

import Data.Array.Accelerate.Linear.Conjugate
import Data.Array.Accelerate.Linear.Epsilon
import Data.Array.Accelerate.Linear.Lift
import Data.Array.Accelerate.Linear.Metric
import Data.Array.Accelerate.Linear.V3
import Data.Array.Accelerate.Linear.Vector

import Control.Lens
import Data.Function
import Linear.Quaternion                        ( Quaternion(..) )
import qualified Prelude                        as P


-- | Spherical linear interpolation between two quaternions
--
slerp :: RealFloat a => Exp (Quaternion a) -> Exp (Quaternion a) -> Exp a -> Exp (Quaternion a)
slerp q p t =
  if 1.0 - cosphi < 1.0e-8
    then q
    else ((sin ((1-t)*phi) *^ q) + sin (t*phi) *^ fp) ^/ sin phi
  where
    dqp = dot q p
    phi = acos cosphi
    T2 cosphi fp = if dqp < 0 then T2 (negate dqp) (negate p)
                              else T2 dqp p

-- | 'asin' with a specified branch cut
--
asinq :: (RealFloat a, Elt (Complex a)) => Exp (Quaternion a) -> Exp (Quaternion a) -> Exp (Quaternion a)
asinq q@(Quaternion_ e _) u =
  if qiq /= 0.0 || e >= -1 && e <= 1
    then asin q
    else cutWith (asin (e ::+ sqrt qiq)) u
  where
    qiq = qi q

-- | 'acos' with a specified branch cut
--
acosq :: (RealFloat a, Elt (Complex a)) => Exp (Quaternion a) -> Exp (Quaternion a) -> Exp (Quaternion a)
acosq q@(Quaternion_ e _) u =
  if qiq /= 0.0 || e >= -1 && e <= 1
    then acos q
    else cutWith (acos (e ::+ sqrt qiq)) u
  where
    qiq = qi q

-- | 'atan' with a specified branch cut
--
atanq :: (RealFloat a, Elt (Complex a)) => Exp (Quaternion a) -> Exp (Quaternion a) -> Exp (Quaternion a)
atanq q@(Quaternion_ e _) u =
  if e /= 0.0 || qiq >= -1 && qiq <= 1
    then atan q
    else cutWith (atan (e ::+ sqrt qiq)) u
  where
    qiq = qi q

-- | 'asinh' with a specified branch cut
--
asinhq :: (RealFloat a, Elt (Complex a)) => Exp (Quaternion a) -> Exp (Quaternion a) -> Exp (Quaternion a)
asinhq q@(Quaternion_ e _) u =
  if e /= 0.0 || qiq >= -1 && qiq <= 1
    then asinh q
    else cutWith (asinh (e ::+ sqrt qiq)) u
  where
    qiq = qi q

-- | 'acosh' with a specified branch cut
--
acoshq :: (RealFloat a, Elt (Complex a)) => Exp (Quaternion a) -> Exp (Quaternion a) -> Exp (Quaternion a)
acoshq q@(Quaternion_ e _) u =
  if qiq /= 0.0 || e >= 1
    then asinh q
    else cutWith (acosh (e ::+ sqrt qiq)) u
  where
    qiq = qi q

-- | 'atanh' with a specified branch cut
--
atanhq :: (RealFloat a, Elt (Complex a)) => Exp (Quaternion a) -> Exp (Quaternion a) -> Exp (Quaternion a)
atanhq q@(Quaternion_ e _) u =
  if qiq /= 0.0 || e > -1 && e < 1
    then atanh q
    else cutWith (atanh (e ::+ sqrt qiq)) u
  where
    qiq = qi q

-- | norm of the imaginary component
--
absi :: Floating a => Exp (Quaternion a) -> Exp a
absi = sqrt . qi

-- | raise a 'Quaternion' to a scalar power
--
pow :: (RealFloat a, Elt (Complex a)) => Exp (Quaternion a) -> Exp a -> Exp (Quaternion a)
pow q t = exp (t *^ log q)

-- | Apply a rotation to a vector
--
rotate :: forall a. (Conjugate (Exp a), RealFloat a) => Exp (Quaternion a) -> Exp (V3 a) -> Exp (V3 a)
rotate q v = ijk
  where
    Quaternion_ _ ijk = q * (Quaternion_ 0 v) * conjugate q

-- | @'axisAngle' axis theta@ builds a 'Quaternion' representing a rotation of
-- @theta@ radians about @axis@.
--
axisAngle :: (Epsilon a, Floating a) => Exp (V3 a) -> Exp a -> Exp (Quaternion a)
axisAngle axis theta = Quaternion_ (cos half) (sin half *^ normalize axis)
  where
    half = theta / 2


-- Instances
-- ---------

pattern Quaternion_ :: Elt a => Exp a -> Exp (V3 a) -> Exp (Quaternion a)
pattern Quaternion_ x v = Pattern (x,v)
{-# COMPLETE Quaternion_ #-}

instance Metric Quaternion
instance Additive Quaternion
instance Elt a => Elt (Quaternion a)

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Quaternion a) where
  type Plain (Quaternion a) = Quaternion (Plain a)
  lift (Quaternion x v) = Quaternion_ (lift x) (lift v)

instance Elt a => Unlift Exp (Quaternion (Exp a)) where
  unlift (Quaternion_ x v) = Quaternion x (unlift v)

instance (Elt a, Elt b) => Each (Exp (Quaternion a)) (Exp (Quaternion b)) (Exp a) (Exp b) where
  each = liftLens (each :: Traversal (Quaternion (Exp a)) (Quaternion (Exp b)) (Exp a) (Exp b))

instance Eq a => Eq (Quaternion a) where
  (==) = (==) `on` t4
  (/=) = (/=) `on` t4

instance Ord a => Ord (Quaternion a) where
  (<)  = (<) `on` t4
  (>)  = (>) `on` t4
  (<=) = (<=) `on` t4
  (>=) = (>=) `on` t4
  min  = qu $$ on min t4
  max  = qu $$ on max t4

t4 :: Elt a => Exp (Quaternion a) -> Exp (a,a,a,a)
t4 (Quaternion_ x (V3_ y z w)) = T4 x y z w

qu :: Elt a => Exp (a,a,a,a) -> Exp (Quaternion a)
qu (T4 x y z w) = Quaternion_ x (V3_ y z w)

instance RealFloat a => P.Num (Exp (Quaternion a)) where
  (+)           = lift2 ((+) :: Quaternion (Exp a) -> Quaternion (Exp a) -> Quaternion (Exp a))
  (-)           = lift2 ((-) :: Quaternion (Exp a) -> Quaternion (Exp a) -> Quaternion (Exp a))
  negate        = fmap negate
  abs z         = Quaternion_ (norm z) (V3_ 0 0 0)
  fromInteger x = Quaternion_ (fromInteger x) (V3_ 0 0 0)

  z1 * z2       = let Quaternion_ s1 v1 = z1
                      Quaternion_ s2 v2 = z2
                   in
                   Quaternion_ (s1*s2 - (v1 `dot` v2))
                               ((v1 `cross` v2) + s1*^v2 + s2*^v1)

  signum q@(Quaternion_ e (V3_ i j k)) =
    if m == 0.0                      then q else
    if not (isInfinite m || isNaN m) then q ^/ sqrt m else
    if ne || ni || nj || nk          then qNaN else
    if not (ii || ij || ik)          then Quaternion_ 1 (V3_ 0 0 0) else
    if not (ie || ij || ik)          then Quaternion_ 0 (V3_ 1 0 0) else
    if not (ie || ii || ik)          then Quaternion_ 0 (V3_ 0 1 0) else
    if not (ie || ii || ij)          then Quaternion_ 0 (V3_ 0 0 1)
                                     else qNaN
    where
      m  = quadrance q
      ie = isInfinite e
      ii = isInfinite i
      ij = isInfinite j
      ik = isInfinite k
      ne = isNaN e
      ni = isNaN i
      nj = isNaN j
      nk = isNaN k
      --
      qNaN = Quaternion_ fNaN (V3_ fNaN fNaN fNaN)
      fNaN = 0/0

instance RealFloat a => P.Fractional (Exp (Quaternion a)) where
  z1 / z2 =
    let Quaternion_ q0 (V3_ q1 q2 q3) = z1
        Quaternion_ r0 (V3_ r1 r2 r3) = z2
    in
    Quaternion_
        (r0 * q0 + r1 * q1 + r2 * q2 + r3 * q3)
        (V3_ (r0 * q1 - r1 * q0 - r2 * q3 + r3 * q2)
             (r0 * q2 + r1 * q3 - r2 * q0 - r3 * q1)
             (r0 * q3 - r1 * q2 + r2 * q1 - r3 * q0)
        )
      ^/ (r0 * r0 + r1 * r1 + r2 * r2 + r3 * r3)

  recip q = let Quaternion_ e v = q
             in Quaternion_ e (fmap negate v) ^/ quadrance q

  fromRational x = Quaternion_ (fromRational x) (V3_ 0 0 0)

instance (RealFloat a, Elt (Complex a)) => P.Floating (Exp (Quaternion a)) where
  pi = Quaternion_ pi (V3_ 0 0 0)

  exp q@(Quaternion_ e v) =
    if qiq == 0
      then Quaternion_ exe v
      else reimagine (exe * cos ai) (exe * (sin ai / ai)) q
    where
      qiq = qi q
      ai  = sqrt qiq
      exe = exp e

  log q@(Quaternion_ e v@(V3_ _i j k)) =
    if qiq == 0
      then if e >= 0
             then Quaternion_ (log e) v
             else Quaternion_ (log (negate e)) (V3_ pi j k) -- mmm, pi
      else reimagine (log m) (atan2 m e / ai) q
    where
      qiq = qi q
      ai  = sqrt qiq
      m   = sqrte2pqiq e qiq

  x ** y = exp (y * log x)

  sqrt q@(Quaternion_ e v) =
    if m   == 0 then q else
    if qiq == 0 then if e > 0
                        then Quaternion_ (sqrt e) (V3_ 0 0 0)
                        else Quaternion_ 0 (V3_ (sqrt (negate e)) 0 0)
                else Quaternion_ (0.5*(m+e)) (v ^* im)
    where
      qiq = qi q
      im  = sqrt (0.5*(m-e)) / sqrt qiq
      m   = sqrte2pqiq e qiq

  cos q@(Quaternion_ e v) =
    if qiq == 0 then Quaternion_ (cos e) v
                else reimagine (cos e * cosh ai) (- sin e / ai / sinh ai) q -- 0.15 bits error
                  -- reimagine (cos e * cosh ai) (- sin e * sinh ai / ai) q -- 13.5 bits worse
    where
      qiq = qi q
      ai  = sqrt qiq

  sin q@(Quaternion_ e v) =
    if qiq == 0 then Quaternion_ (sin e) v
                else reimagine (sin e * cosh ai) (cos e * sinh ai / ai) q
    where
      qiq = qi q
      ai  = sqrt qiq

  tan q@(Quaternion_ e v) =
    if qiq == 0 then Quaternion_ (tan e) v
                else reimagine (ce * sin e / d) (tanrhs sai ai d) q
    where
      qiq = qi q
      ai  = sqrt qiq
      ce  = cos e
      sai = sinh ai
      d   = ce*ce + sai*sai

  sinh q@(Quaternion_ e v) =
    if qiq == 0 then Quaternion_ (sinh e) v
                else reimagine (sinh e * cos ai) (cosh e * sin ai / ai) q
    where
      qiq = qi q
      ai  = sqrt qiq

  cosh q@(Quaternion_ e v) =
    if qiq == 0 then Quaternion_ (cosh e) v
                else reimagine (cosh e * cos ai) (sin ai * (sinh e / ai)) q
    where
      qiq = qi q
      ai  = sqrt qiq

  tanh q@(Quaternion_ e v) =
    if qiq == 0 then Quaternion_ (tanh e) v
                else reimagine (cosh e * se / d) (tanhrhs cai ai d) q
    where
      qiq = qi q
      ai  = sqrt qiq
      se  = sinh e
      cai = cos ai
      d   = se*se + cai*cai

  asin = cut asin
  acos = cut acos
  atan = cut atan

  asinh = cut asinh
  acosh = cut acosh
  atanh = cut atanh


reimagine :: RealFloat a => Exp a -> Exp a -> Exp (Quaternion a) -> Exp (Quaternion a)
reimagine r s (Quaternion_ _ v) =
  if isNaN s || isInfinite s
    then let aux x = if x == 0 then 0
                               else s * x
         in Quaternion_ r (fmap aux v)
    else    Quaternion_ r (v ^* s)

-- | Helper for calculating with specific branch cuts
--
cut :: (RealFloat a, Elt (Complex a)) => (Exp (Complex a) -> Exp (Complex a)) -> Exp (Quaternion a) -> Exp (Quaternion a)
cut f q@(Quaternion_ e (V3_ _ y z)) =
  if qiq == 0 then Quaternion_ a (V3_ b y z)
              else reimagine a (b / ai) q
  where
    qiq     = qi q
    ai      = sqrt qiq
    a ::+ b = f (e ::+ ai)

-- | Helper for calculating with specific branch cuts
--
cutWith :: (RealFloat a, Elt (Complex a)) => Exp (Complex a) -> Exp (Quaternion a) -> Exp (Quaternion a)
cutWith (r ::+ im) q@(Quaternion_ e v) =
  if e /= 0 || qiq == 0 || isNaN qiq || isInfinite qiq
    then 0/0 -- error "bad cut"  -- TLM: argh
    else Quaternion_ r (v ^* s)
  where
    qiq = qi q
    s   = im / sqrt qiq

-- | quadrance of the imaginary component
--
qi :: Num a => Exp (Quaternion a) -> Exp a
qi (Quaternion_ _ v) = quadrance v

sqrte2pqiq :: (Floating a, Ord a) => Exp a -> Exp a -> Exp a
sqrte2pqiq e qiq = -- = sqrt (e*e) + qiq
  if e < - 1.5097698010472593e153 then -(qiq/e) - e else
  if e < 5.582399551122541e57     then sqrt ((e*e) + qiq) -- direct definition
                                  else (qiq/e) + e

tanrhs :: (Floating a, Ord a) => Exp a -> Exp a -> Exp a -> Exp a
tanrhs sai ai d = -- = cosh ai * (sai / ai) / d -- improved from 6.04 bits of error to 0.19 bits
  if sai < -4.618902267687042e-52 then (sai / d / ai) * cosh ai else
  if sai < 1.038530535935153e-39  then (cosh ai * sai) / ai / d
                                  else (sai / d / ai) * cosh ai

tanhrhs :: (Floating a, Ord a) => Exp a -> Exp a -> Exp a -> Exp a
tanhrhs cai ai d = -- = cai * (sin ai / ai) / d
  if d >= -4.2173720203427147e-29 && d < 4.446702369113811e64
    then cai / (d * (ai / sin ai))
    else cai * (1 / ai / sin ai) / d

instance (RealFloat a, Epsilon a) => Epsilon (Quaternion a) where
  nearZero = nearZero . quadrance

instance (RealFloat a, Conjugate (Exp a)) => Conjugate (Exp (Quaternion a)) where
  conjugate (Quaternion_ e v) = (Quaternion_ (conjugate e) (negate v))

instance Functor Quaternion where
  fmap f (Quaternion_ e v) = Quaternion_ (f e) (fmap f v)
  x <$ _                   = Quaternion_ x (V3_ x x x)

