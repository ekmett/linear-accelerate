{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Accelerate.Linear.Quaternion
-- Copyright   : 2014 Edward Kmett, Charles Durham,
--               [2015..2018] Trevor L. McDonell
-- License     : BSD-style (see the file LICENSE)
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Quaternions
----------------------------------------------------------------------------

module Data.Array.Accelerate.Linear.Quaternion (

  Quaternion(..), pattern Quaternion',

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

import Data.Array.Accelerate
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Data.Complex       hiding ( conjugate )
import Data.Array.Accelerate.Data.Functor
import Data.Array.Accelerate.Product
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
    (cosphi, fp) = unlift $ if dqp < 0 then tup2 (-dqp, negate p)
                                       else tup2 (dqp, p)

-- | 'asin' with a specified branch cut
--
asinq :: (RealFloat a, Elt (Complex a)) => Exp (Quaternion a) -> Exp (Quaternion a) -> Exp (Quaternion a)
asinq q@(unlift -> Quaternion e _) u =
  if qiq /= 0.0 || e >= -1 && e <= 1
    then asin q
    else cutWith (asin (lift $ e :+ sqrt qiq)) u
  where
    qiq = qi q

-- | 'acos' with a specified branch cut
--
acosq :: (RealFloat a, Elt (Complex a)) => Exp (Quaternion a) -> Exp (Quaternion a) -> Exp (Quaternion a)
acosq q@(unlift -> Quaternion e _) u =
  if qiq /= 0.0 || e >= -1 && e <= 1
    then acos q
    else cutWith (acos (lift $ e :+ sqrt qiq)) u
  where
    qiq = qi q

-- | 'atan' with a specified branch cut
--
atanq :: (RealFloat a, Elt (Complex a)) => Exp (Quaternion a) -> Exp (Quaternion a) -> Exp (Quaternion a)
atanq q@(unlift -> Quaternion e _) u =
  if e /= 0.0 || qiq >= -1 && qiq <= 1
    then atan q
    else cutWith (atan (lift $ e :+ sqrt qiq)) u
  where
    qiq = qi q

-- | 'asinh' with a specified branch cut
--
asinhq :: (RealFloat a, Elt (Complex a)) => Exp (Quaternion a) -> Exp (Quaternion a) -> Exp (Quaternion a)
asinhq q@(unlift -> Quaternion e _) u =
  if e /= 0.0 || qiq >= -1 && qiq <= 1
    then asinh q
    else cutWith (asinh (lift $ e :+ sqrt qiq)) u
  where
    qiq = qi q

-- | 'acosh' with a specified branch cut
--
acoshq :: (RealFloat a, Elt (Complex a)) => Exp (Quaternion a) -> Exp (Quaternion a) -> Exp (Quaternion a)
acoshq q@(unlift -> Quaternion e _) u =
  if qiq /= 0.0 || e >= 1
    then asinh q
    else cutWith (acosh (lift $ e :+ sqrt qiq)) u
  where
    qiq = qi q

-- | 'atanh' with a specified branch cut
--
atanhq :: (RealFloat a, Elt (Complex a)) => Exp (Quaternion a) -> Exp (Quaternion a) -> Exp (Quaternion a)
atanhq q@(unlift -> Quaternion e _) u =
  if qiq /= 0.0 || e > -1 && e < 1
    then atanh q
    else cutWith (atanh (lift $ e :+ sqrt qiq)) u
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
rotate q v = lift ijk
  where
    Quaternion _ ijk = unlift $ q * (lift (Quaternion 0 (unlift v))) * conjugate q :: Quaternion (Exp a)

-- | @'axisAngle' axis theta@ builds a 'Quaternion' representing a rotation of
-- @theta@ radians about @axis@.
--
axisAngle :: (Epsilon a, Floating a) => Exp (V3 a) -> Exp a -> Exp (Quaternion a)
axisAngle axis theta = lift $ Quaternion (cos half) (unlift (sin half *^ normalize axis))
  where
    half = theta / 2


-- Instances
-- ---------

pattern Quaternion' :: Elt a => Exp a -> Exp (V3 a) -> Exp (Quaternion a)
pattern Quaternion' x v = Pattern (x,v)

instance Metric Quaternion
instance Additive Quaternion
instance Elt a => Elt (Quaternion a)
instance Elt a => IsProduct Elt (Quaternion a)

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Quaternion a) where
  type Plain (Quaternion a) = Quaternion (Plain a)
  lift (Quaternion x v) = Exp $ Tuple $ NilTup `SnocTup` lift x
                                               `SnocTup` lift v

instance Elt a => Unlift Exp (Quaternion (Exp a)) where
  unlift t = Quaternion (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                (unlift (Exp $ ZeroTupIdx `Prj` t))

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
t4 (unlift -> Quaternion x (V3 y z w)) = tup4 (x,y,z,w)

qu :: Elt a => Exp (a,a,a,a) -> Exp (Quaternion a)
qu (untup4 -> (x,y,z,w)) = lift (Quaternion x (V3 y z w))

instance RealFloat a => P.Num (Exp (Quaternion a)) where
  (+)           = lift2 ((+) :: Quaternion (Exp a) -> Quaternion (Exp a) -> Quaternion (Exp a))
  (-)           = lift2 ((-) :: Quaternion (Exp a) -> Quaternion (Exp a) -> Quaternion (Exp a))
  negate        = fmap negate
  abs z         = lift (Quaternion (norm z) (V3 0 0 0))
  fromInteger x = lift (Quaternion (fromInteger x) (V3 0 0 0))

  z1 * z2       = let Quaternion s1 v1' = unlift z1; v1 = lift v1'
                      Quaternion s2 v2' = unlift z2; v2 = lift v2'
                  in
                  lift $ Quaternion (s1*s2 - (v1 `dot` v2))
                                    (unlift ((v1 `cross` v2) + s1*^v2 + s2*^v1))

  signum q@(unlift -> Quaternion e (V3 i j k)) =
    if m == 0.0                      then q else
    if not (isInfinite m || isNaN m) then q ^/ sqrt m else
    if ne || ni || nj || nk          then qNaN else
    if not (ii || ij || ik)          then lift $ Quaternion 1 (V3 0 0 0) else
    if not (ie || ij || ik)          then lift $ Quaternion 0 (V3 1 0 0) else
    if not (ie || ii || ik)          then lift $ Quaternion 0 (V3 0 1 0) else
    if not (ie || ii || ij)          then lift $ Quaternion 0 (V3 0 0 1)
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
      qNaN = lift $ Quaternion fNaN (V3 fNaN fNaN fNaN)
      fNaN = 0/0

instance RealFloat a => P.Fractional (Exp (Quaternion a)) where
  z1 / z2 =
    let Quaternion q0 (V3 q1 q2 q3) = unlift z1
        Quaternion r0 (V3 r1 r2 r3) = unlift z2
    in
    lift (Quaternion (r0*q0+r1*q1+r2*q2+r3*q3)
                     (V3 (r0*q1-r1*q0-r2*q3+r3*q2)
                         (r0*q2+r1*q3-r2*q0-r3*q1)
                         (r0*q3-r1*q2+r2*q1-r3*q0)))
      ^/ (r0*r0 + r1*r1 + r2*r2 + r3*r3)

  recip q = let Quaternion e v = unlift q :: Quaternion (Exp a)
            in  lift (Quaternion e (P.fmap negate v)) ^/ quadrance q

  fromRational x = lift (Quaternion (fromRational x) (V3 0 0 0))

instance (RealFloat a, Elt (Complex a)) => P.Floating (Exp (Quaternion a)) where
  pi = lift (Quaternion pi (V3 0 0 0))

  exp q@(unlift -> Quaternion e v) =
    if qiq == 0
      then lift (Quaternion exe v)
      else reimagine (exe * cos ai) (exe * (sin ai / ai)) q
    where
      qiq = qi q
      ai  = sqrt qiq
      exe = exp e

  log q@(unlift -> Quaternion e v@(V3 _i j k)) =
    if qiq == 0
      then if e >= 0
             then lift $ Quaternion (log e) v
             else lift $ Quaternion (log (negate e)) (V3 pi j k) -- mmm, pi
      else reimagine (log m) (atan2 m e / ai) q
    where
      qiq = qi q
      ai  = sqrt qiq
      m   = sqrte2pqiq e qiq

  x ** y = exp (y * log x)

  sqrt q@(unlift -> Quaternion e v) =
    if m   == 0 then q else
    if qiq == 0 then if e > 0
                        then lift $ Quaternion (sqrt e) (V3 0 0 0)
                        else lift $ Quaternion 0 (V3 (sqrt (negate e)) 0 0)
                else lift $ Quaternion (0.5*(m+e)) (unlift (lift v ^* im))
    where
      qiq = qi q
      im  = sqrt (0.5*(m-e)) / sqrt qiq
      m   = sqrte2pqiq e qiq

  cos q@(unlift -> Quaternion e v) =
    if qiq == 0 then lift $ Quaternion (cos e) v
                else reimagine (cos e * cosh ai) (- sin e / ai / sinh ai) q -- 0.15 bits error
                  -- reimagine (cos e * cosh ai) (- sin e * sinh ai / ai) q -- 13.5 bits worse
    where
      qiq = qi q
      ai  = sqrt qiq

  sin q@(unlift -> Quaternion e v) =
    if qiq == 0 then lift $ Quaternion (sin e) v
                else reimagine (sin e * cosh ai) (cos e * sinh ai / ai) q
    where
      qiq = qi q
      ai  = sqrt qiq

  tan q@(unlift -> Quaternion e v) =
    if qiq == 0 then lift $ Quaternion (tan e) v
                else reimagine (ce * sin e / d) (tanrhs sai ai d) q
    where
      qiq = qi q
      ai  = sqrt qiq
      ce  = cos e
      sai = sinh ai
      d   = ce*ce + sai*sai

  sinh q@(unlift -> Quaternion e v) =
    if qiq == 0 then lift $ Quaternion (sinh e) v
                else reimagine (sinh e * cos ai) (cosh e * sin ai / ai) q
    where
      qiq = qi q
      ai  = sqrt qiq

  cosh q@(unlift -> Quaternion e v) =
    if qiq == 0 then lift $ Quaternion (cosh e) v
                else reimagine (cosh e * cos ai) (sin ai * (sinh e / ai)) q
    where
      qiq = qi q
      ai  = sqrt qiq

  tanh q@(unlift -> Quaternion e v) =
    if qiq == 0 then lift $ Quaternion (tanh e) v
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
reimagine r s (unlift -> Quaternion _ v) =
  if isNaN s || isInfinite s
    then let aux x = if x == 0 then 0
                               else s * x
         in lift $ Quaternion r (P.fmap aux v)
    else    lift $ Quaternion r (unlift (lift v ^* s))

-- | Helper for calculating with specific branch cuts
--
cut :: (RealFloat a, Elt (Complex a)) => (Exp (Complex a) -> Exp (Complex a)) -> Exp (Quaternion a) -> Exp (Quaternion a)
cut f q@(unlift -> Quaternion e (V3 _ y z)) =
  if qiq == 0 then lift $ Quaternion a (V3 b y z)
              else reimagine a (b / ai) q
  where
    qiq    = qi q
    ai     = sqrt qiq
    a :+ b = unlift $ f (lift (e :+ ai))

-- | Helper for calculating with specific branch cuts
--
cutWith :: (RealFloat a, Elt (Complex a)) => Exp (Complex a) -> Exp (Quaternion a) -> Exp (Quaternion a)
cutWith (unlift -> r :+ im) q@(unlift -> Quaternion e v) =
  if e /= 0 || qiq == 0 || isNaN qiq || isInfinite qiq
    then 0/0 -- error "bad cut"  -- TLM: argh
    else lift $ Quaternion r (unlift (lift v ^* s))
  where
    qiq = qi q
    s   = im / sqrt qiq

-- | quadrance of the imaginary component
--
qi :: Num a => Exp (Quaternion a) -> Exp a
qi (unlift -> Quaternion _ v :: Quaternion (Exp a)) = quadrance (lift v)

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
  conjugate (unlift -> Quaternion e v :: Quaternion (Exp a)) =
    lift (Quaternion (conjugate e) (unlift (negate (lift v :: Exp (V3 a)))))

instance Functor Quaternion where
  fmap f (unlift -> Quaternion e v) = lift (Quaternion (f e) (P.fmap f v))
  x <$ _                            = lift (Quaternion x (V3 x x x))

