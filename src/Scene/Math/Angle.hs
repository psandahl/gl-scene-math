-- |
-- Module: Scene.Math.Angle
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.Math.Angle
    ( Angle (..)
    , toDegrees
    , toRadians
    , unpack
    ) where

import           Flow ((<|))

-- | A representation of an angle.
data Angle a
    = Degrees !a
    | Radians !a
    deriving (Eq, Show)

-- | Convert an 'Angle' to degrees.
toDegrees :: Floating a => Angle a -> Angle a
toDegrees deg@(Degrees _theta) = deg
toDegrees (Radians theta)      = Degrees <| theta * (180 / pi)
{-# INLINE toDegrees #-}

-- | Convert an 'Angle' to radians.
toRadians :: Floating a => Angle a -> Angle a
toRadians rad@(Radians _theta) = rad
toRadians (Degrees theta)      = Radians <| theta * (pi / 180)
{-# INLINE toRadians #-}

-- | Unpack an 'Angle' to its underlying type.
unpack :: Angle a -> a
unpack (Degrees theta) = theta
unpack (Radians theta) = theta
{-# INLINE unpack #-}

{-}
-- | Serialize the 'Angle' to a floating value while converting to degrees.
toDegrees :: Floating a => Angle a -> a
toDegrees (Degrees angle) = angle
toDegrees (Radians angle) = angle * (180 / pi)
{-# INLINE toDegrees #-}

-- | Serialize the 'Angle' to a floating value while convertiong to radians.
toRadians :: Floating a => Angle a -> a
toRadians (Degrees angle) = angle * (pi / 180)
toRadians (Radians angle) = angle
{-# INLINE toRadians #-}

-- | Add two 'Angle's.
angleAdd :: Num a => Angle a -> Angle a -> Maybe (Angle a)
angleAdd (Degrees angle1) (Degrees angle2) = Just $! Degrees (angle1 + angle2)
angleAdd (Radians angle1) (Radians angle2) = Just $! Radians (angle1 + angle2)
angleAdd _ _                               = Nothing
{-# INLINE angleAdd #-}

-- | Add an immediate to an 'Angle'.
angleAddI :: Num a => Angle a -> a -> Angle a
angleAddI (Degrees angle) immediate = Degrees (angle + immediate)
angleAddI (Radians angle) immediate = Radians (angle + immediate)
{-# INLINE angleAddI #-}

-- | Subtract 'Angle's.
angleSub :: Num a => Angle a -> Angle a -> Maybe (Angle a)
angleSub (Degrees angle1) (Degrees angle2) = Just $! Degrees (angle1 - angle2)
angleSub (Radians angle1) (Radians angle2) = Just $! Radians (angle1 - angle2)
angleSub _ _                               = Nothing
{-# INLINE angleSub #-}

-- | Subtract an immediate from an 'Angle'.
angleSubI :: Num a => Angle a -> a -> Angle a
angleSubI (Degrees angle) immediate = Degrees (angle - immediate)
angleSubI (Radians angle) immediate = Radians (angle - immediate)
{-# INLINE angleSubI #-}
-}
