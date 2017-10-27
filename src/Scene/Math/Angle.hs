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
    , addAngles
    , mulAngle
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

-- | Add two angles together. If the units differ, the right angle will be
-- converted to the unit of the left angle.
addAngles :: Floating a => Angle a -> Angle a -> Angle a
addAngles (Degrees left) (Degrees right) = Degrees <| left + right
addAngles (Radians left) (Radians right) = Radians <| left + right
addAngles left@(Degrees _l) right        = addAngles left <| toDegrees right
addAngles left@(Radians _l) right        = addAngles left <| toRadians right
{-# INLINE addAngles #-}

-- | Multiply an angle with a literal value.
mulAngle :: Floating a => Angle a -> a -> Angle a
mulAngle (Degrees theta) factor = Degrees <| theta * factor
mulAngle (Radians theta) factor = Radians <| theta * factor
{-# INLINE mulAngle #-}

-- | Unpack an 'Angle' to its underlying type.
unpack :: Angle a -> a
unpack (Degrees theta) = theta
unpack (Radians theta) = theta
{-# INLINE unpack #-}
