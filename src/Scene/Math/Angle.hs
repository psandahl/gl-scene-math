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
    , negateAngle
    , unpack
    , eulerHeading
    , eulerElevation
    ) where

import           Flow   ((<|))
import           Linear (Epsilon, V3 (..), normalize)

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

-- | Negate an angle.
negateAngle :: Num a => Angle a -> Angle a
negateAngle (Degrees theta) = Degrees <| negate theta
negateAngle (Radians theta) = Radians <| negate theta
{-# INLINE negateAngle #-}

-- | Unpack an 'Angle' to its underlying type.
unpack :: Angle a -> a
unpack (Degrees theta) = theta
unpack (Radians theta) = theta
{-# INLINE unpack #-}

-- | Project a 'V3' on the earth surface (i.e. remote the y component) and
-- take the angle it head towards. An angle of zero is heading in the positive
-- z direction.
eulerHeading :: RealFloat a => V3 a -> Angle a
eulerHeading (V3 x _y z) = Radians <| atan2 x z
{-# INLINE eulerHeading #-}

-- | The elevation of the 'V3' with respect to the horizon.
eulerElevation :: (Epsilon a, Floating a) => V3 a -> Angle a
eulerElevation vec =
    let V3 _x y _z = normalize vec
        angle = asin y
    in Radians angle
{-# INLINE eulerElevation #-}
