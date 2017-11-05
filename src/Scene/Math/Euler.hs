-- |
-- Module: Scene.Math.Euler
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.Math.Euler
    ( eulerHeading
    , eulerElevation
    , fromEulerAngles
    ) where

import           Flow              ((<|))
import           Linear            (Epsilon, V3 (..), normalize, (!*!))
import           Scene.Math.Angle  (Angle (..), negateAngle)
import           Scene.Math.Matrix (Application (..), apply, mkRotationMatrix,
                                    mkRotationMatrix)
import           Scene.Math.Vector (back3d, x3d, y3d)

-- | Project a 'V3' on the earth surface (i.e. remote the y component) and
-- take the angle it head towards. An angle of zero is heading in the positive
-- z direction.
eulerHeading :: RealFloat a => V3 a -> Angle a
eulerHeading (V3 x _y z) = Radians <| atan2 x z
{-# INLINE eulerHeading #-}

-- | The elevation of the 'V3' with respect to the horizon. A positive 'Angle'
-- is pointing up, and a negative 'Angle' is pointing down.
eulerElevation :: (Epsilon a, Floating a) => V3 a -> Angle a
eulerElevation vec =
    let V3 _x y _z = normalize vec
        angle = asin y
    in Radians angle
{-# INLINE eulerElevation #-}

-- | From heading and elevation make a new 'V3'. Remember: zero degrees heading
-- is in the positive z direction. A positive elevation is pointing above the
-- horizon and a negativ is pointing below.
fromEulerAngles :: (Epsilon a, Floating a) => Angle a -> Angle a -> V3 a
fromEulerAngles heading elevation =
    let pitch = mkRotationMatrix x3d <| negateAngle elevation
        yaw = mkRotationMatrix y3d heading
        mat = yaw !*! pitch
    in apply mat <| Vector back3d
