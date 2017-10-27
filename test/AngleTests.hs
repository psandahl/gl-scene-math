module AngleTests
    ( convertDegreesToDegrees
    , convertRadiansToDegrees
    , convertRadiansToRadians
    , convertDegreesToRadians
    ) where

import           Flow       ((<|))
import           Scene.Math
import           Test.HUnit (Assertion, (@=?))

-- | Converting degrees to degrees shall be no conversion.
convertDegreesToDegrees :: Assertion
convertDegreesToDegrees = do
    Degrees 0 @=? (toDegrees <| Degrees 0 :: Angle Float)
    Degrees 180 @=? (toDegrees <| Degrees 180 :: Angle Float)
    Degrees 360 @=? (toDegrees <| Degrees 360 :: Angle Float)

-- | Convert from degrees to radians.
convertRadiansToDegrees :: Assertion
convertRadiansToDegrees = do
    Degrees 0 @=? (toDegrees <| Radians 0 :: Angle Float)
    Degrees 90 @=? (toDegrees <| Radians (pi / 2) :: Angle Float)
    Degrees 180 @=? (toDegrees <| Radians pi :: Angle Float)
    Degrees 270 @=? (toDegrees <| Radians (pi + pi / 2) :: Angle Float)
    Degrees 360 @=? (toDegrees <| Radians (pi * 2) :: Angle Float)

-- | Converting radians to radians shall be nor conversion.
convertRadiansToRadians :: Assertion
convertRadiansToRadians = do
    Radians 0 @=? (toRadians <| Radians 0 :: Angle Float)
    Radians pi @=? (toRadians <| Radians pi :: Angle Float)
    Radians (2 * pi) @=? (toRadians <| Radians (pi * 2) :: Angle Float)

-- | Converting degrees to radians.
convertDegreesToRadians :: Assertion
convertDegreesToRadians = do
    Radians 0 @=? (toRadians <| Degrees 0 :: Angle Float)
    Radians (pi / 2) @=? (toRadians <| Degrees 90 :: Angle Float)
    Radians pi @=? (toRadians <| Degrees 180 :: Angle Float)
    Radians (pi + pi / 2) @=? (toRadians <| Degrees 270 :: Angle Float)
    Radians (pi * 2) @=? (toRadians <| Degrees 360 :: Angle Float)
