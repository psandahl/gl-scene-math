module AngleTests
    ( convertDegreesToDegrees
    , convertRadiansToDegrees
    , convertRadiansToRadians
    , convertDegreesToRadians
    , addDegreesToDegrees
    , addRadiansToRadians
    , addRadiansToDegrees
    , addDegreesToRadians
    , mulDegrees
    , mulRadians
    , eulerHeadings
    , eulerElevations
    ) where

import           Flow       ((<|))
import           Linear     (V3)
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

-- | Add degrees to degrees.
addDegreesToDegrees :: Assertion
addDegreesToDegrees =
    Degrees 360 @=? addAngles (Degrees 180) (Degrees 180 :: Angle Float)

-- | Add radians to radians
addRadiansToRadians :: Assertion
addRadiansToRadians =
    Radians (pi * 2) @=? addAngles (Radians pi) (Radians pi :: Angle Float)

-- | Add radians to degrees.
addRadiansToDegrees :: Assertion
addRadiansToDegrees =
    Degrees 360 @=? addAngles (Degrees 180) (Radians pi :: Angle Float)

-- | Add degrees to radians.
addDegreesToRadians :: Assertion
addDegreesToRadians =
    Radians (pi * 2) @=? addAngles (Radians pi) (Degrees 180 :: Angle Float)

-- | Multiply degrees.
mulDegrees :: Assertion
mulDegrees =
    Degrees 180 @=? mulAngle (Degrees 90 :: Angle Float) 2

-- | Multiply radians.
mulRadians :: Assertion
mulRadians =
    Radians pi @=? mulAngle (Radians (pi / 2) :: Angle Float) 2

-- | Test the calculation of Euler heading angles.
eulerHeadings :: Assertion
eulerHeadings = do
    Radians 0 @=? eulerHeading (back3d :: V3 Float)
    Radians (pi / 2) @=? eulerHeading (right3d :: V3 Float)
    Radians pi @=? eulerHeading (front3d :: V3 Float)
    Radians (-pi / 2) @=? eulerHeading (left3d :: V3 Float)

-- | Test the calculation of Euler elevation angles.
eulerElevations :: Assertion
eulerElevations = do
    Radians 0 @=? eulerElevation (left3d :: V3 Float)
    Radians 0 @=? eulerElevation (right3d :: V3 Float)
    Radians (pi / 2) @=? eulerElevation (up3d :: V3 Float)
    Radians (-pi / 2) @=? eulerElevation (down3d :: V3 Float)
