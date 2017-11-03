module EulerTests
    ( eulerHeadings
    , eulerElevations
    ) where

import           Linear     (V3 (..))
import           Scene.Math
import           Test.HUnit (Assertion, (@=?))

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
