{-# OPTIONS_GHC -fno-warn-orphans #-}
module EulerTests
    ( eulerHeadings
    , eulerElevations
    , fromAnglesAndBackAgain
    ) where

import           Linear          (V3 (..))
import           Scene.Math
import           Test.HUnit      (Assertion, (@=?))
import           Test.QuickCheck

instance Arbitrary a => Arbitrary (Angle a) where
    arbitrary = Radians <$> arbitrary

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

fromAnglesAndBackAgain :: Angle Float -> Angle Float -> Bool
fromAnglesAndBackAgain heading elevation =
    let vec = fromEulerAngles heading elevation
        heading' = eulerHeading vec
        elevation' = eulerElevation vec
    in heading == heading' && elevation == elevation'
