{-# OPTIONS_GHC -fno-warn-orphans #-}
module EulerTests
    ( eulerHeadings
    , eulerElevations
    , fromVectorAndBackAgain
    ) where

import           Linear          (Epsilon, V3 (..), nearZero, normalize)
import           Scene.Math
import           System.Random   (Random)
import           Test.HUnit      (Assertion, (@=?))
import           Test.QuickCheck

-- | Generate a random 3D direction vector. Make sure the vector is normalized,
-- as the vector constructed from fromEulerAngles is normalized.
instance (Epsilon a, Floating a, Random a) => Arbitrary (V3 a) where
    arbitrary = do
        vec <- V3 <$> choose (-1, 1) <*> choose (-1, 1) <*> choose (-1, 1)
        return $ normalize vec

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

-- | From a random 'V3', calculate euler angles and from them generate a new
-- 'V3'. Shall be 'nearVectors' to the original.
fromVectorAndBackAgain :: V3 Float -> Bool
fromVectorAndBackAgain vec =
    let heading = eulerHeading vec
        elevation = eulerElevation vec
        vec' = fromEulerAngles heading elevation
    in vec `nearVectors` vec'

nearVectors :: Epsilon a => V3 a -> V3 a -> Bool
nearVectors (V3 x y z) (V3 x' y' z') =
    nearZero (x - x') && nearZero (y - y') && nearZero (z - z')
