module SurfaceTests
    ( heightsInFlatTriangle
    ) where

import           Linear     (V3 (..))
import           Scene.Math
import           Test.HUnit

-- | Height calculation in a perfectly flat triangle with the height of zero
-- over the whole area.
heightsInFlatTriangle :: Assertion
heightsInFlatTriangle = do
    let p1 = V3 0 0 0 :: V3 Float
        p2 = V3 1 0 0
        p3 = V3 0 0 1
    0 @=? baryCentricHeight p1 p2 p3 0 0
    0 @=? baryCentricHeight p1 p2 p3 1 0
    0 @=? baryCentricHeight p1 p2 p3 0 1
    0 @=? baryCentricHeight p1 p2 p3 0.5 0.25
