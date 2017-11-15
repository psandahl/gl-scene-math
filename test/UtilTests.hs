module UtilTests
    ( clampInt
    , interpolate
    ) where

import           Scene.Math
import           Test.HUnit

clampInt :: Int -> Bool
clampInt x =
    let val = clamp (-10) 10 x
    in val >= (-10) && val <= 10

interpolate :: Assertion
interpolate = do
    (1.0 :: Float) @=? lerp 0 1 3
    (2.0 :: Float) @=? lerp 0.5 1 3
    (3.0 :: Float) @=? lerp 1 1 3
