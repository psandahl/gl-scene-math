module UtilTests
    ( clampInt
    ) where

import           Scene.Math

clampInt :: Int -> Bool
clampInt x =
    let val = clamp (-10) 10 x
    in val >= (-10) && val <= 10
