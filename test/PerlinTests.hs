module PerlinTests
    ( heightAlwaysInRange
    , heightIsZeroIfInt
    ) where

import           Scene.Math

-- | Whatever coordinate given to noise2D the result always shall be in range
-- [-1, 1].
heightAlwaysInRange :: Double -> Double -> Bool
heightAlwaysInRange x z =
    let y = noise2D initPerlin x z
    in y >= -1 && y <= 1

-- | If the coordinates to noise2D are integers the result from the algorithm
-- shall always be zero.
heightIsZeroIfInt :: Int -> Int -> Bool
heightIsZeroIfInt x z =
    let y = noise2D initPerlin (fromIntegral x) (fromIntegral z)
    in y == 0.0
