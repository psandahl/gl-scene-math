{-# LANGUAGE BangPatterns #-}
-- |
-- Module: Scene.Math.Perlin
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.Math.Perlin
    ( Perlin
    , Weight (..)
    , initPerlin
    , noise2D
    , composedNoise2D
    , normalizeToGLfloat
    ) where

import           Data.Bits           ((.&.))
import           Data.List           (foldl')
import           Data.Vector.Unboxed (Vector, fromList, (!))
import           Flow                ((<|))
import           Prelude             hiding (init)
import           Scene               (GLfloat)
import           Scene.Math.Util     (clamp, lerp)

-- | Permutation table used for Perlin noise generation. Precalculate the table.
newtype Perlin = Perlin (Vector Int)
    deriving Show

-- | A weight used for composed noise generation. A pair of doubles where the
-- the first double is a frequency multiplier, and the other is an altitude
-- multiplier.
data Weight = Weight !Double !Double
    deriving Show

-- | Initialize the permutation table.
initPerlin :: Perlin
initPerlin = Perlin <| fromList (permutations ++ permutations)

-- | 2D noise as specified by Ken Perlin's improved algorithm.
noise2D :: Perlin -> Double -> Double -> Double
noise2D (Perlin table) !x !y =
    let xi = floor x .&. 255
        yi = floor y .&. 255

        g1 = table ! ((table ! xi) + yi)
        g2 = table ! ((table ! (xi + 1)) + yi)
        g3 = table ! ((table ! xi) + yi + 1)
        g4 = table ! ((table ! (xi + 1)) + yi + 1)

        xf = x - toDouble (floor x)
        yf = y - toDouble (floor y)

        d1 = grad g1 xf yf
        d2 = grad g2 (xf - 1) yf
        d3 = grad g3 xf (yf - 1)
        d4 = grad g4 (xf - 1) (yf - 1)

        u = fade xf
        v = fade yf

        x1Inter = lerp u d1 d2
        x2Inter = lerp u d3 d4
    in lerp v x1Inter x2Inter
    where
        toDouble :: Int -> Double
        toDouble = fromIntegral

fade :: Double -> Double
fade !t = t * t * t * (t * (t * 6 - 15) + 10)

grad :: Int -> Double -> Double -> Double
grad !hash !x !y =
    case hash .&. 3 of
        0 -> x + y
        1 -> (-x) + y
        2 -> x - y
        3 -> (-x) - y
        _ -> 0

-- | Compose noise for several frequences and altitudes 'Weight's.
composedNoise2D :: Perlin -> Double -> Double -> [Weight] -> Double
composedNoise2D perlin !x !y =
    foldl' (\acc (Weight freq altitude) ->
        acc + altitude * noise2D perlin (x * freq) (y * freq)
        ) 0
{-# INLINE composedNoise2D #-}

-- | Take a Double, clamp it to the range [-1, 1], squeeze the range to [0, 1]
-- and convert to a GLfloat.
normalizeToGLfloat :: Double -> GLfloat
normalizeToGLfloat = realToFrac . transformRange . clamp (-1) 1
{-# INLINE normalizeToGLfloat #-}

transformRange :: Double -> Double
transformRange !value = (value + 1) / 2

-- | Permutations table. Magic numbers.
permutations :: [Int]
permutations =
    [ 151,160,137,91,90,15
    , 131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,142,8,99,37,240,21,10,23
    , 190, 6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,35,11,32,57,177,33
    , 88,237,149,56,87,174,20,125,136,171,168, 68,175,74,165,71,134,139,48,27,166
    , 77,146,158,231,83,111,229,122,60,211,133,230,220,105,92,41,55,46,245,40,244
    , 102,143,54, 65,25,63,161, 1,216,80,73,209,76,132,187,208, 89,18,169,200,196
    , 135,130,116,188,159,86,164,100,109,198,173,186, 3,64,52,217,226,250,124,123
    , 5,202,38,147,118,126,255,82,85,212,207,206,59,227,47,16,58,17,182,189,28,42
    , 223,183,170,213,119,248,152, 2,44,154,163, 70,221,153,101,155,167, 43,172,9
    , 129,22,39,253, 19,98,108,110,79,113,224,232,178,185, 112,104,218,246,97,228
    , 251,34,242,193,238,210,144,12,191,179,162,241, 81,51,145,235,249,14,239,107
    , 49,192,214, 31,181,199,106,157,184, 84,204,176,115,121,50,45,127, 4,150,254
    , 138,236,205,93,222,114,67,29,24,72,243,141,128,195,78,66,215,61,156,180
    ]
