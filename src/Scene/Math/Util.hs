{-# LANGUAGE BangPatterns #-}
-- |
-- Module: Scene.Math.Util
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.Math.Util
    ( clamp
    , lerp
    , lerp'
    ) where

-- | Clamp the value between the low and the high values. The value to clamp
-- is the third and last argument.
clamp :: Ord a => a -> a -> a -> a
clamp !lo !hi = max lo . min hi
{-# INLINE clamp #-}

-- | Linear interpolation between the left and right values using the amount.
lerp :: (Num a, Ord a) => a -> a -> a -> a
lerp !amount !left !right =
    (1 - amount) * left + amount * right
{-# INLINE lerp #-}

-- | Linear interpolation between the left and right values using the amount.
-- The amount is forcefully clamped to the range [0, 1].
lerp' :: (Num a, Ord a) => a -> a -> a -> a
lerp' !amount = lerp (clamp 0 1 amount)
{-# INLINE lerp' #-}
