-- |
-- Module: Scene.Math.Util
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.Math.Util
    ( clamp
    ) where

-- | Clamp the value between the low and the high values. The value to clamp
-- is the third and last argument.
clamp :: Ord a => a -> a -> a -> a
clamp lo hi = max lo . min hi
{-# INLINE clamp #-}
