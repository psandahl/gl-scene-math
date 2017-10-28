-- |
-- Module: Scene.Math.AspectRatio
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.Math.AspectRatio
    ( aspectRatio
    ) where

import           Scene (Viewport (..))

-- | Calculate an aspect ratio for a 'Viewport'.
aspectRatio :: Floating a => Viewport -> a
aspectRatio viewport =
    fromIntegral (width viewport) / fromIntegral (height viewport)
{-# INLINE aspectRatio #-}
