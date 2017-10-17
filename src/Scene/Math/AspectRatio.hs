-- |
-- Module: Scene.Math.AspectRatio
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.Math.AspectRatio
    ( AspectRatio (..)
    , aspectRatio
    ) where

data AspectRatio = AspectRatio
    { width  :: !Int
    , height :: !Int
    } deriving (Eq, Show)

aspectRatio :: Floating a => AspectRatio -> a
aspectRatio ar =
    fromIntegral (width ar) / fromIntegral (height ar)
{-# INLINE aspectRatio #-}
