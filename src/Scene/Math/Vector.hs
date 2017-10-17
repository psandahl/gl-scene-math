-- |
-- Module: Scene.Math.Vector
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.Math.Vector
    ( up3d
    , down3d
    , left3d
    , right3d
    , back3d
    , front3d
    , origo3d
    ) where

import           Linear (V3 (..))

-- | A normal 3D vector point upwards, i.e. in the positive y direction.
up3d :: Floating a => V3 a
up3d = V3 0 1 0
{-# INLINE up3d #-}

-- | A normal 3D vector pointing downwards, i.e. in the negative y direction.
down3d :: Floating a => V3 a
down3d = V3 0 (-1) 0
{-# INLINE down3d #-}

-- | A normal 3D vector pointing left, i.e. in the negative x direction.
left3d :: Floating a => V3 a
left3d = V3 (-1) 0 0
{-# INLINE left3d #-}

-- | A normal 3D vector pointing right, i.e. in the positive x direction.
right3d :: Floating a => V3 a
right3d = V3 1 0 0
{-# INLINE right3d #-}

-- | A normal 3D vector pointing backwards, i.e. in the positive z direction.
back3d :: Floating a => V3 a
back3d = V3 0 0 1
{-# INLINE back3d #-}

-- | A normal 3D vector pointing to the front, i.e. in the negative z direction.
front3d :: Floating a => V3 a
front3d = V3 0 0 (-1)
{-# INLINE front3d #-}

-- | A 3D vector describing the origo.
origo3d :: Floating a => V3 a
origo3d = V3 0 0 0
{-# INLINE origo3d #-}
