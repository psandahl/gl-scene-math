-- |
-- Module: Scene.Math.Vector
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.Math.Vector
    ( x3d
    , y3d
    , z3d
    , up3d
    , down3d
    , left3d
    , right3d
    , back3d
    , front3d
    , origo3d
    ) where

import           Linear (V3 (..))

-- | A 3D normal vector along the x-axis.
x3d :: Floating a => V3 a
x3d = V3 1 0 0
{-# INLINE x3d #-}

-- | A 3D normal vector along the y-axis.
y3d :: Floating a => V3 a
y3d = V3 0 1 0
{-# INLINE y3d #-}

-- | A 3D normal vector along the z-axis.
z3d :: Floating a => V3 a
z3d = V3 0 0 1
{-# INLINE z3d #-}

-- | A 3D normal vector point upwards, i.e. in the positive y direction.
up3d :: Floating a => V3 a
up3d = y3d
{-# INLINE up3d #-}

-- | A 3D normal vector pointing downwards, i.e. in the negative y direction.
down3d :: Floating a => V3 a
down3d = negate y3d
{-# INLINE down3d #-}

-- | A 3D normal vector pointing left, i.e. in the negative x direction.
left3d :: Floating a => V3 a
left3d = negate x3d
{-# INLINE left3d #-}

-- | A 3D normal vector pointing right, i.e. in the positive x direction.
right3d :: Floating a => V3 a
right3d = x3d
{-# INLINE right3d #-}

-- | A 3D normal vector pointing backwards, i.e. in the positive z direction.
back3d :: Floating a => V3 a
back3d = z3d
{-# INLINE back3d #-}

-- | A 3D normal vector pointing to the front, i.e. in the negative z direction.
front3d :: Floating a => V3 a
front3d = negate z3d
{-# INLINE front3d #-}

-- | A 3D vector describing the origo.
origo3d :: Floating a => V3 a
origo3d = V3 0 0 0
{-# INLINE origo3d #-}
