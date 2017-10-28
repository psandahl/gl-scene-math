-- |
-- Module: Scene.Math.Surface
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.Math.Surface
    ( surfaceNormal
    ) where

import           Flow   ((<|))
import           Linear (Epsilon, V3, cross, normalize)

-- | The normal of the surface given by the vertices' triangle. The assumption
-- is that the triangle's vertices are given in the same order as the triangle
-- is rendered.
surfaceNormal :: (Epsilon a, Floating a) =>  V3 a -> V3 a -> V3 a -> V3 a
surfaceNormal v1 v2 v3 =
    let vec1 = v2 - v1
        vec2 = v3 - v1
    in normalize <| vec1 `cross` vec2
