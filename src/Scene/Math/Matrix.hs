-- |
-- Module: Scene.Math.Matrix
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.Math.Matrix
    ( mkPerspectiveMatrix
    , mkViewMatrix
    , mkIdentityMatrix
    , normalMatrix
    , mvpMatrix
    ) where

import           Control.Lens           (view)
import           Linear                 (Epsilon, M33, M44, V3, identity, inv44,
                                         lookAt, perspective, transpose, (!*!),
                                         _m33)
import           Scene.Math.Angle       (Angle, toRadians)
import           Scene.Math.AspectRatio (AspectRatio, aspectRatio)

-- | Helper function to create a perspective matrix.
mkPerspectiveMatrix :: Floating a => Angle a -> AspectRatio -> a -> a -> M44 a
mkPerspectiveMatrix angle ar = perspective (toRadians angle) (aspectRatio ar)
{-# INLINE mkPerspectiveMatrix #-}

-- | Helper function to create a view matrix. Identical to 'lookAt' from
-- the Linear package, but with the same naming scheme
-- as the other in this library.
mkViewMatrix :: (Epsilon a, Floating a) => V3 a -> V3 a -> V3 a -> M44 a
mkViewMatrix = lookAt
{-# INLINE mkViewMatrix #-}

-- | Helper function to create an identity matrix. Identical to 'identity'
-- from the Linear, but with the same naming scheme as the other in this library.
mkIdentityMatrix :: Floating a => M44 a
mkIdentityMatrix = identity
{-# INLINE mkIdentityMatrix #-}

-- | Calculate a 3x3 normal matrix.
normalMatrix :: Fractional a => M44 a -> M33 a
normalMatrix = view _m33 . transpose . inv44
{-# INLINE normalMatrix #-}

-- | Concatenate the model, view and projection matrices into the mvp matrix.
mvpMatrix :: Num a => M44 a -> M44 a -> M44 a -> M44 a
mvpMatrix m v p = p !*! v !*! m
{-# INLINE mvpMatrix #-}
