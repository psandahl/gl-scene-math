-- |
-- Module: Scene.Math.Matrix
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.Math.Matrix
    ( Application (..)
    , mkPerspectiveMatrix
    , mkViewMatrix
    , mkRotationMatrix
    , mkIdentityMatrix
    , normalMatrix
    , mvpMatrix
    , apply
    ) where

import           Control.Lens           (view)
import           Flow                   ((<|))
import           Linear                 (Epsilon, M33, M44, V3, V4, axisAngle,
                                         fromQuaternion, identity, inv44,
                                         lookAt, m33_to_m44, perspective, point,
                                         transpose, vector, (!*), (!*!), _m33,
                                         _xyz)
import           Scene.Math.Angle       (Angle, toRadians)
import           Scene.Math.AspectRatio (AspectRatio, aspectRatio)

-- | When a 'V3' is applied to a 4x4 matrix, it can be applied either as a point
-- or as a vector.
data Application a
    = Point !(V3 a)
    | Vector !(V3 a)
    deriving (Eq, Show)

-- | Helper function to create a perspective matrix.
mkPerspectiveMatrix :: Floating a => Angle a -> AspectRatio -> a -> a -> M44 a
mkPerspectiveMatrix angle ar = perspective (toRadians angle) (aspectRatio ar)
{-# INLINE mkPerspectiveMatrix #-}

-- | Helper function to create a view matrix. Identical to 'lookAt' from
-- the Linear package, but with the same naming scheme
-- as the other matrix makers in this library.
mkViewMatrix :: (Epsilon a, Floating a) => V3 a -> V3 a -> V3 a -> M44 a
mkViewMatrix = lookAt
{-# INLINE mkViewMatrix #-}

-- | Helper function to create a rotation matrix.
mkRotationMatrix :: (Epsilon a, Floating a) => V3 a -> Angle a -> M44 a
mkRotationMatrix axis = m33_to_m44 . fromQuaternion . axisAngle axis . toRadians
{-# INLINE mkRotationMatrix #-}

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

-- | Apply the (column major) vector to the 4x4 matrix.
apply :: Num a => M44 a -> Application a -> V3 a
apply mat app = view _xyz <| mat !* toV4 app
    where
        toV4 :: Num a => Application a -> V4 a
        toV4 (Point v)  = point v
        toV4 (Vector v) = vector v