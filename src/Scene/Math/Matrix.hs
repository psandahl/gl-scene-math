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
    , mkTranslationMatrix
    , mkScalingMatrix
    , mkIdentityMatrix
    , normalMatrix
    , mvpMatrix
    , srtMatrix
    , apply
    ) where

import           Control.Lens           (view)
import           Flow                   ((<|))
import           Linear                 (Epsilon, M33, M44, V3 (..), V4 (..),
                                         axisAngle, fromQuaternion, identity,
                                         inv44, lookAt, m33_to_m44, perspective,
                                         point, transpose, vector, (!*), (!*!),
                                         _m33, _xyz)
import           Scene                  (Viewport)
import           Scene.Math.Angle       (Angle, toRadians, unpack)
import           Scene.Math.AspectRatio (aspectRatio)

-- | When a 'V3' is applied to a 4x4 matrix, it can be applied either as a point
-- or as a vector.
data Application a
    = Point !(V3 a)
    | Vector !(V3 a)
    deriving (Eq, Show)

-- | Helper function to create a perspective matrix from a viewing 'Angle',
-- a 'Viewport' and the near and far planes.
mkPerspectiveMatrix :: Floating a => Angle a -> Viewport -> a -> a -> M44 a
mkPerspectiveMatrix angle ar = perspective (unpack <| toRadians angle) (aspectRatio ar)
{-# INLINE mkPerspectiveMatrix #-}

-- | Helper function to create a view matrix. Identical to 'lookAt' from
-- the Linear package, but with the same naming scheme
-- as the other matrix makers in this library.
mkViewMatrix :: (Epsilon a, Floating a) => V3 a -> V3 a -> V3 a -> M44 a
mkViewMatrix = lookAt
{-# INLINE mkViewMatrix #-}

-- | Helper function to create a rotation matrix.
mkRotationMatrix :: (Epsilon a, Floating a) => V3 a -> Angle a -> M44 a
mkRotationMatrix axis = m33_to_m44 . fromQuaternion . axisAngle axis . unpack . toRadians
{-# INLINE mkRotationMatrix #-}

-- | Helper function to create a translation matrix.
mkTranslationMatrix :: Num a => V3 a -> M44 a
mkTranslationMatrix (V3 x y z) =
    V4 (V4 1 0 0 x)
       (V4 0 1 0 y)
       (V4 0 0 1 z)
       (V4 0 0 0 1)

-- | Helper function to create a scaling matrix.
mkScalingMatrix :: Num a => V3 a -> M44 a
mkScalingMatrix (V3 x y z) =
    V4 (V4 x 0 0 0)
       (V4 0 y 0 0)
       (V4 0 0 z 0)
       (V4 0 0 0 1)

-- | Helper function to create an identity matrix. Identical to 'identity'
-- from the Linear, but with the same naming scheme as the other in this library.
mkIdentityMatrix :: Num a => M44 a
mkIdentityMatrix = identity
{-# INLINE mkIdentityMatrix #-}

-- | Calculate a 3x3 normal matrix.
normalMatrix :: Fractional a => M44 a -> M33 a
normalMatrix = view _m33 . transpose . inv44
{-# INLINE normalMatrix #-}

-- | Concatenate the model, view and perspective matrices into the mvp matrix.
-- The matrices *must* be given in this exact argument order; model, view and
-- perspective.
mvpMatrix :: Num a => M44 a -> M44 a -> M44 a -> M44 a
mvpMatrix m v p = p !*! v !*! m
{-# INLINE mvpMatrix #-}

-- | Concatenate scale, rotate and translate model matrices to a final model
-- matrix. The matrices *must* be given in this exact argument order;
-- scale, rotate and translate.
srtMatrix :: Num a => M44 a -> M44 a -> M44 a -> M44 a
srtMatrix s r t = t !*! r !*! s
{-# INLINE srtMatrix #-}

-- | Apply the (column major) vector to the 4x4 matrix.
apply :: Num a => M44 a -> Application a -> V3 a
apply mat app = view _xyz <| mat !* toV4 app
    where
        toV4 :: Num a => Application a -> V4 a
        toV4 (Point v)  = point v
        toV4 (Vector v) = vector v
