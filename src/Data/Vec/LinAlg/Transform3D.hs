-----------------------------------------------------------------------------
--
-- Module      :  Data.Vec.LinAlg.Transform3D
-- Copyright   :  Tobias Bexelius
-- License     :  BSD3
--
-- Maintainer  :  Tobias Bexelius
-- Stability   :
-- Portability :
--
-- |
-- Some 4x4 transformation matrices, using a right handed coordinate system.
-- These matrices are used by multiplying vectors from the right.
--
-- The projection matrices will produce vectors in a left handed coordinate system, i.e. where z goes into the screen.
-----------------------------------------------------------------------------

module Data.Vec.LinAlg.Transform3D (
    translation,
    rotationX,
    rotationY,
    rotationZ,
    rotationVec,
    rotationEuler,
    rotationQuat,
    rotationLookAt,
    scaling,
    perspective,
    orthogonal,
) where
import Data.Vec

-- | A 4x4 translation matrix
translation :: Num a => Vec3 a -> Mat44 a
translation = flip translate identity

-- | A 4x4 rotation matrix for a rotation around the X axis
rotationX :: Floating a
          => a -- ^ The angle in radians
          -> Mat44 a
rotationX a  = matFromList [1, 0, 0, 0,
                            0, cos a, -sin a, 0,
                            0, sin a, cos a, 0,
                            0, 0, 0, 1]

-- | A 4x4 rotation matrix for a rotation around the Y axis
rotationY :: Floating a
          => a -- ^ The angle in radians
          -> Mat44 a
rotationY a  = matFromList [cos a, 0, sin a, 0,
                            0, 1, 0, 0,
                            -sin a, 0, cos a, 0,
                            0, 0, 0, 1]

-- | A 4x4 rotation matrix for a rotation around the Z axis
rotationZ :: Floating a
          => a -- ^ The angle in radians
          -> Mat44 a
rotationZ a  = matFromList [cos a, -sin a, 0, 0,
                            sin a, cos a, 0, 0,
                            0, 0, 1, 0,
                            0, 0, 0, 1]

-- | A 4x4 rotation matrix for a rotation around an arbitrary normalized vector
rotationVec :: Floating a
            => Vec3 a  -- ^ The normalized vector around which the rotation goes
            -> a  -- ^ The angle in radians
            -> Mat44 a
rotationVec (x:.y:.z:.()) a =
    matFromList [x^2+(1-x^2)*c, x*y*(1-c)-z*s, x*z*(1-c)+y*s, 0,
                 x*y*(1-c)+z*s, y^2+(1-y^2)*c, y*z*(1-c)-x*s, 0,
                 x*z*(1-c)-y*s, y*z*(1-c)+x*s, z^2+(1-z^2)*c, 0,
                 0, 0, 0, 1]
    where c = cos a
          s = sin a

-- | A 4x4 rotation matrix from the euler angles yaw pitch and roll. Could be useful in e.g.
--   first person shooter games,
rotationEuler :: Floating a
              => Vec3 a -- rotation around x, y and z respectively
              -> Mat44 a
rotationEuler (x:.y:.z:.()) = rotationZ z `multmm` rotationY y `multmm` rotationX x

-- | A 4x4 rotation matrix from a normalized quaternion. Useful for most free flying rotations, such as airplanes.
rotationQuat :: Num a
             => Vec4 a -- ^ The quaternion with the real part (w) last
             ->  Mat44 a
rotationQuat (x:.y:.z:.w:.()) =
    matFromList [1-2*y^2-2*z^2, 2*(x*y-z*w), 2*(x*z+y*w), 0,
                 2*(x*y+z*w), 1-2*x^2-2*z^2, 2*(y*z-x*w), 0,
                 2*(x*z-y*w), 2*(x*w+y*z), 1-2*x^2-2*y^2, 0,
                 0, 0, 0, 1]

-- | A 4x4 rotation matrix for turning toward a point. Useful for targeting a camera to a specific point.
rotationLookAt :: Floating a
               => Vec3 a -- ^ The up direction, not necessary unit length or perpendicular to the view vector
               -> Vec3 a -- ^ The viewers position
               -> Vec3 a -- ^ The point to look at
               -> Mat44 a
rotationLookAt up' pos target = transpose $ homVec left :. homVec up :. homVec forward :. homPoint 0 :. ()
    where
        forward = normalize $ pos - target
        left = normalize $ up' `cross` forward
        up = forward `cross`left

-- | A 4x4 scaling matrix
scaling :: Num a => Vec3 a -> Mat44 a
scaling = diagonal . homPoint

-- | A perspective projection matrix for a right handed coordinate system looking down negative z. This will project far plane to @z = +1@ and near plane to @z = -1@, i.e. into a left handed system.
perspective :: Floating a
            => a -- ^ Near plane clipping distance (always positive)
            -> a -- ^ Far plane clipping distance (always positive)
            -> a -- ^ Field of view of the y axis, in radians
            -> a -- ^ Aspect ratio, i.e. screen's width\/height
            -> Mat44 a
perspective n f fovy aspect = matFromList [2*n/(r-l), 0, -(r+l)/(r-l), 0,
                                           0, 2*n/(t-b), (t+b)/(t-b), 0,
                                           0, 0, -(f+n)/(f-n), -2*f*n/(f-n),
                                           0,0,-1,0]
    where
        t = n*tan(fovy/2)
        b = -t
        r = aspect*t
        l = -r

-- | An orthogonal projection matrix for a right handed coordinate system looking down negative z. This will project far plane to @z = +1@ and near plane to @z = -1@, i.e. into a left handed system.
orthogonal :: Fractional a
           => a -- ^ Near plane clipping distance
           -> a -- ^ Far plane clipping distance
           -> Vec2 a -- ^ The size of the view (center aligned around origo)
           -> Mat44 a
orthogonal n f (w:.h:.()) = matFromList [2/w, 0, 0, 0,
                                         0, 2/h, 0, 0,
                                         0, 0, 2/(f-n), -(f+n)/(f-n),
                                         0, 0, 0, 1]
