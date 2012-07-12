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


import Data.Vec.LinAlg
