{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Hammer.Math.Algebra
  ( AbelianGroup(..)
  , vecSum
  , MultSemiGroup(..)
  , Ring
  , semigroupProduct
  , LeftModule(..)
  , RightModule(..)
  , MultiVec (..)
  , DotProd(..)
  , CrossProd(..)
  , normalize
  , distance
  , angle
  , angle'
  , UnitVector (..)
  , Pointwise (..)
  , Extend (..)
  , HasCoordinates (..)
  , Dimension (..)
  , Matrix (..)
  , Tensor (..)
  , Diagonal (..)
  , Determinant (..)
  , Orthogonal (..)
  , Projective (..)
  , MatrixNorms (..)
  , Vec2 (..), Vec3 (..), Vec4 (..)
  , Mat2 (..), Mat3 (..), Mat4 (..)
  , Ortho2, Ortho3, Ortho4
  , Normal2, Normal3, Normal4
  , Proj3, Proj4
  , mkVec2 , mkVec3 , mkVec4
  , project , project' , projectUnsafe , flipNormal
  , householder, householderOrtho
  )
  where

import Hammer.Math.AlgebraMat
import Hammer.Math.AlgebraBase
import Hammer.Math.AlgebraVec

