{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleContexts           #-}

module Hammer.Math.Algebra
  ( AbelianGroup  (..)
  , MultSemiGroup (..)
  , Ring
  , LeftModule    (..)
  , RightModule   (..)
  , MultiVec      (..)
  , DotProd       (..)
  , CrossProd     (..)
  , normalize
  , distance
  , angle
  , angle'
  , acosSafe
  , UnitVector     (..)
  , Pointwise      (..)
  , Extend         (..)
  , HasCoordinates (..)
  , Dimension      (..)
  , Matrix         (..)
  , Transposable   (..)
  , Inversable     (..)
  , IdMatrix       (..)
  , Tensor         (..)
  , Diagonal       (..)
  , Determinant    (..)
  , Orthogonal     (..)
  , Vec2 (..), Vec3 (..), Vec4 (..)
  , Mat2 (..), Mat3 (..), Mat4 (..)
  , Normal2, Normal3, Normal4
  , mkVec2 , mkVec3 , mkVec4
  , unVec2 , unVec3 , unVec4
  , project , project' , projectUnsafe , flipNormal
  , householder
  , VecFunctor (..)
  , PrettyShow (..)

  , Hessenberg  (..)
  , OrthoMatrix (..)
  , qrGram
  , qrHouse
  , symmEigen

  )
  where

import Hammer.Math.AlgebraMat ()
import Hammer.Math.AlgebraBase
import Hammer.Math.AlgebraVec
import Hammer.Math.MatrixTools
import Hammer.Math.Unboxed
