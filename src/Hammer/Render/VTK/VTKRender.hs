{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Hammer.Render.VTK.VTKRender where

import Prelude hiding ((++), head, last, map, length, FilePath)
import qualified Prelude as P

import Data.Vector
import qualified Data.Vector as Vec

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import qualified Text.XML as X

import Data.Text (Text, pack)
import qualified Data.Text as T

import Filesystem.Path (FilePath)
import Filesystem.Path.CurrentOS (fromText)

import Hammer.Math.Vector hiding (Vector)
import Hammer.Render.VTK.Base
import Hammer.Render.VTK.VTKXMLTemplate


class Foldable cont where
  folder::(a -> b -> a) -> a -> cont b -> a
                
instance Foldable Vector where
  folder = foldl'
  
instance Foldable IntMap where
  folder func = IM.fold (flip func)
  

addDataPoints::(RenderPoint a)=> VTK a -> VTKAttrPoint a -> VTK a
addDataPoints VTK{..} attr = VTK { pointData = attr:pointData, .. }

addDataCells::(RenderPoint a)=> VTK a -> VTKAttrCell a -> VTK a
addDataCells VTK{..} attr = VTK { cellData = attr:cellData, .. }

mkVTK::(RenderPoint a)=> Text -> Bool -> VTKDataSet a -> [VTKAttrPoint a] -> [VTKAttrCell a] -> VTK a
mkVTK = VTK

mkRectLinGrid::(RenderPoint a)=> Vector a ->  Vector a ->  Vector a -> VTKDataSet a
mkRectLinGrid x y z = RectLinGrid { dimRG  = (length x, length y, length z)
                                  , setxRG = x
                                  , setyRG = y
                                  , setzRG = z }

mkUGVTK::(RenderPoint p, RenderCell shape, Foldable cont)=> String -> Vector p -> cont shape -> VTK p
mkUGVTK name points cells = let
  nameTxt = pack name
  dataset = mkUnstructGrid points cells
  in mkVTK nameTxt False dataset [] []

mkUnstructGrid::(RenderPoint p, RenderCell shape, Foldable cont)=> Vector p -> cont shape -> VTKDataSet p
mkUnstructGrid points cells = let
  i = UnstructGrid { setUG      = points
                   , cellUG     = empty
                   , cellOffUG  = empty
                   , cellTypeUG = empty }
  in folder addCell i cells

addCell::(RenderPoint a, RenderCell shape)=> VTKDataSet a -> shape -> VTKDataSet a
addCell set@(UnstructGrid{..}) obj =
  let
    cell      = makeCell obj
    cell_type = getType obj
    cell_size = Vec.length cell
    cell_Off  = if Vec.null cellOffUG then cell_size else (last cellOffUG) + cell_size
  in set { cellUG      = cellUG ++ cell
         , cellOffUG   = cellOffUG `snoc` cell_Off 
         , cellTypeUG  = cellTypeUG `snoc` cell_type }
addCell set _ = set


writeUniVTKfile::(RenderPoint a) => FilePath -> VTK a -> IO ()
writeUniVTKfile name vtk = X.writeFile X.def name (renderVTKUni vtk)
      
writeMultiVTKfile::(RenderPoint a) => FilePath -> Vector (VTK a) -> IO ()
writeMultiVTKfile name vtk = X.writeFile X.def name (renderVTKMulti vtk)

text2Path::String -> FilePath
text2Path = fromText . pack


-- Basic instances
instance RenderCell (Int, Int, Int) where
  makeCell (a,b,c) = Vec.fromList [a,b,c]
  getType _ = VTK_TRIANGLE
     
instance RenderPoint Vec3 where
  renderPoint (Vec3 x y z) = T.unwords [toTxt x, toTxt y, toTxt z]

instance RenderPoint Vec2 where
  renderPoint (Vec2 x y) = T.unwords [toTxt x, toTxt y]
                                                       
                           