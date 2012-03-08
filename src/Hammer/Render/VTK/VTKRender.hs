

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hammer.Render.VTK.VTKRender where

import Prelude hiding ((++), head, last, map, length, FilePath)
import qualified Prelude as P

import Data.Vector
import qualified Data.Vector as Vec

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Text.XML
import qualified Text.XML as X

import Data.Text (Text, pack, append)
import qualified Data.Text as T

import Filesystem.Path (FilePath)
import Filesystem.Path.CurrentOS (fromText)
import qualified Filesystem.Path as FS

import Hammer.Math.Vector hiding (Vector)
import Hammer.Render.VTK.Base
import Hammer.Render.VTK.VTKXMLTemplate

                
addDataPoints::VTK -> String -> (Int -> Vec3 -> VTKAttr) -> VTK
addDataPoints vtk name func = vtk { pointData = (name, func):(pointData vtk) }

addScalarCells::VTK -> String -> (Int -> Vector Vec3 -> CellType -> VTKAttr) -> VTK
addScalarCells vtk name func = vtk { cellData = (name, func):(cellData vtk) }

mkVTK::Text -> Bool -> VTKDataSet -> [VTKAttrPoint] -> [VTKAttrCell] -> VTK
mkVTK = VTK


mkRectLinGrid:: Vector Vec3 ->  Vector Vec3 ->  Vector Vec3 -> VTKDataSet
mkRectLinGrid x y z = RectLinGrid { dimRG  = (length x, length y, length z)
                                  , setxRG = x
                                  , setyRG = y
                                  , setzRG = z }

mkUGVTK::(RenderCell shape)=> String -> Vector Vec3 -> Vector shape -> VTK 
mkUGVTK name points cells = let
  nameTxt = pack name
  dataset = mkUnstructGrid points cells
  in mkVTK nameTxt False dataset [] []

mkUnstructGrid::(RenderCell shape)=> Vector Vec3 -> Vector shape -> VTKDataSet
mkUnstructGrid points cells = let
  init = UnstructGrid { setUG      = points
                      , cellUG     = empty
                      , cellOffUG  = empty
                      , cellTypeUG = empty }
  in foldl' addCell init cells

addCell::(RenderCell shape)=> VTKDataSet -> shape -> VTKDataSet
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


writeUniVTKfile::FilePath -> VTK -> IO ()
writeUniVTKfile name vtk = X.writeFile X.def name (renderVTKUni vtk)
      
writeMultiVTKfile::FilePath -> Vector VTK -> IO ()
writeMultiVTKfile name vtk = X.writeFile X.def name (renderVTKMulti vtk)


instance RenderCell (Int, Int, Int) where
  makeCell (a,b,c) = Vec.fromList [a,b,c]
  getType _ = VTK_TRIANGLE
  
ug = let
  ps = fromList [(Vec3 0 0 0),(Vec3 1 0 0),(Vec3 0 1 0),(Vec3 0 0 2)]
  ss = fromList [(0, 1, 2),(0, 1, 3),(1, 2, 3), ( (2, 0, 3)::(Int,Int,Int) )]
  in mkUGVTK "test" ps ss
     
text2Path= fromText . pack