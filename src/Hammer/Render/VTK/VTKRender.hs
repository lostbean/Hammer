{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# OPTIONS_GHC -fno-warn-orphans      #-}

-- | This module renders VTK files in XML format with ASCII and binary data. VTK file
-- format is used to visualize scientific data in three (less usually in two) dimensions.
-- One of the more important viewers for VTK files is Paraview <http://www.paraview.org/>.
-- More information about the VTK format can be found here:
-- <http://www.vtk.org/VTK/img/file-formats.pdf>
module Hammer.Render.VTK.VTKRender
  ( VTK (..)
    -- * Write to file
  , writeUniVTKfile
  , writeMultiVTKfile
    -- * Make VTK
  , mkUGVTK
  , mkRLGVTK
  , mkSGVTK
  , mkSPVTK
    -- * Add attributes
  , mkCellAttr
  , mkPointAttr
  , addDataPoints
  , addDataCells
    -- * Classes for new attributes
  , CellType    (..)
  , RenderCell  (..)
  , RenderPoint (..)
  , RenderElemVTK
  , VTKAttrPoint
  , VTKAttrCell
  ) where

import qualified Data.ByteString.Lazy             as BSL
import qualified Data.IntMap                      as IM
import qualified Data.List                        as L
import qualified Data.Vector                      as V
import qualified Data.Vector.Unboxed              as U
import qualified Text.XML.Generator               as X

import           Data.IntMap                      (IntMap)
import           Data.Text                        (pack)
import           Data.Vector.Unboxed              (Vector)

import           Hammer.Render.VTK.Types
import           Hammer.Render.VTK.VTKXMLTemplate

-- =======================================================================================

-- | Write an unitary piece VTK file to disk. Set 'True' for binary format.
writeUniVTKfile :: (RenderElemVTK a) => FilePath -> Bool -> VTK a -> IO ()
writeUniVTKfile name isBinary = BSL.writeFile name . X.xrender . renderVTKUni isBinary

-- | Write an multi-piece piece VTK file to disk. Set 'True' for binary format.
writeMultiVTKfile :: (RenderElemVTK a) => FilePath -> Bool -> V.Vector (VTK a) -> IO ()
writeMultiVTKfile name isBinary = BSL.writeFile name . X.xrender . renderVTKMulti isBinary

-- | Creates an Unstructured Grid dataset. Use:
--
-- > mkUGVTK "name" (Vec.fromList [Vec3 0 0 0, Vec3 0 1 0]) [(0,1)]
--
mkUGVTK :: (RenderElemVTK p, RenderCell shape, Foldable cont shape)=>
           String -> Vector p -> cont shape -> VTK p
mkUGVTK name points cells = let
  nameTxt = pack name
  dataset = mkUnstructGrid points cells
  in VTK nameTxt dataset [] []

-- | Creates an Rectilinear Grid dataset. Use:
--
-- > mkRLGVTK "name" (Vec.fromList [0,1,2]) (Vec.fromList [0,1,2]) ([(0 0 0), (0 1 0)])
--
mkRLGVTK :: String -> Vector Double -> Vector Double -> Vector Double -> VTK Double
mkRLGVTK name px py pz = let
  nameTxt = pack name
  dataset = mkRectLinGrid px py pz
  in VTK nameTxt dataset [] []

-- | Creates an Structured Grid dataset. Use:
--
-- > mkSGVTK "name" 2 1 1 ([(0 0 0), (0 1 0)])
--
mkSGVTK :: (RenderElemVTK a)=> String -> Int -> Int -> Int -> Vector a -> VTK a
mkSGVTK name nx ny nz points = let
  nameTxt = pack name
  dataset = StructGrid { dimSG = (nx, ny, nz), setSG = points }
  in VTK nameTxt dataset [] []

-- | Creates an Structured Points dataset (ImageData). Use:
--
-- > mkSGVTK "name" (2, 1, 1) (0.0, 0.0, 0.0) (5.0, 5.0, 5.0) ([(0 0 0), (0 1 0)])
--
mkSPVTK :: String -> (Int, Int, Int) -> (Double, Double, Double)
        -> (Double, Double, Double) -> VTK Double
mkSPVTK name (dx, dy, dz) orig spc = let
  nameTxt = pack name
  size    = dx * dy * dz
  -- Fake data is needed for the func renderPointData gen. data points!!!
  ps      = U.replicate size 0
  dataset = StructPoint { dimSP    = (dx-1, dy-1, dz-1)
                        , originSP = orig
                        , spaceSP  = spc
                        , setSP    = ps }
  in VTK nameTxt dataset [] []

-- | Adds data to all points in 'VTK'. Internally, it pass the data as a function
-- 'VTKAttPoint'.
--
-- > let attr = mkPointsAttr "grainID" (\i x -> grainIDTable!i)
-- > addDataPoints vtk
--
addDataPoints :: (RenderElemVTK a)=> VTK a -> VTKAttrPoint a -> VTK a
addDataPoints VTK{..} attr = VTK { pointData = attr:pointData, .. }

-- | Adds data to all points in 'VTK'. Internally, it pass the data as a function
-- 'VTKAttPoint'.
--
-- > let attr = mkCellAttr "color" (\i x cellType -> (Vec3 1 1 1) &* 1/(evalCellType cellType))
-- > addDataCells vtk attr
--
addDataCells :: (RenderElemVTK a)=> VTK a -> VTKAttrCell a -> VTK a
addDataCells VTK{..} attr = VTK { cellData = attr:cellData, .. }

-- ================================ Internal stuffs ======================================

type CellBuilder = ([Vector Int], [Int], [CellType], Int)

addCell :: (RenderCell shape)=> CellBuilder -> shape -> CellBuilder
addCell set@(cellUG, cellOffUG, cellTypeUG, offCount) obj
  | cell_size <= 0 = set
  | otherwise = (cell : cellUG, cell_off : cellOffUG, cell_type : cellTypeUG , cell_off)
  where
    cell       = makeCell obj
    cell_off   = offCount + cell_size
    cell_size  = U.length cell
    cell_type  = getType obj

mkUnstructGrid :: (RenderElemVTK p, RenderCell shape, Foldable cont shape)=>
                  Vector p -> cont shape -> VTKDataSet p
mkUnstructGrid points cells = let
  i = ([], [], [], 0)
  (cell, cell_off, cell_type, _) = folder addCell i cells
  in UnstructGrid { setUG      = points
                  , cellUG     = U.concat cell
                  , cellOffUG  = U.reverse $ U.fromList cell_off
                  , cellTypeUG = V.reverse $ V.fromList cell_type }

mkRectLinGrid :: (RenderElemVTK a)=> Vector a ->  Vector a ->  Vector a -> VTKDataSet a
mkRectLinGrid x y z = let
  foo v = let
    s = U.length v
    in if s == 0 then s else s - 1
  in RectLinGrid { dimRG  = (foo x, foo y, foo z)
                 , setxRG = x
                 , setyRG = y
                 , setzRG = z }

-- ================================ Cell containers ======================================

class Foldable cont b where
  folder :: (a -> b -> a) -> a -> cont b -> a

instance Foldable [] a where
  folder func = L.foldr (flip func)

instance (U.Unbox a)=> Foldable Vector a where
  folder func = U.foldr' (flip func)

instance Foldable V.Vector a where
  folder func = V.foldr' (flip func)

instance Foldable IntMap a where
  folder func = IM.fold (flip func)

-- ================================ Basic instances ======================================

instance RenderCell (Int, Int, Int) where
  makeCell (a,b,c) = U.fromList [a,b,c]
  getType _        = VTK_TRIANGLE

instance RenderCell (Int, Int) where
  makeCell (a,b) = U.fromList [a,b]
  getType _      = VTK_LINE

instance RenderCell Int where
  makeCell a = U.singleton a
  getType _  = VTK_VERTEX
