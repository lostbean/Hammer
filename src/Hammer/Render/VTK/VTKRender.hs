{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeSynonymInstances      #-}

-- | This module renders VTK file in XML format with ASCII data. It tries to be flexible
-- but keeping a high level API.
-- 
-- TODO
-- 
--  * Implement render functions for rectilinear grid, structured grid and structured points.
-- 
--  * Implement binary data format.
-- 
module Hammer.Render.VTK.VTKRender
  ( VTK (..)
  , addDataPoints
  , addDataCells
  , writeUniVTKfile
  , writeMultiVTKfile
  , mkUGVTK
  , mkRLGVTK
  , mkSGVTK
  , CellType (..)
  , evalCellType
  , RenderAttr (mkCellAttr, mkPointAttr)
  , RenderCell (..)
  , VTKAttrCell
  , VTKAttrPoint
  ) where

import qualified Prelude                          as P
import qualified Data.Vector                      as Vec
import qualified Data.IntMap                      as IM
import qualified Text.XML.Generator               as X
import qualified Data.Text                        as T
import qualified Data.ByteString.Lazy             as BSL
  
import           Prelude                          hiding (head, last, length, map, (++))
import           Hammer.Math.Vector               hiding (Vector)

import           Data.Vector
import           Data.IntMap                      (IntMap)
import           Data.Text                        (Text, pack)

import           Hammer.Render.VTK.Types
import           Hammer.Render.VTK.VTKXMLTemplate


-- | Write an unitary piece VTK file to disk.
writeUniVTKfile :: (RenderPoint a) => FilePath -> VTK a -> IO ()
writeUniVTKfile name vtk = BSL.writeFile name (X.xrender $ renderVTKUni vtk)

-- | Write an multi-piece piece VTK file to disk.
writeMultiVTKfile :: (RenderPoint a) => FilePath -> Vector (VTK a) -> IO ()
writeMultiVTKfile name vtk = BSL.writeFile name (X.xrender $ renderVTKMulti vtk)

-- | Creates an Unstructured Grid dataset. Use:
-- 
-- > mkUGVTK "piece_name" (Vec.fromList [Vec3 0 0 0, Vec3 0 1 0]) [(0,1)]
-- 
mkUGVTK :: (RenderPoint p, RenderCell shape, Foldable cont)=> String -> Vector p -> cont shape -> VTK p
mkUGVTK name points cells = let
  nameTxt = pack name
  dataset = mkUnstructGrid points cells
  in mkVTK nameTxt False dataset [] []

-- | Creates an Rectilinear Grid dataset. Use:
-- 
-- > mkRLGVTK "piece_name" (Vec.fromList [0,1,2]) (Vec.fromList [0,1,2]) (Vec.fromList [0,1])
-- 
mkRLGVTK :: String -> Vector Double -> Vector Double -> Vector Double -> VTK Double
mkRLGVTK name px py pz = let
  nameTxt = pack name
  dataset = mkRectLinGrid px py pz
  in mkVTK nameTxt False dataset [] []

-- | Creates an Structured Grid dataset. Use:
-- 
-- > mkSGVTK "piece_name" 2 1 1 (Vec.fromList [Vec3 0 0 0, Vec3 0 1 0]) 
-- 
mkSGVTK :: (RenderPoint a)=> String -> Int -> Int -> Int -> Vector a -> VTK a
mkSGVTK name nx ny nz points = let
  nameTxt = pack name
  dataset = mkStructGrid nx ny nz points
  in mkVTK nameTxt False dataset [] []

-- | Adds data to all points in 'VTK'. Internally, it pass the data as a function 'VTKAttPoint'.
-- 
-- > let attr = mkCellAttr "grainID" (\i x -> grainIDTable!i)
-- > addDataPoints vtk 
-- 
addDataPoints :: (RenderPoint a)=> VTK a -> VTKAttrPoint a -> VTK a
addDataPoints VTK{..} attr = VTK { pointData = attr:pointData, .. }

-- | Adds data to all points in 'VTK'. Internally, it pass the data as a function 'VTKAttPoint'.  
-- 
-- > let attr = mkCellAttr "color" (\i x cellType -> (Vec3 1 1 1) &* 1/(evalCellType cellType))
-- > addDataCells vtk attr 
-- 
addDataCells :: (RenderPoint a)=> VTK a -> VTKAttrCell a -> VTK a
addDataCells VTK{..} attr = VTK { cellData = attr:cellData, .. }

-- ================================ Internal stuffs ==============================================

mkVTK :: (RenderPoint a)=> Text -> Bool -> VTKDataSet a -> [VTKAttrPoint a] -> [VTKAttrCell a] -> VTK a
mkVTK = VTK

mkUnstructGrid :: (RenderPoint p, RenderCell shape, Foldable cont)=> Vector p -> cont shape -> VTKDataSet p
mkUnstructGrid points cells = let
  i = UnstructGrid { setUG      = points
                   , cellUG     = empty
                   , cellOffUG  = empty
                   , cellTypeUG = empty }
  in folder addCell i cells

mkRectLinGrid :: (RenderPoint a)=> Vector a ->  Vector a ->  Vector a -> VTKDataSet a
mkRectLinGrid x y z = let
  foo v = let
    s = length v
    in if s == 0 then s else s - 1
  in RectLinGrid { dimRG  = (foo x, foo y, foo z)
                                  , setxRG = x
                                  , setyRG = y
                                  , setzRG = z }

mkStructGrid :: (RenderPoint a)=>  Int -> Int -> Int -> Vector a -> VTKDataSet a
mkStructGrid nx ny nz points = StructGrid { dimSG = (nx, ny, nz)
                                          , setSG = points }

addCell :: (RenderPoint a, RenderCell shape)=> VTKDataSet a -> shape -> VTKDataSet a
addCell set@(UnstructGrid{..}) obj
  | cell_size <= 0 = set
  | otherwise = set { cellUG      = cellUG       ++   cell
                    , cellOffUG   = cellOffUG  `snoc` cell_Off
                    , cellTypeUG  = cellTypeUG `snoc` cell_type }
  where
    cell      = makeCell obj
    cell_type = getType obj
    cell_size = Vec.length cell
    cell_Off  = if Vec.null cellOffUG then cell_size else last cellOffUG + cell_size
addCell set _ = set

-- ------------------------- Cell containers -------------------------------------------

class Foldable cont where
  folder::(a -> b -> a) -> a -> cont b -> a

instance Foldable Vector where
  folder = foldl'

instance Foldable IntMap where
  folder func = IM.fold (flip func)

-- ----------------------- Basic instances ----------------------------------------------

instance RenderCell (Int, Int, Int, Int) where
  makeCell (a,b,c,d) = Vec.fromList [a,b,c,d]
  getType _          = VTK_QUAD

instance RenderCell (Int, Int, Int) where
  makeCell (a,b,c) = Vec.fromList [a,b,c]
  getType _        = VTK_TRIANGLE

instance RenderCell (Int, Int) where
  makeCell (a,b) = Vec.fromList [a,b]
  getType _      = VTK_LINE

instance RenderCell Int where
  makeCell  = Vec.singleton
  getType _ = VTK_VERTEX

instance RenderPoint Vec3 where
  renderPoint (Vec3 x y z) = T.unwords [toTxt x, toTxt y, toTxt z]

instance RenderPoint Vec2 where
  renderPoint (Vec2 x y) = T.unwords [toTxt x, toTxt y]

instance RenderPoint Double where
  renderPoint = toTxt

instance RenderPoint Int where
  renderPoint = toTxt

