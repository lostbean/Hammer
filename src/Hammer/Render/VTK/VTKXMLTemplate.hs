{-# LANGUAGE RecordWildCards #-}

module Hammer.Render.VTK.VTKXMLTemplate
       ( renderVTKUni
       , renderVTKMulti
       ) where

import qualified Data.ByteString.Lazy.Builder as BB
import qualified Data.Text                    as T
import qualified Data.Vector                  as V
import qualified Data.Vector.Unboxed          as U
import qualified Prelude                      as P

import           Data.ByteString.Base64.Lazy  (encode)
import           Data.Foldable                (foldMap)
import           Data.Text                    (Text)
import           Data.Vector.Unboxed          (Vector, (!))

import           Prelude
import           Text.XML.Generator

import           Hammer.Math.Algebra          ()
import           Hammer.Render.VTK.Types

--import           Debug.Trace
--dbg a = trace ("::::::>> " ++ show a) a

-- =======================================================================================

renderVTKUni::(RenderElemVTK a)=> Bool -> VTK a -> Xml Doc
renderVTKUni isBinary vtk = let
  dataSetType = renderType $ dataSet vtk
  node        = [renderVTK isBinary vtk]
  in renderDoc dataSetType node

renderVTKMulti::(RenderElemVTK a)=> Bool -> V.Vector (VTK a) -> Xml Doc
renderVTKMulti isBinary vtk = let
  dataSetType = if V.null vtk
                then ("", noAttrs)
                else (renderType . dataSet .  V.head) vtk
  nodes       = V.toList $ V.map (renderVTK isBinary) vtk
  in renderDoc dataSetType nodes

renderDoc :: (String, Xml Attr) -> [Xml Elem] -> Xml Doc
renderDoc (dataSetType, attr) node = let
  a = xelem "VTKFile" $ (xattr "type" dataSetType)
      <#> xelem dataSetType (attr <#> xelems node)
  in doc defaultDocInfo a

renderType::(RenderElemVTK a)=> VTKDataSet a -> (String, Xml Attr)
renderType dataSet = case dataSet of
  StructPoint  dim orig spc _ -> ("ImageData",        renderWholeExtAttr  dim  <>
                                                      renderOriginAttr    orig <>
                                                      renderSpaceAttr     spc  )
  StructGrid   {}             -> ("StructuredGrid",   noAttrs)
  RectLinGrid  dim _ _ _      -> ("RectilinearGrid",  renderWholeExtAttr dim)
  UnstructGrid {}             -> ("UnstructuredGrid", noAttrs)

-- =============================== render VTK types ======================================

renderVTK :: (RenderElemVTK a)=> Bool -> VTK a -> Xml Elem
renderVTK isBinary VTK{..} = case dataSet of
  StructPoint dime orig spc set           -> renderSP isBinary dime orig spc set     pointData cellData
  StructGrid  dime set                    -> renderSG isBinary dime set              pointData cellData
  RectLinGrid dime setX setY setZ         -> renderRG isBinary dime setX setY setZ   pointData cellData
  UnstructGrid set cell cellOff cellType  -> renderUG isBinary set cell cellOff cellType pointData cellData

renderSG :: a
renderSG = error "[Hammer] Can't render this type of VTK file. No implemented yet."

renderSP :: (RenderElemVTK a)=> Bool -> (Int, Int, Int) -> (Double, Double, Double)
         -> (Double, Double, Double) -> Vector a -> [VTKAttrPoint a] -> [VTKAttrCell a] -> Xml Elem
renderSP isBin ext _ _ ps pointData _ = let
  nodes = [ renderPointData isBin ps pointData ]
  in xelem "Piece" $ xattrRaw "Extent" (renderExtent ext) <#> xelems nodes

renderRG :: (RenderElemVTK a)=> Bool -> (Int, Int, Int) -> Vector a -> Vector a
         -> Vector a -> [VTKAttrPoint a] -> [VTKAttrCell a] -> Xml Elem
renderRG isBin ext@(dx, dy, dz) setX setY setZ pointData cellData = let
  size      = dx * dy * dz
  vecOne    = U.replicate size 1
  vecVertex = V.replicate size VTK_VERTEX
  vecSerial = U.fromList [ i + dx*j + dx*dy*k | i <- [0..dx], j <- [0..dy], k <- [0..dz] ]
  nodes = [ renderPointData   isBin U.empty pointData
          , renderCellData    isBin U.empty vecSerial vecOne vecVertex cellData
          , renderCoordinates isBin setX setY setZ ]
  in xelem "Piece" $ xattrRaw "Extent" (renderExtent ext) <#> xelems nodes


renderUG :: (RenderElemVTK a)=> Bool -> Vector a -> Vector Int -> Vector Int
         -> V.Vector CellType -> [VTKAttrPoint a] -> [VTKAttrCell a] -> Xml Elem
renderUG isBin set cell cellOff cellType pointData cellData = let
  numPoints = U.length set
  numCell   = U.length cellOff
  attrs = [ xattr "NumberOfPoints" (toTxt numPoints)
          , xattr "NumberOfCells"  (toTxt numCell) ]
  nodes = [ renderPointData isBin set pointData
          , renderCellData  isBin set cell cellOff cellType cellData
          , renderPoints    isBin set
          , renderCells     isBin cell cellOff cellType U.empty U.empty ]
  in xelem "Piece" (xattrs attrs <#> xelems nodes)

-- =============================== render tools ==========================================

renderPointData::(RenderElemVTK a)=> Bool -> Vector a -> [VTKAttrPoint a] -> Xml Elem
renderPointData isBin pointData attrs = let
  renderAttr (VTKAttrPoint name func) = renderDataArray isBin (U.imap func pointData) name
  in xelem "PointData" $ xelems $ P.map renderAttr attrs

renderCellData :: (RenderElemVTK a)=> Bool -> Vector a -> Vector Int -> Vector Int
               -> V.Vector CellType -> [VTKAttrCell a] -> Xml Elem
renderCellData isBin set cell cellOff cellType attrs = let
  renderAttr (VTKAttrCell name func) = let
    arr = U.imap (mode func) cellOff
    in renderDataArray isBin arr name
  mode func i _ = let
    sec = U.map (set!) $
          if i == 0
          then U.slice 0 (cellOff!0) cell
          else U.slice (cellOff!i-1) (cellOff!i) cell
    tp  = cellType V.! i
    in func i sec tp
  in xelem "CellData" $ xelems $ P.map renderAttr attrs

renderCells :: Bool -> Vector Int -> Vector Int -> V.Vector CellType
            -> Vector Int -> Vector Int -> Xml Elem
renderCells isBin cellConn cellOffsets cellTypes faces faceOffsets = let
  cts  = U.generate (V.length cellTypes) (evalCellType . (cellTypes V.!))
  full = if U.null faces
         then conn : off : [types]
         else conn : off : types : fac : [facoff]

  conn   = func cellConn    "connectivity"
  off    = func cellOffsets "offsets"
  types  = func cts         "types"
  fac    = func faces       "faces"
  facoff = func faceOffsets "faceoffsets"

  func = renderDataArray isBin
  in xelem "Cells" $ xelems full

renderWholeExtAttr :: (Int, Int, Int) -> Xml Attr
renderWholeExtAttr = xattr "WholeExtent" . renderExtent

renderSpaceAttr :: (Double, Double, Double) -> Xml Attr
renderSpaceAttr = xattr "Spacing" . renderTriple

renderOriginAttr :: (Double, Double, Double) -> Xml Attr
renderOriginAttr = xattr "Origin" . renderTriple

renderTriple :: (Show a)=> (a, a, a) -> Text
renderTriple (a, b, c) = T.unwords [toTxt a, toTxt b, toTxt c]

renderExtent :: (Int, Int, Int) -> Text
renderExtent (dx, dy, dz) = let
  zr = toTxt (0 :: Int)
  in T.unwords [zr, toTxt dx, zr, toTxt dy, zr, toTxt dz]

renderCoordinates :: (RenderElemVTK a)=> Bool -> Vector a -> Vector a -> Vector a -> Xml Elem
renderCoordinates isBin setX setY setZ = let
  nodes =
    -- Insert points
    [ renderCoordinatePoint isBin setX "x"
      -- Insert cells
    , renderCoordinatePoint isBin setY "y"
    , renderCoordinatePoint isBin setZ "z" ]
  in xelem "Coordinates" $ xelems nodes

renderCoordinatePoint :: (RenderElemVTK a)=> Bool -> Vector a -> String -> Xml Elem
renderCoordinatePoint isBin points dir = let
  name = dir P.++ "_coordinate"
  in renderDataArray isBin points name

renderPoints :: (RenderElemVTK a)=> Bool -> Vector a -> Xml Elem
renderPoints isBin points = let
  child = renderDataArray isBin points "Points"
  in xelem "Points" child

renderDataArray :: (RenderElemVTK a)=> Bool -> Vector a -> String -> Xml Elem
renderDataArray isBin points name = let
  size = encode . BB.toLazyByteString . BB.word32BE . fromIntegral $ U.length points
  vecRender
    | isBin = (size <>) . encode . BB.toLazyByteString . foldMap renderBinaryPoint . U.toList
    | otherwise = BB.toLazyByteString . foldMap renderPoint . U.toList
  format
    | isBin     = "binary"
    | otherwise = "ascii"
  attr = [ xattr "type"               (getNumType points)
         , xattr "Name"               name
         , xattr "format"             format
         , xattr "NumberOfComponents" (getNumComp points)
         ]
  in xelem "DataArray" ((xattrs attr) <#> (xtextRaw $ vecRender points))

getNumType :: (RenderPoint a, U.Unbox a)=> Vector a -> String
getNumType = renderNumType . pointNumberType . U.head

getNumComp :: (RenderPoint a, U.Unbox a)=> Vector a -> Text
getNumComp =  toTxt . pointNumberComp . U.head

toTxt::(Show a)=> a -> Text
toTxt = T.pack.show
