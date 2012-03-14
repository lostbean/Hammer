{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hammer.Render.VTK.VTKXMLTemplate
       ( renderVTKUni
       , renderVTKMulti
       ) where

import qualified Prelude as P
import Prelude hiding ((++), length, head, map, null)

import Data.List (intersperse)

import qualified Data.Text as T
import Data.Text (Text, pack, append)

import Data.Vector
import qualified Data.Vector as Vec

import Text.XML

import Hammer.Math.Vector hiding (Vector)
import Hammer.Render.VTK.Base


renderVTKUni::(RenderPoint a)=> VTK a -> Document
renderVTKUni vtk = let
  dataSetType = renderType $ dataSet vtk
  node = [NodeElement $ renderVTK vtk]
  in renderDoc dataSetType node

renderVTKMulti::(RenderPoint a)=> Vector (VTK a) -> Document
renderVTKMulti vtk = let
  dataSetType = if null vtk then "" else (renderType . dataSet .  head) vtk
  nodes = toList $ map (NodeElement . renderVTK) vtk
  in renderDoc dataSetType nodes

renderDoc::Text -> [Node] -> Document
renderDoc dataSetType node = Document {
  documentPrologue = Prologue {prologueBefore = [], prologueDoctype = Nothing, prologueAfter = []}, 
  documentRoot = Element {
    elementName = Name {nameLocalName = "VTKFile", nameNamespace = Nothing, namePrefix = Nothing}, 
    elementAttributes = [(Name {nameLocalName = "type", nameNamespace = Nothing, namePrefix = Nothing}, dataSetType)], 
    elementNodes = [ NodeContent "\n"
                   , NodeElement $ Element {
                       elementName = Name {nameLocalName = dataSetType, nameNamespace = Nothing, namePrefix = Nothing},
                       elementAttributes = [], 
                       elementNodes = [NodeContent "\n"] P.++ node P.++ [NodeContent "\n"]}
                   , NodeContent "\n"
                   ] },
  documentEpilogue = []
  }

renderType::(RenderPoint a)=> VTKDataSet a -> Text
renderType dataSet = case dataSet of 
  StructPoint  _ _ _   -> "ImageData"
  StructGrid   _ _     -> "StructuredGrid"
  RectLinGrid  _ _ _ _ -> "RectilinearGrid"
  UnstructGrid _ _ _ _ -> "UnstructuredGrid"
    
--format = if isBinary then "binary" else "ascii"

renderVTK::(RenderPoint a)=> VTK a -> Element
renderVTK vtk@(VTK{..}) = case dataSet of 
  StructPoint dim orig spc               -> renderSP dim orig spc
  StructGrid  dim set                    -> renderSG dim set
  RectLinGrid dim setX setY setZ         -> renderRG dim setX setY setZ
  UnstructGrid set cell cellOff cellType -> renderUG set cell cellOff cellType pointData cellData 

renderSP = undefined

renderSG = undefined

renderRG = undefined

renderUG::(RenderPoint a)=> Vector a -> Vector Int -> Vector Int -> Vector CellType -> [VTKAttrPoint a] -> [VTKAttrCell a] -> Element
renderUG set cell cellOff cellType pointData cellData =
  let
    numPoints = length set
    numCell   = length cellOff
  in Element {
    elementName = Name {nameLocalName = "Piece", nameNamespace = Nothing, namePrefix = Nothing},
    elementAttributes = [
      -- Number of points and cells
      (Name {nameLocalName = "NumberOfPoints", nameNamespace = Nothing, namePrefix = Nothing}, toTxt numPoints),
      (Name {nameLocalName = "NumberOfCells" , nameNamespace = Nothing, namePrefix = Nothing}, toTxt numCell)], 
    elementNodes =
      -- Insert Point data
      P.map (NodeElement . renderPointData set) pointData
      P.++
      -- Insert Cell Data
      P.map (NodeElement . renderCellData set cell cellOff cellType) cellData
      P.++
      -- Insert points
      [NodeElement $ renderPoints set,
       -- Insert cells
       NodeElement $ renderCells cell cellOff cellType empty empty]
    }
  
  
renderPointData::(RenderPoint a)=> Vector a -> VTKAttrPoint a ->  Element
renderPointData pointData attr = case attr of
    IDDataPoint     name func -> renderData False name "Scalar" func "Int64" pointData
    ScalarDataPoint name func -> renderData False name "Scalar" func "Float32" pointData
    VectorDataPoint name func -> renderData False name "Vector" func "Float32" pointData
    TensorDataPoint name func -> renderData False name "Vector" func "Float32" pointData
  

renderCellData::(RenderPoint a)=> Vector a -> Vector Int -> Vector Int -> Vector CellType -> VTKAttrCell a -> Element
renderCellData set cell cellOff cellType attr =
  let
    mod func i x = let
      sec = map (set!) $ 
            if i == 0
            then slice 0 (cellOff!0) cell
            else slice (cellOff!i-1) (cellOff!i) cell
      tp  = cellType!i
      in func i sec tp
  in case attr of
    IDDataCell     name func -> renderData True name "Scalar" (mod func) "Int64" cellOff
    ScalarDataCell name func -> renderData True name "Scalar" (mod func) "Float32" cellOff
    VectorDataCell name func -> renderData True name "Vector" (mod func) "Float32" cellOff
    TensorDataCell name func -> renderData True name "Vector" (mod func) "Float32" cellOff
  
       
renderData::(RenderAttr attr) => Bool -> String -> Text -> (Int -> a -> attr) -> Text -> Vector a -> Element
renderData isCellData name attrType func attrNum xs=
  let
    dataType = if isCellData then "CellData" else "PointData"
    format = "ascii"
    numRender acc i x = acc `T.append` renderAttr (func i x)

  in Element {
    elementName = Name {nameLocalName = dataType, nameNamespace = Nothing, namePrefix = Nothing},
    elementAttributes = [(Name {nameLocalName = attrType, nameNamespace = Nothing, namePrefix = Nothing}, pack name)],
    elementNodes = [
      NodeContent "\n",
      NodeElement (
        Element {
           elementName = Name {nameLocalName = "DataArray", nameNamespace = Nothing, namePrefix = Nothing},
           elementAttributes = [
             (Name {nameLocalName = "type", nameNamespace = Nothing, namePrefix = Nothing}, attrNum),
             (Name {nameLocalName = "Name", nameNamespace = Nothing, namePrefix = Nothing}, pack name),
             (Name {nameLocalName = "format", nameNamespace = Nothing, namePrefix = Nothing}, format)],
           elementNodes = [
             NodeContent "\n\t\t",
             NodeContent (ifoldl' numRender T.empty xs)]}),
      NodeContent "\n"]}
     
  
renderCells::Vector Int -> Vector Int -> Vector CellType -> Vector Int -> Vector Int -> Element
renderCells cellConn cellOffsets cellTypes faces faceOffsets = Element {
    elementName = Name {nameLocalName = "Cells", nameNamespace = Nothing, namePrefix = Nothing},
    elementAttributes = [],
    elementNodes = intersperse (NodeContent "\n") full }
  where
    full = if Vec.null faces
           then conn : off : [types] 
           else conn : off : types : fac : [facoff]
    
    conn   = func "Int64" "connectivity" "ascii" cellConn
    off    = func "Int64" "offsets" "ascii" cellOffsets
    types  = func "UInt8" "types" "ascii" cellTypes
    fac    = func "Int64" "faces" "ascii" faces
    facoff = func "Int64" "faceoffsets" "ascii" faceOffsets
    
    func numType name format xs = 
      NodeElement (
        Element {
           elementName = Name {nameLocalName = "DataArray", nameNamespace = Nothing, namePrefix = Nothing},
           elementAttributes = [
             (Name {nameLocalName = "type", nameNamespace = Nothing, namePrefix = Nothing}, numType),
             (Name {nameLocalName = "Name", nameNamespace = Nothing, namePrefix = Nothing}, name),
             (Name {nameLocalName = "format", nameNamespace = Nothing, namePrefix = Nothing}, format)],
           elementNodes = [
             NodeContent (Vec.foldl' (\acc x -> acc `T.append` (toTxt x `T.snoc` ' ')) T.empty xs)] })
                 
                 
renderPoints::(RenderPoint a)=> Vector a -> Element
renderPoints points = 
  Element {
    elementName = Name {nameLocalName = "Points", nameNamespace = Nothing, namePrefix = Nothing},
    elementAttributes = [],
    elementNodes = [
      NodeContent "\n",
      NodeElement (
        Element {
           elementName = Name {nameLocalName = "DataArray", nameNamespace = Nothing, namePrefix = Nothing},
           elementAttributes = [
             (Name {nameLocalName = "type", nameNamespace = Nothing, namePrefix = Nothing}, "Float32"),
             (Name {nameLocalName = "Name", nameNamespace = Nothing, namePrefix = Nothing}, "Points"),
             (Name {nameLocalName = "NumberOfComponents", nameNamespace = Nothing, namePrefix = Nothing}, "3"),
             (Name {nameLocalName = "format", nameNamespace = Nothing, namePrefix = Nothing}, "ascii")],
           elementNodes = [
             NodeContent (Vec.foldl' (\acc x -> acc `T.append` (renderPoint x `T.snoc` ' ')) T.empty points)] }),
      NodeContent "\n"]}


instance RenderAttr Double where
  renderAttr = toTxt  
                        
instance RenderAttr Int where
  renderAttr = toTxt    
  
instance RenderAttr Vec3 where
  renderAttr (Vec3 x y z) = T.unwords [toTxt x, toTxt y, toTxt z]
  
instance RenderAttr Mat3 where
  renderAttr (Mat3 a b c) = T.unwords [renderAttr a, renderAttr b, renderAttr c]
