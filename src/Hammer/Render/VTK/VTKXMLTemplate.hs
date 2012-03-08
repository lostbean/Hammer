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

toTxt::(Show a)=>a -> Text
toTxt = pack.show


renderVTKUni::VTK -> Document
renderVTKUni vtk = let
  dataSetType = renderType $ dataSet vtk
  node = [NodeElement $ renderVTK vtk]
  in renderDoc dataSetType node

renderVTKMulti::Vector VTK -> Document
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

renderType::VTKDataSet -> Text
renderType dataSet = case dataSet of 
  StructPoint  _ _ _   -> "ImageData"
  StructGrid   _ _     -> "StructuredGrid"
  RectLinGrid  _ _ _ _ -> "RectilinearGrid"
  UnstructGrid _ _ _ _ -> "UnstructuredGrid"
    
--format = if isBinary then "binary" else "ascii"

renderVTK::VTK -> Element
renderVTK vtk@(VTK{..}) = case dataSet of 
  StructPoint dim orig spc               -> renderSP dim orig spc
  StructGrid  dim set                    -> renderSG dim set
  RectLinGrid dim setX setY setZ         -> renderRG dim setX setY setZ
  UnstructGrid set cell cellOff cellType -> renderUG set cell cellOff cellType pointData cellData 

renderSP = undefined

renderSG = undefined

renderRG = undefined

renderUG::Vector Vec3 -> Vector Int -> Vector Int -> Vector CellType -> [VTKAttrPoint] -> [VTKAttrCell] -> Element
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
  
  
renderPointData::Vector Vec3 -> VTKAttrPoint ->  Element
renderPointData pointData (name, func) = renderData False name func pointData
  

renderCellData::Vector Vec3 -> Vector Int -> Vector Int -> Vector CellType -> VTKAttrCell -> Element
renderCellData set cell cellOff cellType (name, func) = renderData True name func' cellOff
  where
    func' i x = let
      sec = map (set!) $ 
            if i == 0
            then slice 0 (cellOff!0) cell
            else slice (cellOff!i-1) x cell
      tp  = cellType!i
      in func i sec tp
       
renderData::Bool -> String -> (Int -> a -> VTKAttr) -> Vector a -> Element
renderData isCellData name func xs=
  let
    dataType = if isCellData then "CellData" else "PointData"
    format = "ascii"
           
    numRender acc i x = case func i x of       
      ScalarData x -> acc `T.append` (toTxt x       `T.snoc` ' ')
      VectorData x -> acc `T.append` (point2text x  `T.snoc` ' ')
      NormalData x -> acc `T.append` (point2text x  `T.snoc` ' ')
      TensorData x -> acc `T.append` (tensor2text x `T.snoc` ' ')

    attrType = case func 0 (head xs) of
      ScalarData _ -> "Scalar"
      VectorData _ -> "Vector"
      NormalData _ -> "Normal"
      TensorData _ -> "Tensor"
     
    numType = case func 0 (head xs) of
      ScalarData _ -> "Float32"
      VectorData _ -> "Float32"
      NormalData _ -> "Float32"
      TensorData _ -> "Float32"
  
  in Element {
    elementName = Name {nameLocalName = dataType, nameNamespace = Nothing, namePrefix = Nothing},
    elementAttributes = [(Name {nameLocalName = attrType, nameNamespace = Nothing, namePrefix = Nothing}, pack name)],
    elementNodes = [
      NodeContent "\n",
      NodeElement (
        Element {
           elementName = Name {nameLocalName = "DataArray", nameNamespace = Nothing, namePrefix = Nothing},
           elementAttributes = [
             (Name {nameLocalName = "type", nameNamespace = Nothing, namePrefix = Nothing}, numType),
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
                 
                 
renderPoints::Vector Vec3 -> Element
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
             NodeContent (Vec.foldl' (\acc x -> acc `T.append` (point2text x `T.snoc` ' ')) T.empty points)] }),
      NodeContent "\n"]}

point2text (Vec3 x y z) = T.unwords [toTxt x, toTxt y, toTxt z]

tensor2text (Mat3 a b c)= T.unwords [point2text a, point2text b, point2text c]