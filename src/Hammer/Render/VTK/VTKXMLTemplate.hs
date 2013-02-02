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
import Data.Text (Text, pack)

import Data.Vector
import qualified Data.Vector as Vec

import Text.XML
import qualified Data.Map as M

import Hammer.Math.Vector (Vec3(..), Mat3(..))
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
renderDoc dataSetType node = Document
  { documentPrologue = Prologue {prologueBefore = [], prologueDoctype = Nothing, prologueAfter = []}
  , documentRoot = Element
       { elementName = Name {nameLocalName = "VTKFile", nameNamespace = Nothing, namePrefix = Nothing}
       , elementAttributes = M.singleton (Name {nameLocalName = "type", nameNamespace = Nothing, namePrefix = Nothing}) dataSetType
       , elementNodes =
            [ NodeContent "\n"
            , NodeElement $ Element
                { elementName = Name {nameLocalName = dataSetType, nameNamespace = Nothing, namePrefix = Nothing}
                , elementAttributes = M.empty
                , elementNodes = [NodeContent "\n"] P.++ node P.++ [NodeContent "\n"]
                }
            , NodeContent "\n"
            ]
       },
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
renderVTK VTK{..} = case dataSet of 
  StructPoint dim orig spc               -> renderSP dim orig spc
  StructGrid  dim set                    -> renderSG dim set
  RectLinGrid dim setX setY setZ         -> renderRG dim setX setY setZ
  UnstructGrid set cell cellOff cellType -> renderUG set cell cellOff cellType pointData cellData 

renderSP :: a
renderSP = error "[Hammer] Can't render this type of VTK file. No implemented yet."

renderSG :: a
renderSG = error "[Hammer] Can't render this type of VTK file. No implemented yet."

renderRG :: a
renderRG = error "[Hammer] Can't render this type of VTK file. No implemented yet."

renderUG::(RenderPoint a)=> Vector a -> Vector Int -> Vector Int -> Vector CellType -> [VTKAttrPoint a] -> [VTKAttrCell a] -> Element
renderUG set cell cellOff cellType pointData cellData =
  let
    numPoints = length set
    numCell   = length cellOff
  in Element
    { elementName = Name {nameLocalName = "Piece", nameNamespace = Nothing, namePrefix = Nothing}
    , elementAttributes = M.fromList
      -- Number of points and cells
         [ (Name {nameLocalName = "NumberOfPoints", nameNamespace = Nothing, namePrefix = Nothing}, toTxt numPoints)
         , (Name {nameLocalName = "NumberOfCells" , nameNamespace = Nothing, namePrefix = Nothing}, toTxt numCell) ] 
    , elementNodes =
           -- Insert Point data
           [ NodeElement $ renderPointData set pointData
             -- Insert Cell Data
           , NodeElement $ renderCellData set cell cellOff cellType cellData ]

           P.++

           -- Insert points
           [ NodeElement $ renderPoints set
             -- Insert cells
           , NodeElement $ renderCells cell cellOff cellType empty empty ]
    }
  
  
renderPointData::(RenderPoint a)=> Vector a -> [VTKAttrPoint a] -> Element
renderPointData pointData attrs = let
  renderatrr attr = case attr of
    IDDataPoint     name func -> renderData name "Scalar" 1 func "Int64" pointData
    ScalarDataPoint name func -> renderData name "Scalar" 1 func "Float32" pointData
    VectorDataPoint name func -> renderData name "Vector" 3 func "Float32" pointData
    TensorDataPoint name func -> renderData name "Vector" 9 func "Float32" pointData
  in Element
    { elementName = Name {nameLocalName = "PointData", nameNamespace = Nothing, namePrefix = Nothing}
    , elementAttributes = M.empty
    , elementNodes = P.map (NodeElement . renderatrr) attrs
    }


renderCellData::(RenderPoint a)=> Vector a -> Vector Int -> Vector Int -> Vector CellType -> [VTKAttrCell a] -> Element
renderCellData set cell cellOff cellType attrs =
  let
    mode func i _ = let
      sec = map (set!) $ 
            if i == 0
            then slice 0 (cellOff!0) cell
            else slice (cellOff!i-1) (cellOff!i) cell
      tp  = cellType!i
      in func i sec tp
    renderatrr attr = case attr of
      IDDataCell     name func -> renderData name "Scalar" 1 (mode func) "Int64" cellOff
      ScalarDataCell name func -> renderData name "Scalar" 1 (mode func) "Float32" cellOff
      VectorDataCell name func -> renderData name "Vector" 3 (mode func) "Float32" cellOff
      TensorDataCell name func -> renderData name "Vector" 9 (mode func) "Float32" cellOff
  in Element
    { elementName = Name {nameLocalName = "CellData", nameNamespace = Nothing, namePrefix = Nothing}
    , elementAttributes = M.empty
    , elementNodes = P.map (NodeElement . renderatrr) attrs
    }
     
       
renderData::(RenderAttr attr) => String -> Text -> Int -> (Int -> a -> attr) -> Text -> Vector a -> Element
renderData name _ ncomp func attrNum xs=
  let
    basicAttr = [ (Name {nameLocalName = "type", nameNamespace = Nothing, namePrefix = Nothing}, attrNum)
                , (Name {nameLocalName = "Name", nameNamespace = Nothing, namePrefix = Nothing}, pack name)
                , (Name {nameLocalName = "format", nameNamespace = Nothing, namePrefix = Nothing}, format) ]
    ncompAttr = (Name {nameLocalName = "NumberOfComponents", nameNamespace = Nothing, namePrefix = Nothing}, renderAttr ncomp)
    attr = if ncomp > 1
           then ncompAttr:basicAttr
           else basicAttr
    format = "ascii"
    numRender acc i x = acc `T.append` (renderAttr (func i x) `T.snoc` ' ')

  in Element
    { elementName = Name {nameLocalName = "DataArray", nameNamespace = Nothing, namePrefix = Nothing}
    , elementAttributes = M.fromList attr
    , elementNodes =
      [ NodeContent "\n\t\t"
      , NodeContent (ifoldl' numRender T.empty xs)
      ]
    }
     
  
renderCells::Vector Int -> Vector Int -> Vector CellType -> Vector Int -> Vector Int -> Element
renderCells cellConn cellOffsets cellTypes faces faceOffsets = Element {
    elementName = Name {nameLocalName = "Cells", nameNamespace = Nothing, namePrefix = Nothing},
    elementAttributes = M.empty,
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
        Element
           { elementName = Name {nameLocalName = "DataArray", nameNamespace = Nothing, namePrefix = Nothing}
           , elementAttributes = M.fromList
                [ (Name {nameLocalName = "type", nameNamespace = Nothing, namePrefix = Nothing}, numType)
                , (Name {nameLocalName = "Name", nameNamespace = Nothing, namePrefix = Nothing}, name)
                , (Name {nameLocalName = "format", nameNamespace = Nothing, namePrefix = Nothing}, format)
                ]
           , elementNodes =
                  [ NodeContent (Vec.foldl' (\acc x -> acc `T.append` (toTxt x `T.snoc` ' ')) T.empty xs)]
           }
        )
                 
                 
renderPoints::(RenderPoint a)=> Vector a -> Element
renderPoints points = 
  Element
    { elementName = Name {nameLocalName = "Points", nameNamespace = Nothing, namePrefix = Nothing}
    , elementAttributes = M.empty
    , elementNodes = 
      [ NodeContent "\n"
      , NodeElement
        ( Element
           { elementName = Name {nameLocalName = "DataArray", nameNamespace = Nothing, namePrefix = Nothing}
           , elementAttributes = M.fromList
               [ (Name {nameLocalName = "type", nameNamespace = Nothing, namePrefix = Nothing}, "Float32")
               , (Name {nameLocalName = "Name", nameNamespace = Nothing, namePrefix = Nothing}, "Points")
               , (Name {nameLocalName = "NumberOfComponents", nameNamespace = Nothing, namePrefix = Nothing}, "3")
               , (Name {nameLocalName = "format", nameNamespace = Nothing, namePrefix = Nothing}, "ascii")
               ]
           , elementNodes = [
               NodeContent (Vec.foldl' (\acc x -> acc `T.append` (renderPoint x `T.snoc` ' ')) T.empty points)
               ]
           }
        )
      , NodeContent "\n"
      ]
    }


instance RenderAttr Double where
  renderAttr = toTxt
                        
instance RenderAttr Int where
  renderAttr = toTxt
  
instance RenderAttr Vec3 where
  renderAttr (Vec3 x y z) = T.unwords [toTxt x, toTxt y, toTxt z]
  
instance RenderAttr Mat3 where
  renderAttr (Mat3 a b c) = T.unwords [renderAttr a, renderAttr b, renderAttr c]
