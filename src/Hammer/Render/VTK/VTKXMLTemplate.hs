{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Hammer.Render.VTK.VTKXMLTemplate 
       ( renderVTKUni
       , renderVTKMulti
       ) where

import qualified Data.Text          as T
import qualified Data.Text.Internal as TI
import qualified Data.Text.Array    as TA
import qualified Data.Text.Foreign  as TF
import qualified Data.Vector        as V
import qualified Prelude            as P
import qualified Text.XML.Generator as X
  
import           Data.Text          (Text)
import           Data.Vector        (Vector, (!))

import           Prelude
import           Text.XML.Generator
import           Hammer.Render.VTK.Types


renderVTKUni::(RenderPoint a)=> VTK a -> Xml Doc
renderVTKUni vtk = let
  dataSetType = renderType $ dataSet vtk
  node        = [renderVTK vtk]
  in renderDoc dataSetType node

renderVTKMulti::(RenderPoint a)=> Vector (VTK a) -> Xml Doc
renderVTKMulti vtk = let
  dataSetType = if V.null vtk
                then ("", noAttrs)
                else (renderType . dataSet .  V.head) vtk
  nodes       = V.toList $ V.map renderVTK vtk
  in renderDoc dataSetType nodes
     
renderDoc :: (String, Xml Attr) -> [Xml Elem] -> Xml Doc
renderDoc (dataSetType, attr) node = let
  a = X.xelem "VTKFile" $ (xattr "type" dataSetType) <#> xelems b
  b = P.map (\x -> xelem dataSetType (attr <#> x)) node
  in doc defaultDocInfo a

renderType::(RenderPoint a)=> VTKDataSet a -> (String, Xml Attr)
renderType dataSet = case dataSet of 
  StructPoint  {}        -> ("ImageData",        noAttrs)
  StructGrid   {}        -> ("StructuredGrid",   noAttrs)
  RectLinGrid  dim _ _ _ -> ("RectilinearGrid",  renderWholeExtAttr dim)
  UnstructGrid {}        -> ("UnstructuredGrid", noAttrs)
    
renderVTK::(RenderPoint a)=> VTK a -> Xml Elem
renderVTK VTK{..} = case dataSet of 
  StructPoint dim orig spc               -> renderSP dim orig spc              pointData cellData 
  StructGrid  dim set                    -> renderSG dim set                   pointData cellData 
  RectLinGrid dim setX setY setZ         -> renderRG dim setX setY setZ        pointData cellData 
  UnstructGrid set cell cellOff cellType -> renderUG set cell cellOff cellType pointData cellData 

renderSP :: a
renderSP = error "[Hammer] Can't render this type of VTK file. No implemented yet."

renderSG :: a
renderSG = error "[Hammer] Can't render this type of VTK file. No implemented yet."

renderRG :: (RenderPoint a)=> (Int, Int, Int) -> Vector a -> Vector a
         -> Vector a -> [VTKAttrPoint a] -> [VTKAttrCell a] -> Xml Elem
renderRG ext@(dx, dy, dz) setX setY setZ pointData cellData = let
  size      = dx*dy*dz
  vecOne    = V.replicate size 1
  vecVertex = V.replicate size VTK_VERTEX
  vecSerial = V.fromList [ i + dx*j + dx*dy*k | i <- [0..dx], j <- [0..dy], k <- [0..dz] ]
  nodes = [ renderPointData V.empty pointData
          , renderCellData  V.empty vecSerial vecOne vecVertex cellData
          , renderCoordinates setX setY setZ ]
  in xelem "Piece" $ xattrRaw "Extent" (renderExtent ext) <#> xelems nodes
  

renderUG :: (RenderPoint a)=> Vector a -> Vector Int -> Vector Int
         -> Vector CellType -> [VTKAttrPoint a] -> [VTKAttrCell a] -> Xml Elem
renderUG set cell cellOff cellType pointData cellData = let
  numPoints = V.length set
  numCell   = V.length cellOff
  attrs = [ xattr "NumberOfPoints" (toTxt numPoints)
          , xattr "NumberOfCells"  (toTxt numCell) ] 
  nodes = [ renderPointData set pointData
          , renderCellData set cell cellOff cellType cellData
          , renderPoints set
          , renderCells cell cellOff cellType V.empty V.empty ]
  in xelem "Piece" (xattrs attrs <#> xelems nodes)
  
renderPointData::(RenderPoint a)=> Vector a -> [VTKAttrPoint a] -> Xml Elem
renderPointData pointData attrs = let
  fmapAttr attr = case attr of
    IDData     name func -> IDData     name (eval func)
    ScalarData name func -> ScalarData name (eval func)
    VectorData name func -> VectorData name (eval func)
    TensorData name func -> TensorData name (eval func)
  eval (AttrPoint func) = V.imap func pointData
  in xelemEmpty "PointData" <> xelems (P.map (renderData . fmapAttr) attrs)


renderCellData :: (RenderPoint a)=> Vector a -> Vector Int -> Vector Int
               -> Vector CellType -> [VTKAttrCell a] -> Xml Elem
renderCellData set cell cellOff cellType attrs = let
  fmapAttr attr = case attr of
    IDData     name func -> IDData     name (eval func)
    ScalarData name func -> ScalarData name (eval func)
    VectorData name func -> VectorData name (eval func)
    TensorData name func -> TensorData name (eval func)
  eval (AttrCell func) = V.imap (mode func) cellOff
  mode func i _ = let
    sec = V.map (set!) $ 
          if i == 0
          then V.slice 0 (cellOff!0) cell
          else V.slice (cellOff!i-1) (cellOff!i) cell
    tp  = cellType!i
    in func i sec tp
  in xelem "CellData" $ xelems $ P.map (renderData . fmapAttr) attrs
     
renderData :: VTKAttr Vector -> Xml Elem
renderData attr = case attr of
    IDData     n vs -> renderDataArray renderAttr vs 1 n "Int64"
    ScalarData n vs -> renderDataArray renderAttr vs 1 n "Float32"
    VectorData n vs -> renderDataArray renderAttr vs 3 n "Float32"
    TensorData n vs -> renderDataArray renderAttr vs 9 n "Float32"

renderCells::Vector Int -> Vector Int -> Vector CellType -> Vector Int -> Vector Int -> Xml Elem
renderCells cellConn cellOffsets cellTypes faces faceOffsets = xelem "Cells" $ xelems full
  where
    full = if V.null faces
           then conn : off : [types] 
           else conn : off : types : fac : [facoff]
    
    conn   = func cellConn                       0 "connectivity" "Int64"
    off    = func cellOffsets                    0 "offsets"      "Int64"     
    types  = func (V.map evalCellType cellTypes) 0 "types"        "UInt8"       
    fac    = func faces                          0 "faces"        "Int64"       
    facoff = func faceOffsets                    0 "faceoffsets"  "Int64" 
    
    func = renderDataArray renderAttr

renderWholeExtAttr :: (Int, Int, Int) -> Xml Attr
renderWholeExtAttr  dim = xattr "WholeExtent" (renderExtent dim) 
                
renderExtent :: (Int, Int, Int) -> Text
renderExtent (dx, dy, dz) = let
  zr = toTxt (0 :: Int)
  in T.unwords [zr, toTxt dx, zr, toTxt dy, zr, toTxt dz]

renderCoordinates :: (RenderPoint a)=> Vector a -> Vector a -> Vector a -> Xml Elem
renderCoordinates setX setY setZ = let
  nodes =
    -- Insert points
    [ renderCoordinatePoint setX "x"
      -- Insert cells
    , renderCoordinatePoint setY "y"
    , renderCoordinatePoint setZ "z" ]
  in xelem "Coordinates" $ xelems nodes
                 
renderCoordinatePoint :: (RenderPoint a)=> Vector a -> String -> Xml Elem
renderCoordinatePoint points dir = let
  name = dir P.++ "_coordinate"
  in renderDataArray renderPoint points 1 name "Float32"

renderPoints::(RenderPoint a)=> Vector a -> Xml Elem
renderPoints points = let
  child = renderDataArray renderPoint points 3 "Points" "Float32"
  in xelem "Points" child

renderDataArray :: (a -> Text) -> Vector a -> Int -> String -> String -> Xml Elem
renderDataArray render points rank name n_type = let
  vecRender = veConcat . V.map (T.cons ' ' . render)
  -- Wrong way! Why???
  --vecRender' = V.foldl' (\acc n -> acc `T.append` (render n `T.snoc` ' ')) T.empty
  attrList
    | rank > 1  = ncompAttr:basicAttr
    | otherwise = basicAttr
  format = "ascii"
  basicAttr = [ xattr "type"   n_type
              , xattr "Name"   name 
              , xattr "format" format ]
  ncompAttr = xattr "NumberOfComponents" (toTxt rank)
  in xelem "DataArray" ((xattrs attrList) <#> (xtextRaw $ vecRender points))

veConcat :: Vector TI.Text -> TI.Text
veConcat ts = TI.Text (TA.run go) 0 len
  where
    len = V.sum . V.filter (>0) $ V.map TF.lengthWord16 ts
    go = do
      arr <- TA.new len
      let step i (TI.Text a o l) =
            let !j = i + l in TA.copyI arr i a o j >> return j
      V.foldM step 0 ts >> return arr