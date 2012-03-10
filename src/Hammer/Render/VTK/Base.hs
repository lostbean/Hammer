
module Hammer.Render.VTK.Base where

import Data.Vector (Vector)
import Data.Text (Text)
import qualified Data.Text as T
import Hammer.Math.Vector hiding (Vector)


class RenderPoint point where
  renderPoint::point -> Text 
               
  
class RenderCell shape where
  makeCell::shape -> Vector Int
  getType ::shape -> CellType

type MultiPieceVTK a =  Vector (VTK a)

data VTK a = VTK { name      ::Text
                 , isBinary  ::Bool
                 , dataSet   ::VTKDataSet a
                 , pointData ::[VTKAttrPoint a]
                 , cellData  ::[VTKAttrCell a]
                 }

data (RenderPoint a) => VTKDataSet a = 
  StructPoint { dimSP       ::(Int, Int, Int)
              , originSP    ::(Double, Double, Double)
              , spaceSP     ::(Double, Double, Double) }
                  
  | StructGrid  { dimSG       ::(Int, Int, Int)
                , setSG       ::Vector a                 }
                
  | RectLinGrid { dimRG       ::(Int, Int, Int)
                , setxRG      ::Vector a              
                , setyRG      ::Vector a
                , setzRG      ::Vector a                 }
                
  | UnstructGrid { setUG      ::Vector a             
                 , cellUG     ::Vector Int
                 , cellOffUG  ::Vector Int
                 , cellTypeUG ::Vector CellType          }
                  


type VTKAttrPoint a = (String, Int -> a -> VTKAttr)
type VTKAttrCell  a = (String, Int -> Vector a -> CellType -> VTKAttr)

data VTKAttr =  ScalarData Double
             | VectorData Vec3
             | NormalData Vec3
             | TensorData Mat3

data CellType = VTK_VERTEX
              | VTK_POLY_VERTEX
              | VTK_LINE
              | VTK_POLY_LINE
              | VTK_TRIANGLE
              | VTK_TRIANGLE_STRIP
              | VTK_POLYGON
              | VTK_PIXEL
              | VTK_QUAD 
              | VTK_TETRA
              | VTK_VOXEL
              | VTK_HEXAHEDRON
              | VTK_WEDGE
              | VTK_PYRAMID
              | VTK_QUADRATIC_EDGE
              | VTK_QUADRATIC_TRIANGLE
              | VTK_QUADRATIC_QUAD
              | VTK_QUADRATIC_TETRA
              | VTK_QUADRATIC_HEXAHEDRON
                deriving (Eq)

instance Show CellType where
  show = show . evalCellType

evalCellType x = case x of
  VTK_VERTEX               -> 1
  VTK_POLY_VERTEX          -> 2
  VTK_LINE                 -> 3
  VTK_POLY_LINE            -> 4
  VTK_TRIANGLE             -> 5
  VTK_TRIANGLE_STRIP       -> 6
  VTK_POLYGON              -> 7
  VTK_PIXEL                -> 8
  VTK_QUAD                 -> 9
  VTK_TETRA                -> 10
  VTK_VOXEL                -> 11
  VTK_HEXAHEDRON           -> 12
  VTK_WEDGE                -> 13
  VTK_PYRAMID              -> 14
  VTK_QUADRATIC_EDGE       -> 21
  VTK_QUADRATIC_TRIANGLE   -> 22
  VTK_QUADRATIC_QUAD       -> 23
  VTK_QUADRATIC_TETRA      -> 24
  VTK_QUADRATIC_HEXAHEDRON -> 25

toTxt::(Show a)=>a -> Text
toTxt = T.pack.show
