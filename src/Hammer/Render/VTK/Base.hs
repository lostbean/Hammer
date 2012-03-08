
module Hammer.Render.VTK.Base where

import Data.Vector (Vector)
import Data.Text (Text)
import Hammer.Math.Vector hiding (Vector)


class RenderCell shape where
  makeCell::shape -> Vector Int
  getType ::shape -> CellType

type MultiPieceVTK =  Vector VTK

data VTK = VTK { name      ::Text
               , isBinary  ::Bool
               , dataSet   ::VTKDataSet 
               , pointData ::[VTKAttrPoint]
               , cellData  ::[VTKAttrCell]
               }

data VTKDataSet = StructPoint { dimSP       ::(Int, Int, Int)
                              , originSP    ::(Double, Double, Double)
                              , spaceSP     ::(Double, Double, Double) }
                  
                | StructGrid  { dimSG       ::(Int, Int, Int)
                              , setSG       ::Vector Vec3              }
                
                | RectLinGrid { dimRG       ::(Int, Int, Int)
                              , setxRG      ::Vector Vec3              
                              , setyRG      ::Vector Vec3
                              , setzRG      ::Vector Vec3              }
                
                | UnstructGrid { setUG      ::Vector Vec3             
                               , cellUG     ::Vector Int
                               , cellOffUG  ::Vector Int
                               , cellTypeUG ::Vector CellType          }
                  


type VTKAttrPoint = (String, Int -> Vec3 -> VTKAttr)
type VTKAttrCell  = (String, Int -> Vector Vec3 -> CellType -> VTKAttr)

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
