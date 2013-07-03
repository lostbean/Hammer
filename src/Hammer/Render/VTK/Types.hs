{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Hammer.Render.VTK.Types where

import qualified Data.ByteString.Lazy.Builder     as BB
import qualified Data.Text.Lazy.Encoding          as TE
import qualified Data.Vector                      as V

import           Data.Monoid                      ((<>))
import           Data.Text                        (Text)
import           Data.Text.Lazy.Builder           (toLazyText)
import           Data.Vector.Unboxed              (Unbox, Vector)

import           Data.Text.Lazy.Builder.Int
import           Data.Text.Lazy.Builder.RealFloat

import           Hammer.Math.Algebra

-- =======================================================================================

class RenderPoint point where
  renderPoint       :: point -> BB.Builder
  renderBinaryPoint :: point -> BB.Builder

class (RenderPoint a, Unbox a)=> RenderElemVTK a

instance RenderElemVTK Int
instance RenderElemVTK Double
instance RenderElemVTK Vec3
instance RenderElemVTK Vec2
instance RenderElemVTK Mat3

class RenderCell shape where
  makeCell :: shape -> Vector Int
  getType  :: shape -> CellType

-- | This class defines how to create and render attributes.
class RenderAttr attr where
  -- | This function creates an attribute of type 'attr' for each cell.
  -- It is itself a function that is given to render, where:
  --
  -- * Int      : is the position in the cell list
  --
  -- * Vector a : is the list of point the cell
  --
  -- * CellType : is the type of the cell
  --
  -- > let attr = mkCellAttr "color" (\i x cellType -> (Vec3 1 1 1) &*
  --   1/(evalCellType cellType))
  --
  mkCellAttr  :: String -> (Int -> Vector a -> CellType -> attr) -> VTKAttrCell a

  -- | This function creates an attribute of type 'attr' for each point.
  -- It is itself a function that is given to render, where:
  --
  -- * Int : is the position in the point list
  --
  -- * a   : is the value of the point
  --
  -- > let attr = mkCellAttr "grainID" (\i x -> grainIDTable!i)
  --
  mkPointAttr :: String -> (Int -> a -> attr) -> VTKAttrPoint a

type MultiPieceVTK a =  Vector (VTK a)

newtype AttrPoint a attr = AttrPoint (Int -> a -> attr)
newtype AttrCell  a attr = AttrCell  (Int -> Vector a -> CellType -> attr)

type VTKAttrPoint a = VTKAttr (AttrPoint a)
type VTKAttrCell  a = VTKAttr (AttrCell  a)

data (RenderElemVTK a)=> VTK a = VTK
  { name      :: Text
  , isBinary  :: Bool
  , dataSet   :: VTKDataSet a
  , pointData :: [VTKAttrPoint a]
  , cellData  :: [VTKAttrCell  a]
  }

instance (Show a, RenderElemVTK a)=> Show (VTK a) where
  show (VTK{..}) = concat [show name, " ", show isBinary, " ", show dataSet]

data VTKAttr a  = IDData     String (a Int)
                | ScalarData String (a Double)
                | VectorData String (a Vec3)
                | TensorData String (a Mat3)

data VTKDataSet a =
    StructPoint
    { dimSP    :: (Int, Int, Int)
    , originSP :: (Double, Double, Double)
    , spaceSP  :: (Double, Double, Double)
    , setSP    :: Vector a
    }

  | StructGrid
    { dimSG :: (Int, Int, Int)
    , setSG :: Vector a
    }

  | RectLinGrid
    { dimRG  :: (Int, Int, Int)
    , setxRG :: Vector a
    , setyRG :: Vector a
    , setzRG :: Vector a
    }

  | UnstructGrid
    { setUG      :: Vector a
    , cellUG     :: Vector Int
    , cellOffUG  :: Vector Int
    , cellTypeUG :: V.Vector CellType
    } deriving (Show)

data CellType
  = VTK_VERTEX
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

-- | Evaluates the 'CellType' according the VTK standards.
evalCellType :: CellType -> Int
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

instance RenderPoint Mat3 where
  renderPoint (Mat3 x y z) =
    renderPoint x <>
    renderPoint y <>
    renderPoint z

  renderBinaryPoint (Mat3 x y z) =
    renderBinaryPoint x <>
    renderBinaryPoint y <>
    renderBinaryPoint z


instance RenderPoint Vec3 where
  renderPoint (Vec3 x y z) =
    renderPoint x <>
    renderPoint y <>
    renderPoint z

  renderBinaryPoint (Vec3 x y z) =
    renderBinaryPoint x <>
    renderBinaryPoint y <>
    renderBinaryPoint z

instance RenderPoint Vec2 where
  renderPoint (Vec2 x y)       = renderPoint x <> renderPoint y
  renderBinaryPoint (Vec2 x y) = renderBinaryPoint x <> renderBinaryPoint y

instance RenderPoint Double where
  renderPoint       = (<> BB.char8 ' ') . renderDouble
  renderBinaryPoint = BB.doubleLE

instance RenderPoint Int where
  renderPoint       = (<> BB.char8 ' ') . renderInt
  renderBinaryPoint = BB.int32LE . fromIntegral

renderInt :: Int -> BB.Builder
renderInt = BB.lazyByteString . TE.encodeUtf8 . toLazyText . decimal

renderDouble :: Double -> BB.Builder
renderDouble = renderF 4

renderF :: (RealFloat a)=> Int -> a -> BB.Builder
renderF n = BB.lazyByteString . TE.encodeUtf8 . toLazyText . formatRealFloat Fixed (Just n)




instance RenderAttr Double where
  mkCellAttr  n func = ScalarData n (AttrCell  func)
  mkPointAttr n func = ScalarData n (AttrPoint func)

instance RenderAttr Int where
  mkCellAttr  n func = IDData n (AttrCell  func)
  mkPointAttr n func = IDData n (AttrPoint func)

instance RenderAttr Vec3 where
  mkCellAttr  n func   = VectorData n (AttrCell  func)
  mkPointAttr n func   = VectorData n (AttrPoint func)

instance RenderAttr Mat3 where
  mkCellAttr  n func   = TensorData n (AttrCell func)
  mkPointAttr n func   = TensorData n (AttrPoint func)
