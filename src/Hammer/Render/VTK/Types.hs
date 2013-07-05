{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE ExistentialQuantification #-}

module Hammer.Render.VTK.Types where

import qualified Data.ByteString.Lazy.Builder       as BB
import qualified Data.ByteString.Lazy.Builder.ASCII as BBA
import qualified Data.Text.Lazy.Encoding            as TE
import qualified Data.Vector                        as V

import           Data.Monoid                        ((<>))
import           Data.Text                          (Text)
import           Data.Text.Lazy.Builder             (toLazyText)
import           Data.Vector.Unboxed                (Unbox, Vector)
import           Data.Word                          (Word32, Word8)

import           Data.Text.Lazy.Builder.Int
import           Data.Text.Lazy.Builder.RealFloat

import           Hammer.Math.Algebra

-- =======================================================================================

data VTKNumType
  = VTK_UInt8
  | VTK_UInt
  | VTK_Int
  | VTK_Float
  deriving (Show, Eq)

renderNumType :: VTKNumType -> String
renderNumType x = case x of
  VTK_UInt8 -> "UInt8"
  VTK_UInt  -> "UInt32"
  VTK_Int   -> "Int32"
  _         -> "Float64"

class RenderPoint point where
  renderPoint       :: point -> BB.Builder
  renderBinaryPoint :: point -> BB.Builder
  pointNumberType   :: point -> VTKNumType
  pointNumberComp   :: point -> Word8

class (RenderPoint a, Unbox a)=> RenderElemVTK a

instance RenderElemVTK Word8
instance RenderElemVTK Word32
instance RenderElemVTK Int
instance RenderElemVTK Double
instance RenderElemVTK Vec3
instance RenderElemVTK Vec2
instance RenderElemVTK Mat3
instance (RenderElemVTK a)=> RenderElemVTK (a, a, a)

class RenderCell shape where
  makeCell :: shape -> Vector Int
  getType  :: shape -> CellType

-- | This function creates an attribute of type 'attr' for each cell.
-- It is itself a function that is given to render, where:
--
-- * Int      : is the position in the cell list
--
-- * Vector a : is the list of point the cell
--
-- * CellType : is the type of the cell
--
-- > let attr = mkCellAttr "color" (\i x cellType -> (Vec3 1 1 1) &* 1/(evalCellType cellType))
--
mkCellAttr :: (RenderElemVTK attr)=> String
           -> (Int -> Vector a -> CellType -> attr) -> VTKAttrCell a
mkCellAttr = VTKAttrCell

-- | This function creates an attribute of type 'attr' for each point.
-- It is itself a function that is given to render, where:
--
-- * Int : is the position in the point list
--
-- * a   : is the value of the point
--
-- > let attr = mkCellAttr "grainID" (\i x -> grainIDTable!i)
--
mkPointAttr :: (RenderElemVTK attr)=> String -> (Int -> a -> attr) -> VTKAttrPoint a
mkPointAttr = VTKAttrPoint

data VTKAttrPoint a = forall attr. (RenderElemVTK attr)=>
                      VTKAttrPoint String (Int -> a -> attr)

data VTKAttrCell  a = forall attr. (RenderElemVTK attr)=>
                      VTKAttrCell  String (Int -> Vector a -> CellType -> attr)

type MultiPieceVTK a = Vector (VTK a)

data (RenderElemVTK a)=> VTK a = VTK
  { name      :: Text
  , dataSet   :: VTKDataSet a
  , pointData :: [VTKAttrPoint a]
  , cellData  :: [VTKAttrCell a]
  }

instance (Show a, RenderElemVTK a)=> Show (VTK a) where
  show (VTK{..}) = concat [show name, " ", " ", show dataSet]

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

-- ================================= Instances RenderPoint ===============================

instance RenderPoint Mat3 where
  renderPoint (Mat3 x y z) =
    renderPoint x <>
    renderPoint y <>
    renderPoint z
  renderBinaryPoint (Mat3 x y z) =
    renderBinaryPoint x <>
    renderBinaryPoint y <>
    renderBinaryPoint z
  pointNumberType = const VTK_Float
  pointNumberComp = const 9

instance (RenderPoint a)=> RenderPoint (a, a, a) where
  renderPoint (x, y, z) =
    renderPoint x <>
    renderPoint y <>
    renderPoint z
  renderBinaryPoint (x, y, z) =
    renderBinaryPoint x <>
    renderBinaryPoint y <>
    renderBinaryPoint z
  pointNumberType (x, _, _) = pointNumberType x
  pointNumberComp           = const 3

instance RenderPoint Vec3 where
  renderPoint (Vec3 x y z) =
    renderPoint x <>
    renderPoint y <>
    renderPoint z
  renderBinaryPoint (Vec3 x y z) =
    renderBinaryPoint x <>
    renderBinaryPoint y <>
    renderBinaryPoint z
  pointNumberType = const VTK_Float
  pointNumberComp = const 3

instance RenderPoint Vec2 where
  renderPoint (Vec2 x y)       = renderPoint x <> renderPoint y
  renderBinaryPoint (Vec2 x y) = renderBinaryPoint x <> renderBinaryPoint y
  pointNumberType              = const VTK_Float
  pointNumberComp              = const 2

instance RenderPoint Double where
  renderPoint       = (<> BB.char8 ' ') . renderDouble
  renderBinaryPoint = BB.doubleLE
  pointNumberType   = const VTK_Float
  pointNumberComp   = const 1

instance RenderPoint Int where
  renderPoint       = (<> BB.char8 ' ') . BBA.intDec
  renderBinaryPoint = BB.int32LE . fromIntegral
  pointNumberType   = const VTK_Int
  pointNumberComp   = const 1

instance RenderPoint Word32 where
  renderPoint       = (<> BB.char8 ' ') . BBA.word32Dec
  renderBinaryPoint = BB.word32LE
  pointNumberType   = const VTK_UInt
  pointNumberComp   = const 1

instance RenderPoint Word8 where
  renderPoint       = (<> BB.char8 ' ') . BBA.word8Dec
  renderBinaryPoint = BB.word8
  pointNumberType   = const VTK_UInt8
  pointNumberComp   = const 1

renderInt :: Int -> BB.Builder
renderInt = BB.lazyByteString . TE.encodeUtf8 . toLazyText . decimal

renderDouble :: Double -> BB.Builder
renderDouble = renderF 4

renderF :: (RealFloat a)=> Int -> a -> BB.Builder
renderF n = BB.lazyByteString . TE.encodeUtf8 . toLazyText . formatRealFloat Fixed (Just n)
