{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Hammer.VoxBox.VoxConnTypes where

import qualified Data.HashMap.Strict            as HM
import qualified Data.IntMap                    as IM
import qualified Data.Vector                    as V
import qualified Data.Vector.Generic            as G
import qualified Data.Vector.Unboxed            as U
import qualified Hammer.Math.SparseMatrix       as SP

import           Control.Applicative            ((<|>))
import           Control.Monad.ST               (runST)
import           Data.HashMap.Strict            (HashMap)
import           Data.Hashable                  (Hashable)
import           Data.IntMap                    (IntMap)
import           Data.Vector                    (Vector)
import           Data.Vector.Generic.Mutable    (write)
import           Hammer.Math.SparseMatrix       (Sparse3)

import           Control.DeepSeq

import           Hammer.VoxBox.Base

--import           Debug.Trace
--dbg t a = trace (t ++ show a) a

-- =======================================================================================

-- | Basic data structure for D&C boxes used to find connected voxels.
data VoxConn cont grid =
  VoxConn
  { voxConnRange :: VoxBoxRange
  , voxConnMap   :: Maybe (cont Int)
  , voxConnList  :: Maybe (HashMap Int (Vector grid))
  }

class GridConn g where
  toGridType      :: VoxelPos -> g
  toVoxelPos      :: g -> VoxelPos
  getConnPos      :: CartesianDir -> g -> Maybe  (g, g)
  getCrossConnPos :: CartesianDir -> g -> Vector (g, CrossDir)

-- | Type family class that allows different data containers for input and output data.
-- The main idea is to have two types of containers: @Vector@ for full grid e.g.
-- orientation maps to grains ID and @HashMap@ for sparse grid as in faces or edges where
-- only a fraction of the voxels are used.
class Cont c a where
  getValue       :: c a -> VoxBoxRange -> VoxelPos -> Maybe a
  getValueUnsafe :: c a -> VoxBoxRange -> VoxelPos -> a

class (Cont oc Int)=> HasVoxConn oc g where
  mergeDataMap  :: VoxConn oc g -> VoxConn oc g -> Maybe (oc Int)
  initVoxConn   :: (Cont ic a)=> ic a -> VoxBoxRange -> VoxelPos -> VoxConn oc g
  updateDataMap :: Vector g -> Int -> VoxBoxRange -> oc Int -> oc Int

-- | Define if a certain property is connected between two neighbor voxels along a
-- specific direction.
class HasConn a where
  -- | Direct connections (face-to-face) regarding the @CartesianDir@.
  isConn      :: Maybe a -> Maybe a -> CartesianDir -> Bool
  -- | Diagonal connections (edge-to-edge) regarding the @CrossDir@.
  isCrossConn :: Maybe a -> Maybe a -> CrossDir -> Bool

-- =======================================================================================

instance (Show (c Int), Show g)=> Show (VoxConn c g) where
  show (VoxConn vbr dm dl) = "VoxConn (" ++ show vbr ++ ") {"
                           ++ show dm ++ "; " ++ show dl ++ " }"

instance (NFData (c Int), NFData g)=> NFData (VoxConn c g) where
  rnf (VoxConn vbr dm dl) = rnf vbr `seq` rnf dm `seq` rnf dl

instance GridConn VoxelPos where
  {-# SPECIALIZE instance GridConn VoxelPos #-}
  toGridType       = id
  toVoxelPos       = id
  getConnPos dir p = return $ case dir of
      XDir -> (p, p #+# VoxelPos 1 0 0)
      YDir -> (p, p #+# VoxelPos 0 1 0)
      ZDir -> (p, p #+# VoxelPos 0 0 1)
  getCrossConnPos = getCrossPos

instance HasConn Int where
  {-# SPECIALIZE instance HasConn Int #-}
  isConn (Just a) (Just b) _ = a == b
  isConn Nothing  Nothing  _ = True
  isConn _        _        _ = False

  isCrossConn _ _ _ = False

-- =======================================================================================

mergeMaybe :: (a -> a -> Maybe a) -> Maybe a -> Maybe a -> Maybe a
mergeMaybe foo (Just a) (Just b) = foo a b
mergeMaybe _         a        b  = a <|> b

-- =======================================================================================
-- =======================  Instance SubBox for Generinc Vector ==========================
-- =======================================================================================

{-# INLINE getValueG #-}
getValueG :: (G.Vector v b)=> v b -> VoxBoxRange -> VoxelPos -> Maybe b
getValueG vec range pos = (vec G.!?) =<< (range %@? pos)

{-# INLINE getValueUnsafeG #-}
getValueUnsafeG :: (G.Vector v a)=> v a -> VoxBoxRange -> VoxelPos -> a
getValueUnsafeG vec range pos = vec G.! (range %@ pos)

initVoxConnG :: (G.Vector cont Int, GridConn grid
                )=> t -> VoxBoxRange -> VoxelPos -> VoxConn cont grid
initVoxConnG _ vbrGlobal p = let
  vbr = VoxBoxRange p (VoxBoxDim 1 1 1)
  (dm, dl) = case getVoxelID vbrGlobal p of
    Just gid -> let
      l  = HM.singleton gid (V.singleton $ toGridType p)
      m  = G.singleton gid
      in (return m, return l)
    _ -> (Nothing, Nothing)
  in VoxConn vbr dm dl

{-# INLINE mergeVecBoxG #-}
mergeVecBoxG :: (G.Vector v Int, Cont v Int)=> VoxConn v g -> VoxConn v g -> Maybe (v Int)
mergeVecBoxG gma gmb = let
  ranA = voxConnRange gma
  ranB = voxConnRange gmb
  vecA = voxConnMap   gma
  vecB = voxConnMap   gmb
  func range size vA vB = let
    getEither i = let
      getRightBox pos
        | isInBox ranA pos = getValueUnsafe vA ranA pos
        | isInBox ranB pos = getValueUnsafe vB ranB pos
        | otherwise        = error "[GrainFinder] unable to merge two VoxConn's."
      in getRightBox (range %# i)
    in return $ G.generate size getEither
  in do
    (new_range, _, _) <- mergeVoxBoxRange ranA ranB
    let new_size = sizeVoxBoxRange new_range
    mergeMaybe (func new_range new_size) vecA vecB

-- ------------------------  Critical Peformance operations ------------------------------

{-# INLINE updateDB_VecG #-}
updateDB_VecG :: ( G.Vector v2 a, G.Vector v1 g, G.Vector v a
                 , GridConn g, G.Mutable v ~ G.Mutable v2) =>
                 v1 g -> a -> VoxBoxRange -> v a -> v2 a
updateDB_VecG set gid vbr vec = let
  in runST $ do
  vec_m <- G.unsafeThaw vec
  G.mapM_ (\p -> write vec_m (vbr ~@# (toVoxelPos p)) gid) set
  G.unsafeFreeze vec_m

-- This is an optimazed version of ~@#. Going a little bit WET for shake of performance.
-- according to GHC core all math op. are primop's

{-# INLINE (~@#) #-}
(~@#) :: VoxBoxRange -> VoxelPos -> Int
(VoxBoxRange (VoxelPos x0 y0 z0) (VoxBoxDim dx dy _)) ~@# (VoxelPos x y z) =
  (x-x0) + dx*(y-y0) + dx*dy*(z-z0)

-- =======================================================================================
-- ============================  Instance SubBox for Vector ==============================
-- =======================================================================================

instance (U.Unbox a)=> Cont U.Vector a where
  {-# SPECIALIZE instance (U.Unbox a)=> Cont U.Vector a #-}
  {-# INLINE getValue #-}
  getValue       = getValueG
  {-# INLINE getValueUnsafe #-}
  getValueUnsafe = getValueUnsafeG

instance (GridConn g)=> HasVoxConn U.Vector g where
  updateDataMap  = updateDB_VecG
  mergeDataMap   = mergeVecBoxG
  initVoxConn    = initVoxConnG


instance Cont Vector a where
  {-# SPECIALIZE instance Cont Vector a #-}
  {-# INLINE getValue #-}
  getValue       = getValueG
  {-# INLINE getValueUnsafe #-}
  getValueUnsafe = getValueUnsafeG

instance (GridConn g)=> HasVoxConn Vector g where
  {-# SPECIALIZE instance (GridConn g)=> HasVoxConn Vector g #-}
  updateDataMap  = updateDB_VecG
  mergeDataMap   = mergeVecBoxG
  initVoxConn    = initVoxConnG

-- =======================================================================================
-- ======================= Instance SubBox for HashMap VoxelPos ==========================
-- =======================================================================================

instance Cont (HashMap VoxelPos) a where
  {-# SPECIALIZE instance Cont (HashMap VoxelPos) a #-}
  {-# INLINE getValue #-}
  getValue set _ x = HM.lookup x set
  {-# INLINE getValueUnsafe #-}
  getValueUnsafe set _ pos = unsafe_value_HM set pos

instance (GridConn g)=> HasVoxConn (HashMap VoxelPos) g where
  updateDataMap = updateDB_HM
  mergeDataMap  = mergeHMBox
  initVoxConn hm vbrGlobal p = let
    vbr = VoxBoxRange p (VoxBoxDim 1 1 1)
    (dm, dl) = case getValue hm vbrGlobal p of
      Nothing -> (Nothing, Nothing)
      Just _  -> case getVoxelID vbrGlobal p of
        Just gid -> let
          l  = HM.singleton gid (V.singleton $ toGridType p)
          m  = HM.singleton p gid
          in (return m, return l)
        _ -> (Nothing, Nothing)
    in VoxConn vbr dm dl

{-# INLINE unsafe_value_HM #-}
unsafe_value_HM :: (Eq k, Hashable k)=> HashMap k a -> k -> a
unsafe_value_HM hm i = case HM.lookup i hm of
  Just x -> x
  _      -> error "[VoxConnFinder] Can't retrive this position from the HashMap."

{-# INLINE updateDB_HM #-}
updateDB_HM :: (GridConn g)=> Vector g -> Int -> VoxBoxRange
            -> HashMap VoxelPos Int -> HashMap VoxelPos Int
updateDB_HM set gid _ hm = let
  func acc p = HM.adjust (const gid) (toVoxelPos p) acc
  in V.foldl' func hm set

{-# INLINE mergeHMBox #-}
mergeHMBox :: (Eq k, Hashable k)
           => VoxConn (HashMap k) g
           -> VoxConn (HashMap k) g
           -> Maybe (HashMap k Int)
mergeHMBox d1 d2 = let
  func x1 x2 = return $ HM.union x1 x2
  in mergeMaybe func (voxConnMap d1) (voxConnMap d2)

-- =======================================================================================
-- ============================  Instance SubBox for HashMap Int =========================
-- =======================================================================================

instance Cont (HashMap Int) a where
  {-# INLINE getValue #-}
  getValue set range pos = (range %@? pos) >>= \x -> HM.lookup x set
  {-# INLINE getValueUnsafe #-}
  getValueUnsafe set range pos = unsafe_value_HM set (range %@ pos)

instance (GridConn g)=> HasVoxConn (HashMap Int) g where
  updateDataMap = updateDB_HM_Int
  mergeDataMap  = mergeHMBox
  initVoxConn hm vbrGlobal p = let
    pid = vbrGlobal %@ p
    vbr = VoxBoxRange p (VoxBoxDim 1 1 1)
    (dm, dl) = case getValue hm vbrGlobal p of
      Nothing -> (Nothing, Nothing)
      Just _  -> case getVoxelID vbrGlobal p of
        Just gid -> let
          l  = HM.singleton gid (V.singleton $ toGridType p)
          m  = HM.singleton pid gid
          in (return m, return l)
        _ -> (Nothing, Nothing)
    in VoxConn vbr dm dl

{-# INLINE updateDB_HM_Int #-}
updateDB_HM_Int :: (GridConn g)=> Vector g -> Int -> VoxBoxRange
            -> HashMap Int Int -> HashMap Int Int
updateDB_HM_Int set gid range hm = let
  func acc p = let
    pos = range %@ (toVoxelPos p)
    in HM.adjust (const gid) pos  acc
  in V.foldl' func hm set

-- =======================================================================================
-- ==========================  Instance SubBox for IntMap Int ============================
-- =======================================================================================

instance Cont IntMap a where
  {-# INLINE getValue #-}
  getValue set range pos = (range %@? pos) >>= \x -> IM.lookup x set
  {-# INLINE getValueUnsafe #-}
  getValueUnsafe set range pos = unsafe_value_IM set (range %@ pos)

instance (GridConn g)=> HasVoxConn IntMap g where
  updateDataMap = updateDB_IM_Int
  mergeDataMap  = mergeIMBox
  initVoxConn hm vbrGlobal p = let
    pid = vbrGlobal %@ p
    vbr = VoxBoxRange p (VoxBoxDim 1 1 1)
    (dm, dl) = case getValue hm vbrGlobal p of
      Nothing -> (Nothing, Nothing)
      Just _  -> case getVoxelID vbrGlobal p of
        Just gid -> let
          l  = HM.singleton gid (V.singleton $ toGridType p)
          m  = IM.singleton pid gid
          in (return m, return l)
        _ -> (Nothing, Nothing)
    in VoxConn vbr dm dl

{-# INLINE unsafe_value_IM #-}
unsafe_value_IM :: IntMap a -> Int -> a
unsafe_value_IM hm i = case IM.lookup i hm of
  Just x -> x
  _      -> error "[VoxConnFinder] Can't retrive this position from the HashMap."

{-# INLINE updateDB_IM_Int #-}
updateDB_IM_Int :: (GridConn g)=> Vector g -> Int -> VoxBoxRange
            -> IntMap Int -> IntMap Int
updateDB_IM_Int set gid range hm = let
  func acc p = let
    pos = range %@ (toVoxelPos p)
    in IM.adjust (const gid) pos  acc
  in V.foldl' func hm set

{-# INLINE mergeIMBox #-}
mergeIMBox :: VoxConn IntMap g
           -> VoxConn IntMap g
           -> Maybe (IntMap Int)
mergeIMBox d1 d2 = let
  func x1 x2 = return $ IM.union x1 x2
  in mergeMaybe func (voxConnMap d1) (voxConnMap d2)

-- =======================================================================================
-- ==========================  Instance SubBox for Sparse3  ==============================
-- =======================================================================================

vp2spIn :: VoxelPos -> (Int, Int, Int)
vp2spIn (VoxelPos x y z) = (x,y,z)

instance Cont Sparse3 a where
  {-# SPECIALIZE instance Cont Sparse3 a #-}
  {-# INLINE getValue #-}
  getValue set _ x = SP.lookup (vp2spIn x) set
  {-# INLINE getValueUnsafe #-}
  getValueUnsafe set _ pos = unsafe_value_SP set pos

instance (GridConn g)=> HasVoxConn Sparse3 g where
  updateDataMap = updateDB_SP
  mergeDataMap  = mergeBox_SP
  initVoxConn hm vbrGlobal p = let
    vbr = VoxBoxRange p (VoxBoxDim 1 1 1)
    org = SP.Sparse3Org $ vp2spIn p
    (dm, dl) = case getValue hm vbrGlobal p of
      Nothing -> (Nothing, Nothing)
      Just _  -> case getVoxelID vbrGlobal p of
        Just gid -> let
          l  = HM.singleton gid (V.singleton $ toGridType p)
          m  = SP.singleton org gid
          in (return m, return l)
        _ -> (return $ SP.emptySingleton org, Nothing)
    in VoxConn vbr dm dl

{-# INLINE unsafe_value_SP #-}
unsafe_value_SP :: Sparse3 a -> VoxelPos -> a
unsafe_value_SP sp i = case SP.lookup (vp2spIn i) sp of
  Just x -> x
  _      -> error "[VoxConnFinder] Can't retrive this position from the HashMap."

{-# INLINE updateDB_SP #-}
updateDB_SP :: (GridConn g)=> Vector g -> Int -> VoxBoxRange -> Sparse3 Int -> Sparse3 Int
updateDB_SP set gid _ sp = let
  vec = V.map (\p -> (vp2spIn $ toVoxelPos p, gid)) set
  in SP.adjust vec sp

{-# INLINE mergeBox_SP #-}
mergeBox_SP :: VoxConn Sparse3 g
            -> VoxConn Sparse3 g
            -> Maybe (Sparse3 Int)
mergeBox_SP d1 d2 = mergeMaybe SP.mergeSparse3 (voxConnMap d1) (voxConnMap d2)
