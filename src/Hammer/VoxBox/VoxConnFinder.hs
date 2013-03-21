{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Hammer.VoxBox.VoxConnFinder
  ( grainFinder
  , finderVoxConn
  , VoxConn  (voxConnRange, voxConnMap, voxConnList)
  , HasConn  (..)
  , SubBox   (..)
  , GridConn (..)
  ) where

import qualified Data.HashMap.Strict            as HM
import qualified Data.Vector                    as V
  
import           Control.Applicative            ((<|>))
import           Control.Monad.ST               (runST)
import           Data.HashMap.Strict            (HashMap)
import           Data.Hashable                  (Hashable)
import           Data.Vector                    (Vector)
import           Data.Vector.Mutable            (write)
import           Hammer.MicroGraph              (GrainID, mkGrainID)
import           Hammer.VoxBox.Base

import           Control.DeepSeq
import           Control.Parallel.Strategies

import           Debug.Trace

dbg t a = trace (t ++ show a) a

-- ================================================================================

-- | Basic data structure for D&C boxes used to find connected voxels.   
data VoxConn cont grid = VoxConn
  { voxConnRange :: VoxBoxRange
  , voxConnMap   :: Maybe (cont Int)
  , voxConnList  :: Maybe (HashMap Int (Vector grid))
  }
             
instance (Show (c Int), Show g)=> Show (VoxConn c g) where
  show (VoxConn vbr dm dl) = "VoxConn (" ++ show vbr ++ ") {"
                           ++ show dm ++ "; " ++ show dl ++ " }"
  
instance (NFData (c Int), NFData g)=> NFData (VoxConn c g) where
  rnf (VoxConn vbr dm dl) = rnf vbr `seq` rnf dm `seq` rnf dl


class GridConn pos where
  toGridType      :: VoxelPos -> pos
  toVoxelPos      :: pos -> VoxelPos
  getConnPos      :: CartesianDir -> pos -> Maybe (pos, pos)
  getCrossConnPos :: CartesianDir -> pos -> Vector (pos, CrossDir)

instance GridConn VoxelPos where
  toGridType       = id
  toVoxelPos       = id 
  getConnPos dir p = return $ case dir of
      XDir -> (p, p #+# VoxelPos 1 0 0)
      YDir -> (p, p #+# VoxelPos 0 1 0)
      ZDir -> (p, p #+# VoxelPos 0 0 1)
  getCrossConnPos = getCrossPos


-- | Type family class that allows different data containers for input and output data.
-- The main idea is to have two types of containers: @Vector@ for full grid e.g. orientation
-- maps to grains ID and @HashMap@ for sparse grid as in faces or edges where only a fraction
-- of the voxels are used.
class SubBox c where
  getValue       :: c a -> VoxBoxRange -> VoxelPos -> Maybe a
  getValueUnsafe :: c a -> VoxBoxRange -> VoxelPos -> a
  initVoxConn    :: (GridConn g)=> c a -> VoxBoxRange -> VoxelPos -> VoxConn c g
  mergeDataMap   :: VoxConn c g -> VoxConn c g -> Maybe (c Int)
  updateDataMap  :: (GridConn g)=> Vector g -> Int -> VoxBoxRange -> c Int -> c Int
 
-- | Define if a certain property is connected between two neighbor voxels
-- along a specific direction. 
class HasConn a where
  -- | Direct connections (face-to-face) regarding the @CartesianDir@.
  isConn      :: Maybe a -> Maybe a -> CartesianDir -> Bool
  -- | Diagonal connections (edge-to-edge) regarding the @CrossDir@.
  isCrossConn :: Maybe a -> Maybe a -> CrossDir -> Bool

instance HasConn Int where
  {-# INLINE isConn  #-}
  isConn (Just a) (Just b) _ = a == b 
  isConn Nothing  Nothing  _ = True
  isConn _        _        _ = False
  
  {-# INLINE isCrossConn  #-}
  isCrossConn _ _ _ = False

-- ================================================================================
  
-- | Function to find grains in voxels with @Int@ property. Based on @finderVoxConn@ for Vector Int.
grainFinder :: VoxBox Int -> Maybe (VoxBox GrainID, HashMap Int (Vector VoxelPos))
grainFinder vb@VoxBox{..} = let
  found = finderVoxConn grainID dimension
  in case (voxConnMap found, voxConnList found) of
    (Just m, Just l) -> let
      new_vec  = V.map mkGrainID m
      in return (vb { grainID = new_vec }, l)
    _ -> Nothing

-- | Main function from this module. Given a certain property within a data container (typically
-- @Vector@ or @HashMap@) and together with its @VoxBoxRange@, this function scans and group the
-- connected neighbor voxels by given them a unique ID.
-- The scan is based on a divide & conquer algorithm where the connected voxels are tested along the
-- boarder of the divided space. The algorithm can run in a multi-core device for extra speed.
finderVoxConn :: (HasConn a, SubBox c, NFData (c Int), NFData g, Show (c Int), GridConn g)
              => c a -> VoxBoxRange -> VoxConn c g
finderVoxConn vec range = go range
  where
    merge = mergeVoxConn vec range
    
    --go :: VoxBoxRange -> Maybe (VoxConn c a)
    go boxrange = case splitInTwoBox boxrange of
      Nothing -> let
        (VoxBoxRange p _) = boxrange
        in initVoxConn vec range p
      Just (vb1, vb2)
        | sizeVoxBoxRange boxrange >= 500 -> run vb1 vb2 rpar
        | otherwise                       -> run vb1 vb2 rseq
        where
        run vbA vbB strat = runEval $ do
          b1 <- strat (go vbA)
          b2 <- strat (go vbB)
          rdeepseq b1
          rdeepseq b2
          return (merge b1 b2)


tryConn :: (HasConn a, SubBox cont, GridConn pos)
        => cont a
        -> VoxBoxRange
        -> CartesianDir
        -> pos
        -> Vector (pos, pos)
tryConn as vbr dir p = case getConnPos dir p of
  Just connPair 
    | testConn connPair -> connPair `V.cons` filterCross
    | otherwise         -> filterCross
  _ -> filterCross
  where
    getter = getValue as vbr . toVoxelPos
  
    testConn (a,b) = isConn (getter a) (getter b) dir

    filterCross = let
      f (x, d) = isCrossConn (getter p) (getter x) d
      cs       = getCrossConnPos dir p
      in V.map (\x -> (p, fst x)) $ V.filter f cs


mixMaps :: (SubBox c, GridConn g)=> VoxConn c g -> Vector (g, g) -> VoxConn c g
mixMaps db@VoxConn{..} conn = case (voxConnMap, voxConnList) of
  (Just m, Just l) -> let
    (new_m, new_l) = V.foldl' (commitConn voxConnRange) (m, l) conn
    in db { voxConnMap = return new_m, voxConnList = return new_l }
  _                -> db

commitConn ::(SubBox c, GridConn g)
           => VoxBoxRange
           -> (c Int, HashMap Int (Vector g))
           -> (g, g)
           -> (c Int, HashMap Int (Vector g))
commitConn range maps@(connMap, connList) (p1, p2) = let
  
  compareBoth gid1 gid2
    -- Check if it has been megered before (Int_1 == Int_2)
    | gid1 == gid2 = Nothing
    | gid1 <  gid2 = takeToFrom gid1 gid2
    | otherwise    = takeToFrom gid2 gid1
      
  -- Take the VID set from gid1 to gid2
  -- gid1 -> Dominate grain ID
  -- gid2 -> its content will be merge into gid1
  takeToFrom gid1 gid2 = do
    set <- gidSet
    return (modShB set, modGLM set)
    where
      modShB set = updateDataMap set gid1 range connMap
      gidSet     = HM.lookup gid2 connList
      delGLM     = HM.delete gid2 connList
      modGLM set = HM.insertWith (V.++) gid1 set delGLM
      
  commit = do
    ogid1 <- getValue connMap range (toVoxelPos p1)
    ogid2 <- getValue connMap range (toVoxelPos p2)
    compareBoth ogid1 ogid2

  in maybe maps id commit
     
mergeMaybe :: (a -> a -> Maybe a) -> Maybe a -> Maybe a -> Maybe a
mergeMaybe foo (Just a) (Just b) = foo a b
mergeMaybe _         a        b  = a <|> b

mergeDataList :: VoxConn c g -> VoxConn c g -> Maybe (HashMap Int (Vector g)) 
mergeDataList d1 d2 = let
  func x1 x2 = return $ HM.union x1 x2
  in mergeMaybe func (voxConnList d1) (voxConnList d2)

mergeVoxConn :: (HasConn a, SubBox c, GridConn g)
             => c a -> VoxBoxRange -> VoxConn c g -> VoxConn c g -> VoxConn c g
mergeVoxConn as vbr d1 d2 = case mergeVoxBoxRange (voxConnRange d1) (voxConnRange d2) of
  Just (newBR, dir, wall) -> let
    uniGrainMap  = mergeDataMap  d1 d2 
    uniGrainList = mergeDataList d1 d2
    wallGrid     = V.map toGridType wall 
    withConn     = V.concatMap (tryConn as vbr dir) wallGrid
    newBD        = VoxConn newBR uniGrainMap uniGrainList
    in mixMaps newBD withConn
  _ -> error "[VoxConnFinder] I can't merge these two VoxBoxRange's"


mergeVoxBoxRange :: VoxBoxRange -> VoxBoxRange
                 -> Maybe (VoxBoxRange, CartesianDir, Vector VoxelPos)
mergeVoxBoxRange b1 b2
  | b1 == b2                           = Nothing
  | testX tur1 tul2 && testX blr1 bll2 = out bll1 tur2 XDir blr1 tul2
  | testX tur2 tul1 && testX blr2 bll1 = out bll2 tur1 XDir blr2 tul1
                                         
  | testY tul1 tll2 && testY bur1 blr2 = out bll1 tur2 YDir bur1 tll2
  | testY tul2 tll1 && testY bur2 blr1 = out bll2 tur1 YDir bur2 tll1
                                         
  | testZ tur1 bur2 && testZ tll1 bll2 = out bll1 tur2 ZDir bur2 tll1
  | testZ tur2 bur1 && testZ tll2 bll1 = out bll2 tur1 ZDir bur1 tll2
  | otherwise                          = trace "==++ No merge!!" Nothing
  where
    (bll1, _, bur1, blr1, tur1, _, tll1, tul1) = getBox b1
    (bll2, _, bur2, blr2, tur2, _, tll2, tul2) = getBox b2
    testX a b = a #+# (VoxelPos 1 0 0) == b
    testY a b = a #+# (VoxelPos 0 1 0) == b
    testZ a b = a #+# (VoxelPos 0 0 1) == b
    out p1 p2 dir w1 w2 = let
      br = posRange2VoxBox p1 p2
      wl = posRange2VoxBox w1 w2
      in return (br, dir, scanWall dir wl)

-- | Extract a list of all @VoxelPos@ of the lower side of a plane
-- defined by the @CartesianDir@. Used to scan the connections along the
-- wall (boarder between two divided spaces).
scanWall :: CartesianDir -> VoxBoxRange -> Vector VoxelPos
scanWall dir br = let
  (lx, ux, ly, uy, lz, uz) = getBoxLinRange br
  in V.fromList $ case dir of
    XDir -> [VoxelPos lx j k | j <- [ly..uy], k <- [lz..uz]]
    YDir -> [VoxelPos i ly k | i <- [lx..ux], k <- [lz..uz]]
    ZDir -> [VoxelPos i j lz | i <- [lx..ux], j <- [ly..uy]]
   
-- =============================  Instance SubBox for Vector ==============================

instance SubBox Vector where   
  {-# INLINE getValue #-}
  getValue vec range pos = (vec V.!?) =<< (range %@? pos)
  {-# INLINE getValueUnsafe #-}
  getValueUnsafe vec range pos = vec V.! (range %@ pos)
  updateDataMap = updateDB_Vec
  mergeDataMap  = mergeVecBox
  initVoxConn _ vbrGlobal p = let
    vbr = VoxBoxRange p (VoxBoxDim 1 1 1)
    (dm, dl) = case getVoxelID vbrGlobal p of
      Just gid -> let
        l  = HM.singleton gid (V.singleton $ toGridType p)
        m  = V.singleton gid
        in (return m, return l)
      _ -> (Nothing, Nothing)
    in VoxConn vbr dm dl

{-# INLINE mergeVecBox #-}
mergeVecBox :: VoxConn Vector g -> VoxConn Vector g -> Maybe (Vector Int) 
mergeVecBox gma gmb = let
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
    in return $ V.generate size getEither
  in do
    (new_range, _, _) <- mergeVoxBoxRange ranA ranB
    let new_size = sizeVoxBoxRange new_range
    mergeMaybe (func new_range new_size) vecA vecB

-- ----------------------  Critical Peformance operations ------------------------------
  
{-# INLINE updateDB_Vec #-}
updateDB_Vec :: (GridConn g)=> Vector g -> Int -> VoxBoxRange -> Vector Int -> Vector Int
updateDB_Vec set gid vbr vec = let
  in runST $ do
  vec_m <- V.unsafeThaw vec 
  V.mapM_ (\p -> write vec_m (vbr ~@# (toVoxelPos p)) gid) set
  V.unsafeFreeze vec_m
  
-- This is an optimazed version of ~@#. Going a little bit WET for shake of performance.
-- according to GHC core all math op. are primop's
  
{-# INLINE (~@#) #-} 
(~@#) :: VoxBoxRange -> VoxelPos -> Int
(VoxBoxRange (VoxelPos x0 y0 z0) (VoxBoxDim dx dy _)) ~@# (VoxelPos x y z) =
  (x-x0) + dx*(y-y0) + dx*dy*(z-z0)

-- =============================  Instance SubBox for HashMap ==============================
 
instance SubBox (HashMap VoxelPos) where   
  {-# INLINE getValue #-}
  getValue set _ x = HM.lookup x set
  {-# INLINE getValueUnsafe #-}
  getValueUnsafe set _ pos = unsafe_value_HM set pos 
  updateDataMap = updateDB_HM
  mergeDataMap  = mergeHMBox 
  initVoxConn hm vbrGlobal p = let
    vbr = VoxBoxRange p (VoxBoxDim 1 1 1)
    (dm, dl) = if not (HM.member p hm)
               then (Nothing, Nothing)
               else
                 case getVoxelID vbrGlobal p of
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

