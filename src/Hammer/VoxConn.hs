{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Hammer.VoxConn
       ( grainFinder
       , finderVoxConn
       , resetGrainIDs
       , VoxConn    (voxConnRange, voxConnMap, voxConnList)
       , HasVoxConn (..)
       , GridConn   (..)
       , Cont       (..)
       ) where

import qualified Data.HashMap.Strict            as HM
import qualified Data.Vector                    as V
import qualified Data.Vector.Unboxed            as U

import           Data.HashMap.Strict            (HashMap)
import           Data.Vector                    (Vector)

import           Control.DeepSeq
import           Control.Parallel.Strategies

import           Hammer.Math.Array              (shuffleVector)
import           Hammer.MicroGraph.Types        (GrainID, mkGrainID, unGrainID)
import           Hammer.VoxBox

import           Hammer.VoxConn.Types

--import           Debug.Trace
--dbg t a = trace (t ++ show a) a

-- =======================================================================================

-- | Reset and shuffle the grain IDs. It gives 'GrainID' values from @0@ to @n@, the number
-- of grains and then shuffle it. The shuffling randomness depends on the number of grains.
resetGrainIDs :: (VoxBox GrainID, HashMap Int (Vector VoxelPos)) ->
                 (VoxBox GrainID, HashMap Int (Vector VoxelPos))
resetGrainIDs (vb, hmap) = let
  nGrains = HM.size hmap
  shuffle = V.toList $ shuffleVector (V.enumFromN 0 nGrains)
  gv      = HM.toList hmap
  map1    = HM.fromList $ zipWith (\s (g,_) -> (g, s)) shuffle gv
  map2    = HM.fromList $ zipWith (\s (_,v) -> (s, v)) shuffle gv
  foo x   = maybe x mkGrainID (HM.lookup (unGrainID x) map1)
  in (vb { grainID = U.map foo (grainID vb) }, map2)

-- | Function to find grains in voxels with 'Int' property. Based on 'finderVoxConn' for
-- 'Vector' 'Int'.
grainFinder :: (U.Unbox a)=> (a -> a -> Bool) -> VoxBox a ->
               Maybe (VoxBox GrainID, HashMap Int (Vector VoxelPos))
grainFinder isEqual vb@VoxBox{..} = let
  found = finderVoxConn isEqual grainID dimension
  in case (voxConnMap found, voxConnList found) of
    (Just m, Just l) -> let
      new_vec  = U.map mkGrainID m
      in return (vb { grainID = new_vec }, l)
    _ -> Nothing

-- | Main function from this module. Given a certain property within a data container
-- (typically 'Vector' or 'HashMap') and together with its 'VoxBoxRange', this function
-- scans and group the connected neighbor voxels by given them a unique ID. The scan is
-- based on a divide & conquer algorithm where the connected voxels are tested along the
-- boarder of the divided space. The algorithm can run in a multi-core device for extra
-- speed.
finderVoxConn :: ( Cont ic a, HasVoxConn c g, NFData (c Int), NFData g, GridConn g
                 )=> (a -> a -> Bool) -> ic a -> VoxBoxRange -> VoxConn c g
finderVoxConn isEqual vec range = force $ go range
  where
    merge = mergeVoxConn isEqual vec range

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

-- | This function checks whether or not a given position @g@ is connected with its
-- neighbors and returns the list of connected positions.
tryConn :: (Cont c a, GridConn g)=> (a -> a -> Bool) -> c a ->
           VoxBoxRange -> CartesianDir -> g -> Vector (g, g)
tryConn isEqual as vbr dir p
  | usesCrossDir p = testConn V.++ testCrossConn
  | otherwise      = testConn
  where
    getter = getValue as vbr . toVoxelPos

    hasconn a b = maybe False id $ do
      pa <- getter a
      pb <- getter b
      return $ isEqual pa pb

    testConn = maybe V.empty V.singleton $ do
      (a, b) <- getConnPos dir p
      if hasconn a b
        then return (a, b)
        else Nothing

    testCrossConn = let
      foo = hasconn p
      cs  = V.map fst $ getCrossConnPos dir p
      in V.map (\x -> (p, x)) $ V.filter foo cs

mixMaps :: (HasVoxConn c g, GridConn g)=> VoxConn c g -> Vector (g, g) -> VoxConn c g
mixMaps db@VoxConn{..} conn = case (voxConnMap, voxConnList) of
  (Just m, Just l) -> let
    (new_m, new_l) = V.foldl' (commitConn voxConnRange) (m, l) conn
    in db { voxConnMap = return new_m, voxConnList = return new_l }
  _                -> db

-- | This function updates the data structure for commit the connection between two
-- positions.
commitConn ::(HasVoxConn c g, GridConn g)
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

mergeDataList :: VoxConn c g -> VoxConn c g -> Maybe (HashMap Int (Vector g))
mergeDataList d1 d2 = let
  func x1 x2 = return $ HM.union x1 x2
  in mergeMaybe func (voxConnList d1) (voxConnList d2)

-- | Merges two neighbor boxes ('VoxConn') by connecting all valid connections along the
-- wall formed by their interface.
mergeVoxConn :: (Cont ic a, GridConn g, HasVoxConn c g)=>
                (a -> a -> Bool) -> ic a -> VoxBoxRange ->
                VoxConn c g -> VoxConn c g -> VoxConn c g
mergeVoxConn isEqual as vbr d1 d2 =
  case mergeVoxBoxRange (voxConnRange d1) (voxConnRange d2) of
    Just (newBR, dir, wallbox) -> let
      wall         = scanWall dir wallbox
      uniGrainMap  = mergeDataMap  d1 d2
      uniGrainList = mergeDataList d1 d2
      wallGrid     = V.map toGridType wall
      withConn     = V.concatMap (tryConn isEqual as vbr dir) wallGrid
      newBD        = VoxConn newBR uniGrainMap uniGrainList
      in mixMaps newBD withConn
    _ -> error "[VoxConnFinder] I can't merge these two VoxBoxRange's"

-- | Extract a list of all 'VoxelPos' of the lower side of a plane defined by the
-- 'CartesianDir'. Used to scan the connections along the wall (boarder between two
-- divided spaces).
scanWall :: CartesianDir -> WallBoxRange -> Vector VoxelPos
scanWall dir (WallBoxRange br) = let
  (lx, ux, ly, uy, lz, uz) = getBoxLinRange br
  in V.fromList $ case dir of
    XDir -> [VoxelPos lx j k | j <- [ly..uy], k <- [lz..uz]]
    YDir -> [VoxelPos i ly k | i <- [lx..ux], k <- [lz..uz]]
    ZDir -> [VoxelPos i j lz | i <- [lx..ux], j <- [ly..uy]]
