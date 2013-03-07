{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Hammer.MicroGraph.GrainGraphBuilder
  ( initMicroGraph

  , insertNewVertex    
  , insertNewEdge 
  --, insertNewFace 
  --, insertNewGrain

  , insertNewVertexAutoConn
  , insertNewEdgeAutoConn
  , insertNewFaceAutoConn
  , insertNewGrainAutoConn
    
  , findGraph
    
  , combinations
  , combitronics

  ) where

-- External modules
import qualified Data.IntMap         as IM
import qualified Data.List           as L
import qualified Data.Map            as M
import qualified Data.IntSet         as IS
import qualified Data.HashSet        as HS
import qualified Data.HashMap.Strict as HM

import Data.Hashable       (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.IntMap         (IntMap)
import Data.Map            (Map)
import Control.Monad       (liftM)
import Data.IntSet         (IntSet)

import Hammer.MicroGraph.Types

-- ====================================================================================

initMicroGraph :: MicroGraph g f e v
initMicroGraph = MicroGraph HM.empty HM.empty HM.empty HM.empty   

--  ------------------------- GrainHierarchy Instancies---------------------------------

combinations :: Int -> [a] -> [[a]]
combinations _ [] = []
combinations 0 _  = []
combinations 1 x  = map (:[]) x
combinations n (x:xs) = (map (x:) $ combinations (n-1) xs) ++ combinations n xs

combitronics :: IntSet -> Int -> ([Int] -> c) -> [c]
combitronics set n func = let
  foo acc k = let
    xs = IS.toList $ IS.delete k set
    comb = combinations n xs
  ---------- TODO for more than n + 1
    in map func comb ++ acc
  in IS.foldl' foo [] set


instance GrainHierarchy VertexID where
  type SubLevel     VertexID = EdgeID
  type SubLevelProp VertexID = EdgeProp
  
  generateSubLevelConn v = case unVertexID v of
    Left (a,b,c,d) -> let 
      e1 = mkEdgeID (a,b,c)
      e2 = mkEdgeID (a,b,d)
      e3 = mkEdgeID (a,c,d)
      e4 = mkEdgeID (b,c,d)
      in [e1, e2, e3, e4] 
    Right set -> let
      foo [a,b,c] = mkEdgeID (a,b,c)
      foo _       = error "[GrainGraph] It can't be happening. It's must be a bug!"
      in combitronics set 3 foo

  updateSubLevelProp v1 old = let
    ret m = return . EdgeProp m
    retN  = return . NullEdgeProp
    func mv = case mv of
      DummyEdge      -> HalfEdge v1
      HalfEdge v2    -> FullEdge v1 v2
      FullEdge v2 v3 -> BadEdge [v1, v2, v3]
      BadEdge  vs    -> BadEdge (v1:vs)
    in case old of
      Just (EdgeProp mv x)   -> ret (func mv) x
      Just (NullEdgeProp mv) -> retN (func mv)
      Nothing                -> retN (HalfEdge v1)
      
instance GrainHierarchy EdgeID where
  type SubLevel     EdgeID = FaceID
  type SubLevelProp EdgeID = FaceProp

  generateSubLevelConn e = case unEdgeID e of
    Left (a,b,c) -> let
      f1      = mkFaceID (a,b)
      f2      = mkFaceID (b,c)
      f3      = mkFaceID (a,c)
      in [f1, f2, f3]
    Right set -> let
      foo [a,b] = mkFaceID (a,b)
      foo _       = error "[GrainGraph] It can't be happening. It's must be a bug!"
      in combitronics set 2 foo

  updateSubLevelProp e old = let
    ret m = return . FaceProp m
    retN  = return . NullFaceProp
    in case old of
      -- OBS: It doesn't check for consistence the IntMap
      Just (FaceProp me x)   -> ret  (HS.insert e me) x
      Just (NullFaceProp me) -> retN (HS.insert e me)
      Nothing                -> retN (HS.singleton e)
      

instance GrainHierarchy FaceID where
  type SubLevel     FaceID = GrainID
  type SubLevelProp FaceID = GrainProp

  generateSubLevelConn f = let
    (a, b) = unFaceID f
    in [mkGrainID a, mkGrainID b]
  
  updateSubLevelProp f old = let
    ret m = return . GrainProp m
    retN  = return . NullGrainProp
    in case old of
       -- OBS: It doesn't check for consistence the IntMap
      Just (GrainProp mg x)   -> ret  (HS.insert f mg) x
      Just (NullGrainProp mg) -> retN (HS.insert f mg)
      Nothing                 -> retN (HS.singleton f)

-- ------------------------------- UpdateLevel Instances ---------------------------------

instance UpdateLevel GrainProp where
  updateLevelProp v func prop = case prop of
    Nothing -> return $ GrainProp HS.empty v
    Just x  -> return $ case x of
      GrainProp mv t   -> GrainProp mv (func v t)
      NullGrainProp mv -> GrainProp mv v
     
instance UpdateLevel EdgeProp where
  updateLevelProp v func prop = case prop of
    Nothing -> return $ EdgeProp DummyEdge v
    Just x  -> return $ case x of
      EdgeProp mg t   -> EdgeProp mg (func v t)
      NullEdgeProp mg -> EdgeProp mg v

instance UpdateLevel FaceProp where
  updateLevelProp v func prop = case prop of
    Nothing -> return $ FaceProp HS.empty v
    Just x  -> return $ case x of
      FaceProp mf t   -> FaceProp mf (func v t)
      NullFaceProp mf -> FaceProp mf v

instance UpdateLevel VertexProp where
  updateLevelProp v func prop = case prop of
    Nothing -> return $ VertexProp v
    Just x  -> return $ case x of
      VertexProp t   -> VertexProp (func v t)
      NullVertexProp -> VertexProp v

-- ============================ HashMap Operations ============================================
                               
insertUniqueHM :: (Eq k, Hashable k)=> a -> k -> (k -> k) -> HashMap k a -> (k, HashMap k a) 
insertUniqueHM v k func m
  | HM.member k m = insertUniqueHM v (func k) func m
  | otherwise     = (k, HM.insert k v m)

alterHM :: (Eq k, Hashable k)=> (Maybe a -> Maybe a) -> k -> HashMap k a -> HashMap k a 
alterHM f k m = case f $ HM.lookup k m of
  Just x -> HM.insert k x m
  _      -> HM.delete k m

-- ------------------------------- Fold functions ------------------------------------------

foldSubLevelHM :: (GrainHierarchy l)
               => (SubLevel l)
               -> [l]
               -> HashMap (SubLevel l) (SubLevelProp l a)
               -> HashMap (SubLevel l) (SubLevelProp l a)
foldSubLevelHM l ss hm = let
  func acc sl = alterHM (updateSubLevelProp sl) l acc
  in L.foldl' func hm ss
     
-- ------------------------------ Insert with connections -----------------------------------
 
insertSubLevelConn :: (GrainHierarchy l)
                   => l
                   -> [SubLevel l]
                   -> HashMap (SubLevel l) (SubLevelProp l a)
                   -> HashMap (SubLevel l) (SubLevelProp l a)
insertSubLevelConn l slcs hm =  let
  foo acu sb = foldSubLevelHM sb [l] acu
  in L.foldl' foo hm slcs

insertNewVertex :: (v -> v -> v) -> VertexID -> v -> [EdgeID]
                -> MicroGraph g f e v -> MicroGraph g f e v
insertNewVertex func vid prop es (mgp@MicroGraph{..}) =
  case updateLevelProp prop func Nothing of
    Just prop -> let
      (uvid, mv) = insertUniqueHM prop vid getAlterVertexID microVertex
      me         = insertSubLevelConn uvid es microEdges
      in mgp { microEdges = me, microVertex = mv }
    _         -> mgp
  
insertNewEdge :: (e -> e -> e) -> EdgeID -> e -> [FaceID]
              -> MicroGraph g f e v -> MicroGraph g f e v
insertNewEdge func eid prop fs (mgp@MicroGraph{..}) = let
  me = alterHM (updateLevelProp prop func) eid microEdges
  mf = insertSubLevelConn eid fs microFaces
  in mgp { microFaces = mf, microEdges = me } 
 
-- --------------------- Insert with auto deviration connections ----------------------------

insertSubLevelAutoConn :: (GrainHierarchy l)
                       => l
                       -> HashMap (SubLevel l) (SubLevelProp l a)
                       -> HashMap (SubLevel l) (SubLevelProp l a)
insertSubLevelAutoConn l = insertSubLevelConn l (generateSubLevelConn l)

insertNewVertexAutoConn :: (v -> v -> v) -> VertexID -> v
                        -> MicroGraph g f e v -> MicroGraph g f e v
insertNewVertexAutoConn func vid prop (mgp@MicroGraph{..}) =
  case updateLevelProp prop func Nothing of
    Just prop -> let
      (uvid, mv) = insertUniqueHM prop vid getAlterVertexID microVertex
      me         = insertSubLevelAutoConn uvid microEdges
      in mgp { microEdges = me, microVertex = mv }
    _         -> mgp 
  
insertNewEdgeAutoConn :: (e -> e -> e) -> EdgeID -> e
                      -> MicroGraph g f e v -> MicroGraph g f e v
insertNewEdgeAutoConn func eid prop (mgp@MicroGraph{..}) = let
  me = alterHM (updateLevelProp prop func) eid microEdges
  mf = insertSubLevelAutoConn eid microFaces 
  in mgp { microFaces = mf, microEdges = me } 
 
insertNewFaceAutoConn :: (f -> f -> f) -> FaceID -> f
                      -> MicroGraph g f e v -> MicroGraph g f e v
insertNewFaceAutoConn func fid prop (mgp@MicroGraph{..}) = let
  mf = alterHM (updateLevelProp prop func) fid microFaces
  mg = insertSubLevelAutoConn fid microGrains 
  in mgp { microGrains = mg, microFaces = mf } 
 
insertNewGrainAutoConn :: (g -> g -> g) -> GrainID -> g -> MicroGraph g f e v -> MicroGraph g f e v
insertNewGrainAutoConn func gid prop (mgp@MicroGraph{..}) = let
  mg = alterHM (updateLevelProp prop func) gid microGrains
  in mgp { microGrains = mg } 
 
-- ------------------------------ Find graph connection only -------------------------------

findGraph :: HashMap VertexID (VertexProp v) -> MicroGraph () () () v
findGraph v = let
  e = findSubLevelGraph v
  f = findSubLevelGraph e
  g = findSubLevelGraph f
  in MicroGraph g f e v

findSubLevelGraph :: (GrainHierarchy l)
                  => HashMap l a
                  -> HashMap (SubLevel l) (SubLevelProp l ())
findSubLevelGraph = HM.foldlWithKey' (\acc k _ -> insertSubLevelAutoConn k acc) HM.empty

