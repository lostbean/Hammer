{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hammer.MicroGraph.Base
       ( initMicroGraph
         -- * Insert new elements
       , insertNewVertex
       , insertNewEdge
       , insertNewFace
       , insertNewGrain
         -- * Insert connections only
       , insertVertexConn
       , insertEdgeConn
       , insertFaceConn
         -- * Insert property only
       , insertVertexProp
       , insertEdgeProp
       , insertFaceProp
       , insertGrainProp
         -- * Retrieve properties
       , getVertexProp
       , getEdgeProp
       , getFaceProp
       , getGrainProp
       , getGrainIDList
       ) where

import qualified Data.List           as L
import qualified Data.HashSet        as HS
import qualified Data.HashMap.Strict as HM

import           Data.HashSet        (HashSet)
import           Data.HashMap.Strict (HashMap)

import           Hammer.MicroGraph.Types

-- =======================================================================================

initMicroGraph :: MicroGraph g f e v
initMicroGraph = MicroGraph HM.empty HM.empty HM.empty HM.empty

-- ================================== Fold functions =====================================

foldSubLevelHM :: (GrainHierarchy l)
               => (SubLevel l)
               -> [l]
               -> HashMap (SubLevel l) (SubLevelProp l a)
               -> HashMap (SubLevel l) (SubLevelProp l a)
foldSubLevelHM l ss hm = let
  func acc sl = alterHM (updateSubLevelProp sl) l acc
  in L.foldl' func hm ss

-- ============================= Insert with connections =================================

insertSubLevelConn :: (GrainHierarchy l)
                   => l
                   -> [SubLevel l]
                   -> HashMap (SubLevel l) (SubLevelProp l a)
                   -> HashMap (SubLevel l) (SubLevelProp l a)
insertSubLevelConn l slcs hm =  let
  foo acu sb = foldSubLevelHM sb [l] acu
  in L.foldl' foo hm slcs

insertVertexConn :: VertexID -> [EdgeID] -> MicroGraph g f e v -> MicroGraph g f e v
insertVertexConn vid es (mgp@MicroGraph{..}) = let
  me = insertSubLevelConn vid es microEdges
  in mgp { microEdges = me }

insertEdgeConn :: EdgeID -> [FaceID] -> MicroGraph g f e v -> MicroGraph g f e v
insertEdgeConn eid fs (mgp@MicroGraph{..}) = let
  mf = insertSubLevelConn eid fs microFaces
  in mgp { microFaces = mf }

insertFaceConn :: FaceID -> MicroGraph g f e v -> MicroGraph g f e v
insertFaceConn fid (mgp@MicroGraph{..}) = let
  mg = insertSubLevelAutoConn fid generateSubLevelConn microGrains
  generateSubLevelConn f = let
    (a, b) = unFaceID f
    in [mkGrainID a, mkGrainID b]
  in mgp { microGrains = mg }

-- ================================== Insert properties ==================================

insertVertexProp :: (v -> v -> v) -> VertexID -> v
                 -> MicroGraph g f e v -> MicroGraph g f e v
insertVertexProp func vid v (mgp@MicroGraph{..}) = let
  mv = alterHM (return . insertPropValue func v) vid microVertex
  in mgp { microVertex = mv }

insertEdgeProp :: (e -> e -> e) -> EdgeID -> e
               -> MicroGraph g f e v -> MicroGraph g f e v
insertEdgeProp func eid v (mgp@MicroGraph{..}) = let
  me = alterHM (return . insertPropValue func v) eid microEdges
  in mgp { microEdges = me }

insertFaceProp :: (f -> f -> f) -> FaceID -> f
               -> MicroGraph g f e v -> MicroGraph g f e v
insertFaceProp func fid v (mgp@MicroGraph{..}) = let
  mf = alterHM (return . insertPropValue func v) fid microFaces
  in mgp { microFaces = mf }

insertGrainProp :: (g -> g -> g) -> GrainID -> g
                -> MicroGraph g f e v -> MicroGraph g f e v
insertGrainProp func gid v (mgp@MicroGraph{..}) = let
  mg = alterHM (return . insertPropValue func v) gid microGrains
  in mgp { microGrains = mg }

-- ======================== Insert unique new elements and properties ====================

insertNewVertex :: VertexID -> v -> [EdgeID] -> MicroGraph g f e v -> MicroGraph g f e v
insertNewVertex vid vp es (mgp@MicroGraph{..}) = let
  newProp    = newPropValue vp
  (uvid, mv) = insertUniqueHM newProp vid getAlterVertexID microVertex
  me         = insertSubLevelConn uvid es microEdges
  in mgp { microEdges = me, microVertex = mv }

insertNewEdge :: EdgeID -> e -> [FaceID] -> MicroGraph g f e v -> MicroGraph g f e v
insertNewEdge eid ep fs (mgp@MicroGraph{..}) = let
  newProp    = newPropValue ep
  (ueid, me) = insertUniqueHM newProp eid getAlterEdgeID microEdges
  mf         = insertSubLevelConn ueid fs microFaces
  in mgp { microFaces = mf, microEdges = me }

insertNewFace :: FaceID -> f -> MicroGraph g f e v -> MicroGraph g f e v
insertNewFace fid fp (mgp@MicroGraph{..}) = let
  newProp    = newPropValue fp
  (ufid, mf) = insertUniqueHM newProp fid getAlterFaceID microFaces
  mg         = insertSubLevelAutoConn ufid generateSubLevelConn microGrains
  generateSubLevelConn f = let
    (a, b) = unFaceID f
    in [mkGrainID a, mkGrainID b]
  in mgp { microGrains = mg, microFaces = mf }

insertNewGrain :: GrainID -> g -> MicroGraph g f e v -> MicroGraph g f e v
insertNewGrain gid v (mgp@MicroGraph{..}) = let
  mg = alterHM (const (return $ newPropValue v)) gid microGrains
  in mgp { microGrains = mg }

insertSubLevelAutoConn :: (GrainHierarchy l)=> l -> (l -> [SubLevel l])
                       -> HashMap (SubLevel l) (SubLevelProp l a)
                       -> HashMap (SubLevel l) (SubLevelProp l a)
insertSubLevelAutoConn l foo = insertSubLevelConn l (foo l)

-- =================================== Access functions ==================================

getVertexProp :: VertexID -> MicroGraph g f e v -> Maybe (VertexProp v)
getVertexProp vid MicroGraph{..} = HM.lookup vid microVertex

getEdgeProp :: EdgeID -> MicroGraph g f e v -> Maybe (EdgeProp e)
getEdgeProp eid MicroGraph{..} = HM.lookup eid microEdges

getFaceProp :: FaceID -> MicroGraph g f e v -> Maybe (FaceProp f)
getFaceProp fid MicroGraph{..} = HM.lookup fid microFaces

getGrainProp :: GrainID -> MicroGraph g f e v -> Maybe (GrainProp g)
getGrainProp gid MicroGraph{..} = HM.lookup gid microGrains


getGrainIDList :: MicroGraph g f e v -> [GrainID]
getGrainIDList MicroGraph{..} = HM.keys microGrains

-- =================================== GrainHierarchy Instancies =========================

instance GrainHierarchy VertexID where
  type SubLevel     VertexID = EdgeID
  type SubLevelProp VertexID = EdgeProp
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
  updateSubLevelProp f old = let
    ret m = return . GrainProp m
    retN  = return . NullGrainProp
    in case old of
       -- OBS: It doesn't check for consistence the IntMap
      Just (GrainProp mg x)   -> ret  (HS.insert f mg) x
      Just (NullGrainProp mg) -> retN (HS.insert f mg)
      Nothing                 -> retN (HS.singleton f)

-- =================================== HasPropValue Instancies ===========================

instance HasPropValue VertexProp where
  hasPropValue NullVertexProp = False
  hasPropValue _              = True

  newPropValue = VertexProp

  getPropValue p = case p of
    VertexProp v -> return v
    _            -> Nothing

  setPropValue p v = case p of
    VertexProp _ -> VertexProp v
    _            -> VertexProp v

  unsetPropValue = const NullVertexProp

  adjustPropValue func v prop = case prop of
    VertexProp t   -> VertexProp (func t v)
    NullVertexProp -> VertexProp v

instance HasPropValue EdgeProp where
  hasPropValue (NullEdgeProp _) = False
  hasPropValue _                = True

  newPropValue = EdgeProp DummyEdge

  getPropValue p = case p of
    EdgeProp _ v -> return v
    _            -> Nothing

  setPropValue p v = case p of
    EdgeProp c _   -> EdgeProp c v
    NullEdgeProp c -> EdgeProp c v

  unsetPropValue p = case p of
    EdgeProp c _   -> NullEdgeProp c
    NullEdgeProp c -> NullEdgeProp c

  adjustPropValue func v prop = case prop of
    EdgeProp mg t   -> EdgeProp mg (func t v)
    NullEdgeProp mg -> EdgeProp mg v

instance HasPropValue FaceProp where
  hasPropValue (NullFaceProp _) = False
  hasPropValue _                = True

  newPropValue = FaceProp HS.empty

  getPropValue p = case p of
    FaceProp _ v -> return v
    _            -> Nothing

  setPropValue p v = case p of
    FaceProp c _   -> FaceProp c v
    NullFaceProp c -> FaceProp c v

  unsetPropValue p = case p of
    FaceProp c _   -> NullFaceProp c
    NullFaceProp c -> NullFaceProp c

  adjustPropValue func v prop = case prop of
    FaceProp mf t   -> FaceProp mf (func t v)
    NullFaceProp mf -> FaceProp mf v

instance HasPropValue GrainProp where
  hasPropValue (NullGrainProp _) = False
  hasPropValue _                 = True

  newPropValue = GrainProp HS.empty

  getPropValue p = case p of
    GrainProp _ v -> return v
    _             -> Nothing

  setPropValue p v = case p of
    GrainProp c _   -> GrainProp c v
    NullGrainProp c -> GrainProp c v

  unsetPropValue p = case p of
    GrainProp c _   -> NullGrainProp c
    NullGrainProp c -> NullGrainProp c

  adjustPropValue func v prop = case prop of
    GrainProp mv t   -> GrainProp mv (func t v)
    NullGrainProp mv -> GrainProp mv v

-- =================================== HasPropConn Instancies ============================

instance HasPropConn EdgeProp where
  type PropConn EdgeProp = MicroEdge
  getPropConn p = case p of
    EdgeProp m _   -> return m
    NullEdgeProp m -> return m

instance HasPropConn FaceProp where
  type PropConn FaceProp = HashSet EdgeID
  getPropConn p = case p of
    FaceProp c _   -> return c
    NullFaceProp c -> return c

instance HasPropConn GrainProp where
  type PropConn GrainProp = HashSet FaceID
  getPropConn p = case p of
    GrainProp c _   -> return c
    NullGrainProp c -> return c
