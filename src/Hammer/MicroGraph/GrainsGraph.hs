{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Hammer.MicroGraph.GrainsGraph
  ( initMicroGraph
  , insertNewVertex    
  , insertNewEdge 
  , insertNewFace 
  , insertNewGrain
    
  , findGraph
    
  , getVertexProp 
  , getEdgeProp 
  , getFaceProp 
  , getGrainProp 
  , getGrainIDList
    
  , closeSeq
  , CloseSeq (..)

  , module Hammer.MicroGraph.Types

  ) where

-- External modules
import qualified Data.IntMap         as IM
import qualified Data.Map            as M
import qualified Data.HashMap.Strict as HM

import Data.Hashable       (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.IntMap         (IntMap)
import Data.Map            (Map)
import Control.Monad       (liftM)
import Data.Maybe

import Hammer.MicroGraph.Types

-- ====================================================================================

initMicroGraph :: MicroGraph g f e v
initMicroGraph = MicroGraph HM.empty HM.empty HM.empty HM.empty   

--  ------------------------- GrainHierarchy Instancies---------------------------------

instance GrainHierarchy VertexID where
  type SubLevel     VertexID = EdgeID
  type SubLevelProp VertexID = EdgeProp

  foldSubLevel func acc v = let
    (a,b,c,d) = unVertexID v
    e1        = mkEdgeID (a,b,c)
    e2        = mkEdgeID (a,b,d)
    e3        = mkEdgeID (a,c,d)
    e4        = mkEdgeID (b,c,d)
    foo       = func v
    in foo d e1 . foo c e2 . foo b e3 . foo a e4 $ acc

  updateSubLevelProp v1 _ _ old = let
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

  foldSubLevel func acc e = let
    (a,b,c) = unEdgeID e
    f1      = mkFaceID (a,b)
    f2      = mkFaceID (b,c)
    f3      = mkFaceID (a,c)
    foo     = func e 
    in foo c f1 . foo a f2 . foo b f3 $ acc

  updateSubLevelProp e k _ old = let
    ret m = return . FaceProp m
    retN  = return . NullFaceProp
    in case old of
      -- OBS: It doesn't check for consistence the IntMap
      Just (FaceProp me x)   -> ret  (IM.insert k e me) x
      Just (NullFaceProp me) -> retN (IM.insert k e me)
      Nothing                -> retN (IM.singleton k e)
      

instance GrainHierarchy FaceID where
  type SubLevel     FaceID = GrainID
  type SubLevelProp FaceID = GrainProp

  foldSubLevel func acc f = let
    (a,b) = unFaceID f
    foo   = func f
    in foo a (mkGrainID b) . foo b (mkGrainID a) $ acc

  updateSubLevelProp f k _ old = let
    ret m = return . GrainProp m
    retN  = return . NullGrainProp
    in case old of
       -- OBS: It doesn't check for consistence the IntMap
      Just (GrainProp mg x)   -> ret  (IM.insert k f mg) x
      Just (NullGrainProp mg) -> retN (IM.insert k f mg)
      Nothing                 -> retN (IM.singleton k f)

-- ------------------------------- UpdateLevel Instancies ---------------------------------------------

instance UpdateLevel GrainProp where
  updateLevelProp v func prop = case prop of
    Nothing -> return $ GrainProp IM.empty v
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
    Nothing -> return $ FaceProp IM.empty v
    Just x  -> return $ case x of
      FaceProp mf t   -> FaceProp mf (func v t)
      NullFaceProp mf -> FaceProp mf v

instance UpdateLevel VertexProp where
  updateLevelProp v func prop = case prop of
    Nothing -> return $ VertexProp v
    Just x  -> return $ case x of
      VertexProp t   -> VertexProp (func v t)
      NullVertexProp -> VertexProp v

-- ------------------------------ HasPropValue Instancies -------------------------------

instance HasPropValue VertexProp where
  getPropValue p = case p of
    VertexProp v -> return v
    _            -> Nothing

instance HasPropValue EdgeProp where
  getPropValue p = case p of
    EdgeProp _ v -> return v
    _            -> Nothing

instance HasPropValue FaceProp where
  getPropValue p = case p of
    FaceProp _ v -> return v
    _            -> Nothing

instance HasPropValue GrainProp where
  getPropValue p = case p of
    GrainProp _ v -> return v
    _             -> Nothing

-- ------------------------------ HasPropValue Instancies -------------------------------

instance HasPropConn EdgeProp where
  type PropConn EdgeProp = MicroEdge
  getPropConn p = case p of
    EdgeProp m _ -> return m
    _            -> Nothing
    
instance HasPropConn FaceProp where
  type PropConn FaceProp = IntMap EdgeID
  getPropConn p = case p of
    FaceProp c _ -> return c
    _            -> Nothing
    
instance HasPropConn GrainProp where
  type PropConn GrainProp = IntMap FaceID
  getPropConn p = case p of
    GrainProp c _ -> return c
    _             -> Nothing

-- ------------------------------- Fold functions ---------------------------------------------

alterHM :: (Eq k, Hashable k)=> (Maybe a -> Maybe a) -> k -> HashMap k a -> HashMap k a 
alterHM f k m = case f $ HM.lookup k m of
  Just x -> HM.insert k x m
  _      -> HM.delete k m

type FoldFunc l a = (l -> Int -> SubLevel l -> Maybe a -> Maybe a)

foldSubLevelOnHashMap :: (GrainHierarchy l)=> FoldFunc l a -> HashMap (SubLevel l) a -> l -> HashMap (SubLevel l) a
foldSubLevelOnHashMap ins = foldSubLevel (\l i sl -> alterHM (ins l i sl) sl)
  
foldSubLevelOnMap :: (GrainHierarchy l)=> FoldFunc l a -> Map (SubLevel l) a -> l -> Map (SubLevel l) a
foldSubLevelOnMap ins acc v = foldSubLevel (\l i sl -> M.alter (ins l i sl) sl) acc v

foldSubLevelOnList :: (GrainHierarchy l)=> [(SubLevel l)] -> l -> [(SubLevel l)]
foldSubLevelOnList acc v = foldSubLevel (\_ _ subl ss -> subl:ss) acc v

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
findSubLevelGraph = let
  func acc k _ = foldSubLevelOnHashMap updateSubLevelProp acc k
  in HM.foldlWithKey' func HM.empty

-- ------------------------------ Modifie MicroGraph -------------------------------

insertSubLevelConn :: (GrainHierarchy l)
                      => HashMap (SubLevel l) (SubLevelProp l a)
                      -> l
                      -> HashMap (SubLevel l) (SubLevelProp l a)
insertSubLevelConn = foldSubLevelOnHashMap updateSubLevelProp

insertNewVertex :: (v -> v -> v) -> VertexID -> v -> MicroGraph g f e v -> MicroGraph g f e v
insertNewVertex func vid prop (mgp@MicroGraph{..}) = let
  mv = alterHM (updateLevelProp prop func) vid microVertex
  me = insertSubLevelConn microEdges vid 
  in mgp { microEdges = me, microVertex = mv } 
  
insertNewEdge :: (e -> e -> e) -> EdgeID -> e -> MicroGraph g f e v -> MicroGraph g f e v
insertNewEdge func eid prop (mgp@MicroGraph{..}) = let
  me = alterHM (updateLevelProp prop func) eid microEdges
  mf = insertSubLevelConn microFaces eid 
  in mgp { microFaces = mf, microEdges = me } 
 
insertNewFace :: (f -> f -> f) -> FaceID -> f -> MicroGraph g f e v -> MicroGraph g f e v
insertNewFace func fid prop (mgp@MicroGraph{..}) = let
  mf = alterHM (updateLevelProp prop func) fid microFaces
  mg = insertSubLevelConn microGrains fid 
  in mgp { microGrains = mg, microFaces = mf } 
 
insertNewGrain :: (g -> g -> g) -> GrainID -> g -> MicroGraph g f e v -> MicroGraph g f e v
insertNewGrain func gid prop (mgp@MicroGraph{..}) = let
  mg = alterHM (updateLevelProp prop func) gid microGrains
  in mgp { microGrains = mg } 
 
-- ------------------------------ Access functions -------------------------------

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

-- ================================== Close Seqeunce ================================

class CloseSeq a where
  type SeqUnit a
  seqHead :: (Eq (SeqUnit a))=> a -> SeqUnit a
  seqTail :: (Eq (SeqUnit a))=> a -> SeqUnit a
  seqInv  :: a -> a

instance CloseSeq (a, a) where
  type SeqUnit (a, a) = a
  seqHead = fst
  seqTail = snd
  seqInv (a,b) = (b,a) 

closeSeq :: (CloseSeq a, Eq (SeqUnit a))=>[a] -> Maybe [a]
closeSeq []        = Nothing
closeSeq [_]       = Nothing
closeSeq (seed:xs) = liftM (seed:) (getSeq (seqTail seed) xs) 
  where
    --nextRef :: (CloseSeq a, Eq (SeqUnit a))=> SeqUnit a -> a -> Maybe (SeqUnit a)
    nextRef x t
      | x == a && x /= b = Just (b, t)
      | x /= a && x == b = Just (a, seqInv t)
      | otherwise        = Nothing
      where
        a = seqHead t
        b = seqTail t

    --isIn :: (CloseSeq a, Eq (SeqUnit a))=> SeqUnit a -> a -> Bool
    isIn x    = isJust . nextRef x
    --isClosure :: (CloseSeq a, Eq (SeqUnit a))=> a -> Bool
    isClosure = isIn (seqHead seed)

    --getSeq :: (CloseSeq a, Eq (SeqUnit a))=> SeqUnit a -> [a] -> Maybe ([a])
    getSeq _ [] = Nothing
    getSeq ref rs = case getNext ref rs of
      Just (newRef, x, list)
        | null list -> if isClosure x
                       then Just [x]
                       else Nothing
        | otherwise -> do
          prox   <- getSeq newRef list
          return (x:prox)
      _             -> Nothing
      
    --getNext :: (CloseSeq a, Eq (SeqUnit a))=> SeqUnit a -> [a] -> Maybe (a, [a])
    getNext ref ss = case break (isIn ref) ss of
      ([], [])   -> Nothing
      (_ , [])   -> Nothing
      (as, b:bs) -> do
        (next, rest) <- nextRef ref b
        return (next, rest, as ++ bs)


