{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Hammer.MicroGraph.GrainGraphReader
  ( getVertexProp 
  , getEdgeProp 
  , getFaceProp 
  , getGrainProp 
  , getGrainIDList
  ) where

-- External modules
import qualified Data.HashMap.Strict as HM

import           Data.HashSet        (HashSet)

import           Hammer.MicroGraph.Types

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

-- ------------------------------ HasPropValue Instancies ------------------------

instance HasPropConn EdgeProp where
  type PropConn EdgeProp = MicroEdge
  getPropConn p = case p of
    EdgeProp m _ -> return m
    _            -> Nothing
    
instance HasPropConn FaceProp where
  type PropConn FaceProp = HashSet EdgeID
  getPropConn p = case p of
    FaceProp c _ -> return c
    _            -> Nothing
    
instance HasPropConn GrainProp where
  type PropConn GrainProp = HashSet FaceID
  getPropConn p = case p of
    GrainProp c _ -> return c
    _             -> Nothing

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

