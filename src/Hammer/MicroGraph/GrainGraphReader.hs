{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
  hasPropValue NullVertexProp = False
  hasPropValue _              = True
  
  getPropValue p = case p of
    VertexProp v -> return v
    _            -> Nothing
  
  setPropValue p v = case p of
    VertexProp _ -> VertexProp v
    _            -> VertexProp v

instance HasPropValue EdgeProp where
  hasPropValue (NullEdgeProp _) = False
  hasPropValue _                = True

  getPropValue p = case p of
    EdgeProp _ v -> return v
    _            -> Nothing
    
  setPropValue p v = case p of
    EdgeProp c _   -> EdgeProp c v
    NullEdgeProp c -> EdgeProp c v


instance HasPropValue FaceProp where
  hasPropValue (NullFaceProp _) = False
  hasPropValue _                = True
  
  getPropValue p = case p of
    FaceProp _ v -> return v
    _            -> Nothing
  
  setPropValue p v = case p of
    FaceProp c _   -> FaceProp c v
    NullFaceProp c -> FaceProp c v

instance HasPropValue GrainProp where
  hasPropValue (NullGrainProp _) = False
  hasPropValue _                 = True

  getPropValue p = case p of
    GrainProp _ v -> return v
    _             -> Nothing
  
  setPropValue p v = case p of
    GrainProp c _   -> GrainProp c v
    NullGrainProp c -> GrainProp c v
    
-- ------------------------------ HasPropConn Instancies ------------------------

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

