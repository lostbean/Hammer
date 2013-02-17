{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Hammer.MicroGraph.Types
  ( GrainID
  , FaceID
  , EdgeID
  , VertexID
    
  , mkGrainID
  , mkFaceID
  , mkEdgeID
  , mkVertexID

  , unGrainID
  , unFaceID
  , unEdgeID
  , unVertexID

  , MicroEdge      (..)
  , GrainProp      (..)
  , FaceProp       (..)
  , EdgeProp       (..)
  , VertexProp     (..)
  , MicroGraph     (..)
  , GrainHierarchy (..)
  , UpdateLevel    (..) 
  , HasPropValue   (..)
  , HasPropConn    (..)
  ) where

-- External modules
import qualified Data.List as L

import Data.Hashable       (Hashable, hashWithSalt)
import Data.HashMap.Strict (HashMap)
import Data.IntMap         (IntMap)

-- ==========================================================================================

newtype GrainID  = GrainID   Int                 deriving (Show, Eq, Ord)
newtype FaceID   = FaceID   (Int, Int)           deriving (Show)
newtype EdgeID   = EdgeID   (Int, Int, Int)      deriving (Show)
newtype VertexID = VertexID (Int, Int, Int, Int) deriving (Show)

mkGrainID :: Int -> GrainID 
mkGrainID = GrainID

mkFaceID :: (Int, Int)-> FaceID 
mkFaceID = FaceID . fast2DSort

mkEdgeID :: (Int, Int, Int)-> EdgeID 
mkEdgeID = EdgeID . fast3DSort

mkVertexID :: (Int, Int, Int, Int)-> VertexID 
mkVertexID = VertexID . fast4DSort


unGrainID :: GrainID -> Int
unGrainID (GrainID x) = x

unFaceID :: FaceID -> (Int, Int)
unFaceID (FaceID x) = x 

unEdgeID :: EdgeID -> (Int, Int, Int)
unEdgeID (EdgeID x) = x

unVertexID :: VertexID -> (Int, Int, Int, Int)
unVertexID (VertexID x) = x


instance Hashable GrainID where
  hashWithSalt i (GrainID x) = hashWithSalt i x

instance Hashable FaceID where
  hashWithSalt i (FaceID x) = hashWithSalt i (fast2DSort x)

instance Hashable EdgeID where
  hashWithSalt i (EdgeID x) = hashWithSalt i (fast3DSort x)
 
instance Hashable VertexID where
  hashWithSalt i (VertexID x) = hashWithSalt i (fast4DSort x)
  
instance Eq VertexID where
  x == y = compare x y == EQ
instance Ord VertexID where
  compare (VertexID a) (VertexID b) = compare a' b'
    where
    a' = fast4DSort a
    b' = fast4DSort b

instance Eq EdgeID where
  x == y = compare x y == EQ
instance Ord EdgeID where
  compare (EdgeID a) (EdgeID b) = compare a' b'
    where
    a' = fast3DSort a
    b' = fast3DSort b

instance Eq FaceID where
  x == y = compare x y == EQ
instance Ord FaceID where
  compare (FaceID a) (FaceID b) = compare a' b'
    where
    a' = fast2DSort a
    b' = fast2DSort b


{-# INLINE fast4DSort #-}
fast4DSort :: (Ord a)=> (a,a,a,a) -> (a,a,a,a)
fast4DSort v@(a, b, c, d)
  | (a >= b) && (b >= c) && (c >= d) = v
  | otherwise                        = (a', b', c', d')
  where
    minab = min a b
    maxab = max a b
    mincd = min c d
    maxcd = max c d
    a'    = max maxab maxcd
    b'    = max (max minab mincd) (min maxab maxcd)
    c'    = min (max minab mincd) (min maxab maxcd)
    d'    = min minab mincd

{-# INLINE fast3DSort #-}
fast3DSort :: (Ord a)=> (a,a,a) -> (a,a,a)
fast3DSort v@(a, b, c)
  | (a >= b) && (b >= c) = v
  | otherwise            = (a', b', c')
  where
    minab = min a b
    maxab = max a b
    a'    = max maxab c
    b'    = max (min maxab c) minab
    c'    = min minab c

{-# INLINE fast2DSort #-}
fast2DSort :: (Ord a)=> (a,a) -> (a,a)
fast2DSort v@(a, b)
  | a >= b    = v
  | otherwise = (b, a)

-- Test fast sort for tuples
testFast3 :: (Ord a)=> (a,a,a) -> Bool
testFast3 t@(x,y,z) = let
  unlist [a,b,c] = (a,b,c)
  unlist _       = error "[GrainsGraph] Oops! List size different than 3" 
  func = all ((== fast3DSort t) . fast3DSort . unlist)
  in func $ L.permutations [x,y,z]

testFast4 :: (Ord a)=> (a,a,a,a) -> Bool
testFast4 t@(x,y,z,w) = let
  unlist [a,b,c,d] = (a,b,c,d)
  unlist _       = error "[GrainsGraph] Oops! List size different than 3" 
  func = all ((== fast4DSort t) . fast4DSort . unlist) 
  in func $ L.permutations [x,y,z,w]

-- ===================================== Graph data types =========================================

-- | Define the concept of edge (triple line in 3D microstructure or grain boundary in 2D)
data MicroEdge
  = DummyEdge
  | BadEdge  [VertexID]
  | HalfEdge VertexID
  | FullEdge VertexID VertexID
  deriving (Show, Eq)

data VertexProp a = VertexProp                 a | NullVertexProp                 deriving (Show)
data EdgeProp   a = EdgeProp   MicroEdge       a | NullEdgeProp   MicroEdge       deriving (Show)
data FaceProp   a = FaceProp   (IntMap EdgeID) a | NullFaceProp   (IntMap EdgeID) deriving (Show)
data GrainProp  a = GrainProp  (IntMap FaceID) a | NullGrainProp  (IntMap FaceID) deriving (Show) -- replace IntMap FaceID por HashSet

-- | Define the microstructure graph with properties associated to
-- which one of the entities: 'GrainProp', 'FaceProp', 'EdgeProp' and 'VertexProp'.
data MicroGraph g f e v = MicroGraph
  { microGrains :: HashMap GrainID  (GrainProp  g)
  , microFaces  :: HashMap FaceID   (FaceProp   f)
  , microEdges  :: HashMap EdgeID   (EdgeProp   e)
  , microVertex :: HashMap VertexID (VertexProp v)
  } deriving (Show)

--  ---------------------------------- GrainHierarchy Class ---------------------------------------

-- | Class defining the bottom-up relation for a microstructure graph,
-- i.e from the Level Vertex to the SubLevel Edge. The basic idea here, is
-- that a level 'l' can decomposed in one or more 'SubLevel l'.
-- EdgeID (1,2,3) -> FaceID (1,2), FaceID(2,3) and FaceID(1,3)
class (Ord (SubLevel l), Hashable (SubLevel l))=> GrainHierarchy l where
  type SubLevel     l :: *
  type SubLevelProp l :: * -> *
  -- | The function 'foldSubLevel' folds the decomposition of a level 'l' with
  -- some insert function to a container 'a'
  foldSubLevel        :: (l -> Int -> SubLevel l -> a -> a) -> a -> l -> a
  -- | The function 'updateSubLevelProp' updates a 'SubLevelProp l a' based on
  -- by updating their reference to the level 'l'
  updateSubLevelProp  :: l -> Int -> SubLevel l -> Maybe (SubLevelProp l a) -> Maybe (SubLevelProp l a)

--  -------------------------------- UpdateLevel Class -------------------------------------------

class UpdateLevel l where
  updateLevelProp  :: a -> (a -> a -> a) -> Maybe (l a) -> Maybe (l a)

--  -------------------------------- HasPropValue Class -------------------------------------------

class HasPropValue prop where
  getPropValue       :: prop v -> Maybe v

--  -------------------------------- HasPropConn Class -------------------------------------------

class (HasPropValue prop)=> HasPropConn prop where
  type PropConn prop :: *
  getPropConn        :: prop v -> Maybe (PropConn prop)
  
  getPropBoth        :: prop v -> Maybe (v, PropConn prop)
  getPropBoth p = do
    v <- getPropValue p
    c <- getPropConn  p
    return (v, c)

-- ==========================================================================================