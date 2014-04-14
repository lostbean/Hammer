{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Hammer.MicroGraph.Graph
       ( Graph (..)
       , mkGraph
       , dfs
       , connComp
       , getChilderen
       , hasEdge
       ) where


import qualified Data.HashMap.Strict         as HM
import qualified Data.HashSet                as HS
import qualified Data.Vector                 as V
import qualified Data.List                   as L

import           Data.HashMap.Strict      (HashMap)
import           Data.HashSet             (HashSet)
import           Data.Vector              (Vector)
import           Data.Hashable            (Hashable)


newtype Graph a = Graph { graph :: HashMap a (HashSet a) }

instance (Show a)=> Show (Graph a) where
  show (Graph{..}) = show graph

-- | Construct a graph from edges.
mkGraph :: (Eq a, Hashable a)=> [(a, a)] -> Graph a
mkGraph = Graph . builder
  where
    addE (k, !v)
      -- avoid self edge
      | k == v    = id
      | otherwise = HM.insertWith (HS.union) k (HS.singleton v)
    builder = L.foldl' (\m (!a, !b) -> addE (a,b) $ addE (b,a) m) HM.empty

-- | Search algorithm for transverse all connected nodes from a
-- given node. No ordered sequence is defined.
search :: (Eq a, Hashable a)=> Graph a -> a -> HashSet a
search g@Graph{..} a
  | HM.member a graph = go a (HS.empty) (HS.singleton a)
  | otherwise         = HS.empty
  where
    go n !queue !visited
      | HS.null nq = HS.insert n visited
      | otherwise  = go nx nq (HS.insert n visited)
      where
        nx = head $ HS.toList nq
        cs = getChilderen g n
        nq = HS.union (HS.delete n queue) (HS.difference cs visited)

-- | Deep First Search algorithm for transverse all connected nodes from a
-- given node.
dfs :: (Eq a, Hashable a)=> Graph a -> a -> HashSet a
dfs g@Graph{..} a
  | HM.member a graph = go [a] HS.empty
  | otherwise         = HS.empty
  where
    go !(n:ns) !visited = let
      cs = getChilderen g n
      nq = (HS.toList $ HS.difference cs visited) ++ ns
      in  go nq (HS.insert n visited)
    go _ !visited = visited

-- | Find all groups of connected components of a graph.
connComp :: (Eq a, Hashable a)=> Graph a -> [HashSet a]
connComp g@Graph{..} = go ns0
  where
    ns0 = HS.fromList (HM.keys graph)
    go !ns
      | HS.null ns = []
      | otherwise  = let
        x = head $ HS.toList ns
        c = dfs g x
        in c : go (HS.difference ns c)

-- | Check if there is an edge from the first to the second node.
hasEdge :: (Eq a, Hashable a)=> Graph a -> a -> a -> Bool
hasEdge Graph{..} a b = maybe False (HS.member b) (HM.lookup a graph)

-- | Retrieve all neighboring nodes of a given node.
getChilderen :: (Eq a, Hashable a)=> Graph a -> a -> HashSet a
getChilderen Graph{..} n = maybe (HS.empty) id (HM.lookup n graph)

test = mkGraph ([(1,2), (2,3), (5,2), (5,3), (60,1) , (7,4)]::[(Int,Int)])
