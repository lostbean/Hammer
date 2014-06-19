{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Hammer.Graph.Graph
       ( Graph (..)
       , mkUniGraph
       , mkBiGraph
       , dfs
       , connComp
       , getChilderen
       , hasEdge
       , graphToList
       , getSubGraph
       ) where


import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import qualified Data.List           as L

import           Data.HashMap.Strict (HashMap)
import           Data.HashSet        (HashSet)
import           Data.Hashable       (Hashable)


newtype Graph a b = Graph { graph :: HashMap a (HashMap a b) } deriving (Eq)

instance (Show a, Show b)=> Show (Graph a b) where
  show (Graph{..}) = show graph

-- | Extracts a sub graph which contains a given set of nodes.
getSubGraph ::(Eq a, Hashable a)=> Graph a b -> [a] -> Graph a b
getSubGraph Graph{..} ns = let
  set  = HS.fromList ns
  func = HM.filterWithKey (\k _ -> HS.member k set)
  in Graph $ HM.map func (func graph)

-- | Construct a unidirectional graph from edges.
mkUniGraph :: (Eq a, Eq b, Hashable a, Hashable b)=> [a] -> [((a, a), b)] -> Graph a b
mkUniGraph nodes = Graph . builder
  where
    g0 = HM.fromList $ map (flip (,) HM.empty) nodes
    addE (k, !v, !p) = HM.insertWith (HM.union) k (HM.singleton v p)
    builder = L.foldl' (\m ((!a, !b), k) -> addE (a,b,k) $ addE (b,a,k) m) g0

-- | Construct a bidirectional graph from edges.
mkBiGraph :: (Eq a, Eq b, Hashable a, Hashable b)=> [a] -> [((a, a), b)] -> Graph a b
mkBiGraph nodes = Graph . builder
  where
    g0 = HM.fromList $ map (flip (,) HM.empty) nodes
    addE (k, !v, !p) = HM.insertWith (HM.union) k (HM.singleton v p)
    builder = L.foldl' (\m ((!a, !b), k) -> addE (a,b,k) m) g0

-- | Search algorithm for transverse all connected nodes from a
-- given node. No ordered sequence is defined.
search :: (Eq a, Eq b, Hashable a, Hashable b)=> Graph a b -> a -> HashSet a
search g@Graph{..} a
  | HM.member a graph = go a (HS.empty) (HS.singleton a)
  | otherwise         = HS.empty
  where
    go n !queue !visited
      | HS.null nq = HS.insert n visited
      | otherwise  = go nx nq (HS.insert n visited)
      where
        nx = head $ HS.toList nq
        cs = HS.fromList $ HM.keys $ getChilderen g n
        nq = HS.union (HS.delete n queue) (HS.difference cs visited)

-- | Deep First Search algorithm for transverse all connected nodes from a
-- given node.
dfs :: (Eq a, Eq b, Hashable a, Hashable b)=> Graph a b -> a -> HashSet a
dfs g@Graph{..} a
  | HM.member a graph = go [a] HS.empty
  | otherwise         = HS.empty
  where
    go !(n:ns) !visited = let
      cs = HS.fromList $ HM.keys $ getChilderen g n
      nq = (HS.toList $ HS.difference cs visited) ++ ns
      in  go nq (HS.insert n visited)
    go _ !visited = visited

-- | Find all groups of connected components of a graph.
connComp :: (Eq a, Eq b, Hashable a, Hashable b)=> Graph a b -> [HashSet a]
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
hasEdge :: (Eq a, Hashable a, Eq b, Hashable b)=> Graph a b -> a -> a -> Bool
hasEdge Graph{..} a b = maybe False (HM.member b) (HM.lookup a graph)

-- | Retrieve all neighboring nodes of a given node.
getChilderen :: (Eq a, Hashable a, Eq b, Hashable b)=> Graph a b -> a -> HashMap a b
getChilderen Graph{..} n = maybe (HM.empty) id (HM.lookup n graph)

graphToList :: Graph a b -> [((a,a), b)]
graphToList = HM.foldlWithKey' func [] . graph
  where
    func acc i col = (map (\(j,v) -> ((i,j), v)) (HM.toList col)) ++ acc

getGraphEdge :: (Eq a, Hashable a, Eq b, Hashable b)=> Graph a b -> (a, a) -> Maybe b
getGraphEdge g (n1, n2) = HM.lookup n1 (graph g) >>= HM.lookup n2

-- =================================== Testing ===========================================

test :: Graph Int Double
test = mkUniGraph [20, 2] ([((1,2), 1), ((2,3), 1), ((5,2), 1), ((5,3), 1), ((60,1), 1) , ((7,4), 1)]::[((Int,Int), Double)])
