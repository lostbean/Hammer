{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Hammer.Graph.RandomWalk
       ( applyNSOperator
       , mkTransitionMatrix
       , invertGraph
       , mkNSMatrix
       ) where

import qualified Data.List           as L
import qualified Data.HashMap.Strict as HM

import           Data.Hashable       (Hashable)

import Hammer.Graph.Graph

addGraph :: (Num b, Eq b)=> Graph Int b -> Graph Int b -> Graph Int b
addGraph g1 g2 = Graph gu
  where
    gu  = HM.unionWith foo (graph g1) (graph g2)
    foo = HM.unionWith (+)

multGraph :: (Num b, Eq b)=> Graph Int b -> Graph Int b -> Graph Int b
multGraph g1 g2 = let
  g2i = invertGraph g2
  func row = let
    foo col = mulHM row col
    in HM.filter (/= 0) $ HM.map foo (graph g2i)
  mulHM row col = let
    foo acc k v = maybe acc ((acc +) . (v *)) (HM.lookup k col)
    in HM.foldlWithKey' foo 0 row
  in Graph $ HM.filter (not . HM.null) $ HM.map func (graph g1)

invertGraph :: (Hashable a, Eq a)=> Graph a b -> Graph a b
invertGraph Graph{..} = let
  g = HM.foldlWithKey' func HM.empty graph
  foo k v = HM.singleton k v
  func acc k v = HM.unionWith (HM.union) acc (HM.map (foo k) v)
  in Graph g

-- =============================== On graph clustering ===================================

mkTransitionMatrix :: Graph Int Double -> Graph Int Double
mkTransitionMatrix Graph{..} = let
  func hm = HM.map (/s) hm
    where s = HM.foldl' (+) 0 hm
  in Graph $ HM.map func graph

mkNSMatrix :: Int -> Graph Int Double -> Graph Int Double
mkNSMatrix n g
  | n > 0     = L.foldl' addGraph (head sqrs) (tail sqrs)
  | otherwise = Graph $ HM.empty
  where
    tm   = mkTransitionMatrix g
    sqrs = L.scanl1 multGraph (replicate n tm)

applyNSOperator :: Int -> Graph Int Double -> Graph Int Double
applyNSOperator n g = let
  k = fromIntegral $ 2 * n
  Graph ns = mkNSMatrix n g
  getL1 u v = let
    row1 = maybe HM.empty id (HM.lookup u ns)
    row2 = maybe HM.empty id (HM.lookup v ns)
    ls   = HM.unionWith (-) row1 row2
    in HM.foldl' (\acc x -> acc + abs x) 0 ls
  func i = HM.mapWithKey (\j _ -> exp (k - getL1 i j) - 1)
  in Graph $ HM.mapWithKey func (graph g)

-- ===================================== MCL =============================================

addSelfNodes :: Graph Int Double -> Graph Int Double
addSelfNodes Graph{..} = Graph $ HM.unionWith (HM.union) graph graph2
  where
    es = map (\x -> ((x, x), 1.0)) ns
    ns = HM.keys graph
    Graph graph2 = mkBiGraph [] es


-- =================================== Testing ===========================================

test :: Graph Int Double
test = mkUniGraph [] ([((1, 2), 0.3), ((2,3), 0.8), ((3,1), 0.5)]::[((Int,Int), Double)])

testB :: Graph Int Double
testB = mkBiGraph [] ([((1, 2), 0.3), ((2,3), 0.8), ((3,1), 0.5)]::[((Int,Int), Double)])

testM = let
  a :: Graph Int Double
  a = mkBiGraph [] [ ((1,1), 2), ((1,2), 5), ((1,3), 1)
                   , ((2,1), 4), ((2,2), 3), ((2,3), 1)]

  b = mkBiGraph [] [ ((1,1), 1)
                   , ((2,2), 2)
                   , ((3,1), 2), ((3,2), 3), ((3,3), 1)]
  rm = mkBiGraph [] [ ((1,1),4.0),((1,2),13.0),((1,3),1.0)
                    , ((2,1),6.0),((2,2),9.0), ((2,3),1.0)]
  ra = mkBiGraph [] [ ((1,1), 3), ((1,2), 5), ((1,3), 1)
                    , ((2,1), 4), ((2,2), 5), ((2,3), 1)
                    , ((3,1), 2), ((3,2), 3), ((3,3), 1)]
  in rm == (multGraph a b) && ra == (addGraph a b)

testP :: Graph Int Double
testP = mkUniGraph [] [ ((1, 2), 1), ((2,3), 1), ((3,1), 1)
                      , ((4, 5), 1), ((5,6), 1), ((6,4), 1)
                      , ((3, 4), 1) ]

testFig4 :: Graph Int Double
testFig4 = mkUniGraph [] [ ((0,1), 1), ((1,2), 1), ((2,3), 1), ((3,0), 1), ((2,0), 1)

                         , ((4,5), 1), ((5,6), 1), ((6,7), 1), ((7,4), 1), ((6,4), 1), ((5,7), 1)

                         , ((17,18), 1), ((18,20), 1), ((20,19), 1), ((19,17), 1), ((17,20), 1)

                         , ((8,9), 1), ((9,10), 1), ((10,13), 1), ((13,16), 1), ((16,15), 1), ((15,14), 1)
                         , ((14,11), 1), ((11,8), 1), ((9,12), 1), ((13,12), 1), ((15,12), 1), ((11,12), 1)
                         , ((8,12), 1), ((16,12), 1)

                         -- inter connections
                         , ((1,4), 1), ((18,3), 1), ((18,8), 1), ((7,8), 1), ((6,10), 1.5)
                         ]

testFig5 :: Graph Int Double
testFig5 = mkUniGraph [] [ ((0,1), 1), ((0,2), 1), ((0,3), 1), ((0,4), 1), ((0,5), 1)
                         , ((6,7), 1), ((6,8), 1), ((6,9), 1), ((6,10), 1)
                         , ((11,12), 1), ((11,13), 1), ((11,14), 1), ((11,15), 1)
                         -- inter connections
                         , ((0,6), 1), ((6,11), 1)
                         ]
