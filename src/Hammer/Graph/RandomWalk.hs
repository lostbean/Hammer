{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module Hammer.Graph.RandomWalk
       ( runMCL
       ) where

import qualified Data.List           as L
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Generic as G

import           Control.DeepSeq

import           Hammer.Graph.Graph
import           Hammer.Graph.Sparse

import           Debug.Trace

-- =============================== On graph clustering ===================================

mkNSMatrix :: Int -> Graph Int Double -> Graph Int Double
mkNSMatrix n g
  | n > 0     = L.foldl' addGraph (head sqrs) (tail sqrs)
  | otherwise = Graph HM.empty
  where
    tm   = normalizeGraph g
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

normalizeGraph :: Graph Int Double -> Graph Int Double
normalizeGraph Graph{..} = let
  func hm = HM.map (/s) hm
    where s = HM.foldl' (+) 0 hm
  in Graph $ HM.map func graph

addSelfNodes :: Graph Int Double -> Graph Int Double
addSelfNodes Graph{..} = Graph $ HM.unionWith (HM.union) graph graph2
  where
    es = map (\x -> ((x, x), 1.0)) ns
    ns = HM.keys graph
    Graph graph2 = mkDiGraph [] es

getMClClusters :: CRS Double -> [[Int]]
getMClClusters = HM.elems . HM.fromListWith (++) . G.ifoldl' func [] . crs
  where
    func acc k x
      | G.null v  = acc
      | otherwise = (xmin, [k]) : acc
      where
        v    = vecS x
        xmin = G.minimum (G.map fst v)

runMCL :: Double -> Graph Int Double -> [[Int]]
runMCL r = getMClClusters . run 0 . graphToCRS . normalizeGraph . addSelfNodes
  where
    run :: Int -> CRS Double -> CRS Double
    run !n !x
      | n   > 200   = m
      | err < 0.001 = m
      | otherwise   = trace (show (n, chaos)) $ run (n+1) m
      where
        m     = x `deepseq` stepMCL r x
        err   = abs (chaos - 1)
        chaos = getChaos m

stepMCL :: Double -> CRS Double -> CRS Double
stepMCL r = pruneMCL . inflateMCL r . expandMCL

expandMCL :: CRS Double -> CRS Double
expandMCL x = multMMsmrt x x

inflateMCL :: Double -> CRS Double -> CRS Double
inflateMCL r CRS{..} = CRS $ G.map func crs
  where
    func vec = let
      col  = vecS vec
      sumC = G.foldl' (\acc (_, x) -> acc + x**r) 0 col
      infl = G.map    (\(j, x) -> (j, (x**r) / sumC)) col
      in unsafeMkVecS infl

pruneMCL :: CRS Double -> CRS Double
pruneMCL = CRS . G.map func . crs
  where func = unsafeMkVecS . G.filter ((> 0.001) . snd) . vecS

getChaos :: CRS Double -> Double
getChaos = G.foldl' func 0 . crs
  where
    func acc vec
      | sumC == 0 = acc
      | otherwise = max acc (maxC / sumC)
      where
        col  = vecS vec
        maxC = G.foldl' (\acu (_,x) -> max acu x) 0 col
        sumC = G.foldl' (\acu (_,x) -> acu + x*x) 0 col

pruneFix :: Double -> VecS Double -> VecS Double
pruneFix t = unsafeMkVecS . G.filter ((> t) . snd) . vecS

-- | Use a = 0.01
pruneDyn :: Double -> VecS Double -> VecS Double
pruneDyn a vec = trace (show (sumx, avgx, maxx, t)) $ unsafeMkVecS $ G.filter ((> t) . snd) v
  where
    v = vecS vec
    t = a * avgx * (1 - (maxx - avgx))
    (sumx, maxx) = G.foldl' (\(!acc, !m) (_, x) -> (acc + x, max x m)) (0, 0) v
    avgx = sumx / (fromIntegral $ G.length v)

pruneDyn2 :: Double -> VecS Double -> VecS Double
pruneDyn2 a vec = unsafeMkVecS $ G.filter ((> cf) . snd) v
  where
    v = vecS vec
    (sumx, maxx) = G.foldl' (\(!acc, !m) (_, x) -> (acc + x, max x m)) (0, 0) v
    avgx = sumx / (fromIntegral $ G.length v)
    --c0 = a * avgx * (1 - (maxx - avgx))
    cf = go avgx

    getAvgCut c = sumcut / (fromIntegral $ ncut)
      where
        (sumcut, ncut) = G.foldl' func (0, 0 :: Int) v
        func acc@(!s, !n) (_, x)
          | x <= c    = (s+x, n+1)
          | otherwise = acc

    go !c
      | avgcut > 0.005 = go avgcut
      | otherwise     = c
      where avgcut = getAvgCut c

-- =================================== Testing ===========================================

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
