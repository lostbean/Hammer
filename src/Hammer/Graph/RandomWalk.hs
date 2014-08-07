{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module Hammer.Graph.RandomWalk
       ( MCLCfg  (..)
       , Pruning (..)
       , defaultMCL
       , runMCL
       ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Generic as G

import           Control.DeepSeq

import           Hammer.Graph.Graph
import           Hammer.Graph.Sparse

import           Debug.Trace

-- ===================================== MCL =============================================

data Pruning
  = FixPrune Double
  | VarPrune Double
  | DynPrune Double
  | NoPrune
  deriving (Show, Eq)

data MCLCfg
  = MCLCfg
  { inflation :: Double
  , pruneMode :: Pruning
  , maxIter   :: Int
  , minError  :: Double
  , selfLoop  :: Double
  } deriving (Show)

-- | Default MCL configuration
defaultMCL :: MCLCfg
defaultMCL = MCLCfg
  { inflation = 1.2
  , pruneMode = FixPrune 0.0001
  , maxIter   = 200
  , minError  = 0.0001
  , selfLoop  = 0 }

-- | Graph clustering using the MCL algorithm.
runMCL :: MCLCfg -> Graph Int Double -> [[Int]]
runMCL c@MCLCfg{..} = getMClClusters . run maxIter .normalizeCRS .
                      graphToCRS . addSelfNodes selfLoop
  where
    run :: Int -> CRS Double -> CRS Double
    run !n !x
      | n   < 0          = m
      | chaos < minError = m
      | otherwise        = trace (show (n, chaos)) $ run (n-1) m
      where
        m     = x `deepseq` stepMCL c x
        chaos = getChaos m

-- | Add self loop for values larger than zero.
addSelfNodes :: Double -> Graph Int Double -> Graph Int Double
addSelfNodes loop g@Graph{..}
  | loop > 0  = Graph $ HM.unionWith (HM.union) graph graph2
  | otherwise = g
  where
    es = map (\x -> ((x, x), loop)) ns
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

stepMCL :: MCLCfg -> CRS Double -> CRS Double
stepMCL MCLCfg{..} = inflateCRS pruneMode inflation . expandCRS

expandCRS :: CRS Double -> CRS Double
expandCRS x = multMMsmrt x x

-- | Inflate a given matrix means power its elements, prune the smaller ones using some
-- pruning strategy and re-normalize.
inflateCRS :: Pruning -> Double -> CRS Double -> CRS Double
inflateCRS p r = CRS . G.map func . crs
  where func = normalizeVecS . pruneVecS p . powerVecS r

-- | Element-wise power of a 'VecS'
powerVecS:: Double -> VecS Double -> VecS Double
powerVecS r = unsafeMkVecS . G.map (\(j, x) -> (j, x ** r)) . vecS

normalizeCRS :: CRS Double -> CRS Double
normalizeCRS = CRS . G.map normalizeVecS . crs

-- | Normalize the vector where the sum of all its elements is 1.
normalizeVecS :: VecS Double -> VecS Double
normalizeVecS vec
  | sumC == 0 = vec
  | otherwise = unsafeMkVecS norm
  where
    col  = vecS vec
    sumC = G.foldl' (\acc (_, x) -> acc + x) 0 col
    norm = G.map    (\(j, x) -> (j, x / sumC)) col

-- | Calculate how far a given matrix is from "doubly idempotent"
getChaos :: CRS Double -> Double
getChaos = G.foldl' func 0 . crs
  where
    func acc vec
      | sumC == 0 = acc
      | otherwise = max acc (abs $ maxC - sumC)
      where
        col  = vecS vec
        maxC = G.foldl' (\acu (_,x) -> max acu x) 0 col
        sumC = G.foldl' (\acu (_,x) -> acu + x*x) 0 col

-- ==================================== Pruning ==========================================

-- | Prunes an vector using a given strategy.
pruneVecS :: Pruning -> (VecS Double -> VecS Double)
pruneVecS p = case p of
  FixPrune t -> fixPrune t
  VarPrune t -> varPrune t
  DynPrune t -> dynPrune t
  _          -> id

-- | Prunes all elements smaller that a fix values
fixPrune :: Double -> VecS Double -> VecS Double
fixPrune t = unsafeMkVecS . G.filter ((> t) . snd) . vecS

-- | Prunes all elements smaller that a threshold calculated based on statistical
-- distribution of the input vector.
varPrune :: Double -> VecS Double -> VecS Double
varPrune a vec = unsafeMkVecS $ G.filter ((> t) . snd) (vecS vec)
  where
    (_, avgx, maxx) = getPruningStat vec
    t = a * avgx * (1 - (maxx - avgx))

-- | Prunes all small elements with fixed total amount of pruning (e.g. 0.05 means pruning
-- small elements with maximum total pruning of 5%)
dynPrune :: Double -> VecS Double -> VecS Double
dynPrune k vec
  | minx > k = vec
  | otherwise   = unsafeMkVecS $ G.filter ((> cf) . snd) v
  where
    v = vecS vec
    (minx, avgx, maxx) = getPruningStat vec
    c0 = (maxx + avgx) / 2
    cf = go c0
    go !c
      | total < k = c
      | n <= 1    = c
      | otherwise = go avg
      where
        -- cut under the average to avoid infinite loop
        (avg, total, n) = getAvgCut vec (c * 0.98)

-- | Get some statistics parameters (minimum value, average, maximum value)
getPruningStat :: VecS Double -> (Double, Double, Double)
getPruningStat vec = (mi, avg, ma)
  where
    v      = vecS vec
    avg    = s / (fromIntegral $ G.length v)
    (s, mi, ma) = G.foldl' func (0, 1, 0) v
    func (!acc, !k, !t) (_, x) = (acc + x, min x k, max x t)

-- | Calculate the number of elements above a given threshold and their average value.
getAvgCut :: VecS Double -> Double -> (Double, Double, Int)
getAvgCut vec c
  | ncut > 0  = (avg, sumcut, ncut)
  | otherwise = (0, 0, 0)
  where
    avg = sumcut / (fromIntegral $ ncut)
    (sumcut, ncut) = G.foldl' func (0, 0 :: Int) (vecS vec)
    func acc@(!s, !n) (_, x)
      | x <= c    = (s+x, n+1)
      | otherwise = acc

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
