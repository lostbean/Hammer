
module Main where

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector         as V
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap         as IM
import qualified Data.List           as L

import           System.Random
import           Criterion.Main

import           Hammer.MicroGraph
import           Hammer.Render.VTK.VTKRender
import           Hammer.Render.VoxBoxVTK
import           Hammer.VoxBox.Base
import           Hammer.VoxBox.VoxConnFinder
import           Hammer.VoxBox.MicroVoxel
import           Hammer.Math.SparseMatrix



getRandList :: (RandomGen g, Random a)=> g -> Int -> [a]
getRandList _ 0 = []
getRandList gen n = v:rest where
    (v, gen') = random gen
    rest = getRandList gen' (n - 1)
    
testmap :: Int -> HM.HashMap Int Int
testmap n =
    L.foldl' (\m k -> HM.insert k 1 m) HM.empty [0..n]

testintmap :: Int -> IM.IntMap Int
testintmap n =
    L.foldl' (\m k -> IM.insert k 1 m) IM.empty [0..n]

testuvec :: Int -> VU.Vector Int
testuvec n =
    L.foldl' (\m k -> k `VU.cons` m) VU.empty [0..n]
   
testvec :: Int -> V.Vector Int
testvec n =
    L.foldl' (\m k -> k `V.cons` m) V.empty [0..n]



main :: IO () 
main = do
    gen <- getStdGen
    let
      inData1 = getRandList gen size :: [Int]
      inData2 = getRandList gen size :: [Int]
      inPair1 = zip inData1 [(1::Int)..]
      inPair2 = zip inData2 [(1::Int)..]
      inTrio1 = zip [(x,y,z)| z <- [10,100], y <- [0..size1], x <- [0..size1]] inData1

      inVU1 = VU.fromList inPair1
      inVU2 = VU.fromList inPair2
      inIM1 = IM.fromList inPair1
      inIM2 = IM.fromList inPair2
      inHM1 = HM.fromList inPair1
      inHM2 = HM.fromList inPair2

      inS  = mkSparse3 (Sparse3Org (0,0,0))     (Sparse3Dim (size1,size1,size1)) (V.fromList inTrio1)
      inSX = mkSparse3 (Sparse3Org (size1,0,0)) (Sparse3Dim (size1,size1,size1)) (V.fromList inTrio1)
      inSY = mkSparse3 (Sparse3Org (0,size1,0)) (Sparse3Dim (size1,size1,size1)) (V.fromList inTrio1)
      inSZ = mkSparse3 (Sparse3Org (0,0,size1)) (Sparse3Dim (size1,size1,size1)) (V.fromList inTrio1)
    
      size1 = 400
      size  = 2 * size1 * size1

    defaultMain $ 
      [ bench "fromList Unboxed Vector" $ nf (VU.fromList) inData1
      --, bench "fromList Vector"  $ nf (V.fromList)  inData1
      --, bench "fromList IntMap"  $ nf (IM.fromList) inPair1
      --, bench "fromList HashMap" $ nf (HM.fromList) inPair1
      --, bench "fromList Sparse3" $ nf (\x -> mkSparse3 (V.fromList x) (Sparse3Org (0,0,0)) (Sparse3Dim (size1,size1,size1))) inTrio1
        
      --, bench "insert Unboxed Vector" $ whnf testuvec 1000
      --, bench "insert Vector"         $ whnf testvec  1000
      --, bench "insert IntMap"    $ whnf testintmap size
      --, bench "insert HashMap"   $ whnf testmap    size

      --, bench "union Unboxed Vector" $ nf (VU.++ inVU1) inVU2
      --, bench "union IntMap"         $ nf (IM.union inIM1) inIM2
      --, bench "union HashMap"        $ nf (HM.union inHM1) inHM2
      , bench "union Sparse3[X]"     $ nf (mergeSparse3_X inS)   inSX
      , bench "union Sparse3[Y]"     $ nf (mergeSparse3_Y inS)   inSY
      , bench "union Sparse3[Z]"     $ nf (mergeSparse3_Z inS)   inSZ
      --, bench "union Sparse3[Y] ST"  $ nf (mergeSparse3_Y_M inS) inSY
         
      ] ++ benchmarkMicroVoxel vboxTest

vboxTest = VoxBox { dimension = mkStdVoxBoxRange (VoxBoxDim 21   15   5)
                  , origin    = VoxBoxOrigin  0    0    0
                  , spacing   = VoxelDim      1    1    1 
                  , grainID   = g }
  where
    g :: VU.Vector Int
    g = VU.fromList
          [2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4

          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,3,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,3,3,3,3,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,3,3,2,3,3,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,3,3,2,3,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,3,2,3,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
                                                   
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,3,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,3,3,3,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,3,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4
          ,2,2,3,2,2,2,2,2,2,2,3,3,3,3,4,4,4,4,4,4,4
          ,2,2,3,2,2,2,2,2,2,2,3,4,4,4,4,4,4,4,4,4,4
          ,2,2,3,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
                                                   
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4
          ,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4
          ,2,2,2,2,2,3,2,1,1,1,1,1,4,4,3,4,4,4,4,4,4
          ,2,2,2,2,2,3,2,1,2,2,4,1,4,4,3,4,4,4,4,4,4
          ,2,2,2,2,2,3,2,1,1,2,4,1,4,4,3,4,4,4,4,4,4
          ,2,2,2,2,2,3,2,2,2,2,4,4,4,4,3,4,4,4,4,4,4
          ,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4
          ,2,2,2,2,3,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,3,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,3,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,3,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,3,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
                                                   
          ,2,2,2,2,2,2,2,2,2,1,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,1,4,4,4,4,4,4,4,4,4,4,4
          ,2,1,1,1,1,1,1,1,1,1,4,4,4,4,4,4,4,4,4,4,4
          ,2,1,2,2,2,2,2,2,2,1,4,4,4,4,4,4,4,4,4,4,4
          ,2,1,2,1,1,1,1,2,2,1,4,4,4,4,4,4,4,4,4,4,4
          ,2,1,2,2,2,2,1,2,2,1,4,4,4,4,4,4,4,4,4,4,4
          ,2,1,2,1,1,1,1,2,2,1,4,4,4,4,4,4,4,4,4,4,4
          ,2,1,2,1,2,2,2,2,2,1,4,4,4,4,4,4,4,4,4,4,4
          ,2,1,1,1,2,1,1,1,2,1,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,1,2,1,2,1,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,1,2,2,2,1,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,1,1,1,1,1,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,1,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,1,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,1,4,4,4,4,4,4,4,4,4,4,4

          ]

