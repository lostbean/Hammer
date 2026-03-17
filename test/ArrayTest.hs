module Main (main) where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Linear.Class

import Hammer.Math.Array

testIx2D :: (Int, Int) -> Bool
testIx2D s =
    let
        shp = Shape s
        ps = V.fromList $ rangeIx shp :: V.Vector (Int, Int)
        ts = V.imap (\i p -> i == (pos shp p) && p == (ix shp i)) ps
     in
        linLen shp == V.length ps && V.and ts

testIx3D :: (Int, Int, Int) -> Bool
testIx3D s =
    let
        shp = Shape s
        ps = V.fromList $ rangeIx shp :: V.Vector (Int, Int, Int)
        ts = V.imap (\i p -> i == (pos shp p) && p == (ix shp i)) ps
     in
        linLen shp == V.length ps && V.and ts

testMatrix :: Bool
testMatrix =
    let
        ma = genMatrix2D (\(i, j) -> fromIntegral $ i ^ (2 :: Int) - 2 * j) (Shape (5, 3))
        mb = genMatrix2D (\(i, j) -> fromIntegral $ j ^ (2 :: Int) - 2 * i) (Shape (3, 5))
        mc = genMatrix2D (\(i, j) -> fromIntegral $ (i ^ (4 :: Int)) * j) (Shape (3, 5))
        shp = Shape (5 :: Int, 5 :: Int)
        mabt = mkUnsafeArray shp $ VU.fromList xs1
        xs1 :: [Double]
        xs1 =
            [ 20
            , 14
            , -4
            , -34
            , -76
            , 14
            , 11
            , 2
            , -13
            , -34
            , -4
            , 2
            , 20
            , 50
            , 92
            , -34
            , -13
            , 50
            , 155
            , 302
            , -76
            , -34
            , 92
            , 302
            , 596
            ]
        mact = mkUnsafeArray shp $ VU.fromList xs2
        xs2 :: [Double]
        xs2 =
            [ 0
            , -66
            , -132
            , -198
            , -264
            , 0
            , -49
            , -98
            , -147
            , -196
            , 0
            , 2
            , 4
            , 6
            , 8
            , 0
            , 87
            , 174
            , 261
            , 348
            , 0
            , 206
            , 412
            , 618
            , 824
            ]
        t1 = transpose ma == mb
        t2 = mabt == ma .*. mb
        t3 = transpose mact == ma .*. mc
        t4 = VU.fromList [20, 11, 20, 155, 596] == diagVec mabt
     in
        t1 && t2 && t3 && t4

main :: IO ()
main = do
    putStrLn $ "testIx2D (3,4): " ++ show (testIx2D (3, 4))
    putStrLn $ "testIx3D (2,3,4): " ++ show (testIx3D (2, 3, 4))
    putStrLn $ "testMatrix: " ++ show testMatrix
