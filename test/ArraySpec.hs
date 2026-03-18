module ArraySpec (spec) where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Test.Hspec
import Test.QuickCheck

import Hammer.Math.Array
import Linear.Class

spec :: Spec
spec = do
    describe "Hammer.Math.Array" $ do
        it "testIx2D property" $
            property $
                \(Positive w, Positive h) ->
                    let s = (w, h)
                        shp = Shape s
                        ps = V.fromList $ rangeIx shp :: V.Vector (Int, Int)
                        ts = V.imap (\i p -> i == (pos shp p) && p == (ix shp i)) ps
                     in linLen shp == V.length ps && V.and ts

        it "testIx3D property" $
            property $
                \(Positive x, Positive y, Positive z) ->
                    let s = (x, y, z)
                        shp = Shape s
                        ps = V.fromList $ rangeIx shp :: V.Vector (Int, Int, Int)
                        ts = V.imap (\i p -> i == (pos shp p) && p == (ix shp i)) ps
                     in linLen shp == V.length ps && V.and ts

        it "testMatrix" $ do
            let
                ma = genMatrix2D (\(i, j) -> fromIntegral $ i ^ (2 :: Int) - 2 * j) (Shape (5, 3))
                mb = genMatrix2D (\(i, j) -> fromIntegral $ j ^ (2 :: Int) - 2 * i) (Shape (3, 5))
                mc = genMatrix2D (\(i, j) -> fromIntegral $ (i ^ (4 :: Int)) * j) (Shape (3, 5))
                shp = Shape (5 :: Int, 5 :: Int)
                mabt = mkUnsafeArray shp $ VU.fromList xs1
                xs1 :: [Double]
                xs1 = [20, 14, -4, -34, -76, 14, 11, 2, -13, -34, -4, 2, 20, 50, 92, -34, -13, 50, 155, 302, -76, -34, 92, 302, 596]
                mact = mkUnsafeArray shp $ VU.fromList xs2
                xs2 :: [Double]
                xs2 = [0, -66, -132, -198, -264, 0, -49, -98, -147, -196, 0, 2, 4, 6, 8, 0, 87, 174, 261, 348, 0, 206, 412, 618, 824]

            transpose ma `shouldBe` mb
            (ma .*. mb) `shouldBe` mabt
            (ma .*. mc) `shouldBe` transpose mact
            diagVec mabt `shouldBe` VU.fromList [20, 11, 20, 155, 596]

        it "zeroMatrix2D" $ do
            let m = zeroMatrix2D (Shape (2, 2))
            VU.all (== 0) (array m) `shouldBe` True

        it "idIdMatrix2D" $ do
            let m = idIdMatrix2D 2
            VU.toList (array m) `shouldBe` [1, 0, 0, 1]

        it "binarySearch" $ do
            let v = VU.fromList [1, 3, 5, 7, 9 :: Int]
            binarySearch v 5 `shouldBe` 2
            binarySearch v 1 `shouldBe` 0
            binarySearch v 9 `shouldBe` 4
            binarySearch v 0 `shouldBe` 0
            binarySearch v 10 `shouldBe` 4

        it "shuffleVector" $ do
            let v = VU.fromList [1 .. 10 :: Int]
                s1 = shuffleVector v
                s2 = shuffleVector v
            s1 `shouldBe` s2 -- Shuffling is deterministic based on size
            VU.length s1 `shouldBe` 10
            VU.all (\x -> VU.elem x s1) v `shouldBe` True
