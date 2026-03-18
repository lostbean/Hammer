{-# LANGUAGE LambdaCase #-}

module SparseMatrixSpec (spec) where

import qualified Data.Vector as V
import Hammer.Math.SparseMatrix
import Test.Hspec

spec :: Spec
spec = do
    describe "Hammer.Math.SparseMatrix" $ do
        it "lookup in testR" $ do
            Hammer.Math.SparseMatrix.lookup (0, 0, 0) testR `shouldBe` Just 10
            Hammer.Math.SparseMatrix.lookup (1, 1, 1) testR `shouldBe` Just 0
            Hammer.Math.SparseMatrix.lookup (2, 2, 2) testR `shouldBe` Nothing

        it "singleton works" $ do
            let s = singleton (Sparse3Org (1, 2, 3)) (42 :: Int)
            Hammer.Math.SparseMatrix.lookup (1, 2, 3) s `shouldBe` Just 42
            Hammer.Math.SparseMatrix.lookup (0, 0, 0) s `shouldBe` Nothing

        it "mergeSparse3_X" $ do
            let m = mergeSparse3 testR testX
            m `shouldSatisfy` \case
                Just _ -> True
                Nothing -> False
            let Just merged = m
            Hammer.Math.SparseMatrix.lookup (0, 0, 0) merged `shouldBe` Just 10
            Hammer.Math.SparseMatrix.lookup (4, 0, 0) merged `shouldBe` Just 11
            Hammer.Math.SparseMatrix.lookup (4, 1, 1) merged `shouldBe` Just 1

        it "mergeSparse3_Y" $ do
            let m = mergeSparse3 testR testY
            m `shouldSatisfy` \case
                Just _ -> True
                Nothing -> False
            let Just merged = m
            Hammer.Math.SparseMatrix.lookup (0, 0, 0) merged `shouldBe` Just 10
            Hammer.Math.SparseMatrix.lookup (0, 4, 0) merged `shouldBe` Just 12

        it "mergeSparse3_Z" $ do
            let m = mergeSparse3 testR testZ
            m `shouldSatisfy` \case
                Just _ -> True
                Nothing -> False
            let Just merged = m
            Hammer.Math.SparseMatrix.lookup (0, 0, 0) merged `shouldBe` Just 10
            Hammer.Math.SparseMatrix.lookup (0, 0, 4) merged `shouldBe` Just 13

        it "adjust works" $ do
            let s = adjust (V.fromList [((0, 0, 0), 20 :: Int)]) testR
            Hammer.Math.SparseMatrix.lookup (0, 0, 0) s `shouldBe` Just 20
            Hammer.Math.SparseMatrix.lookup (1, 1, 1) s `shouldBe` Just 0
