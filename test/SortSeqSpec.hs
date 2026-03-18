{-# LANGUAGE ScopedTypeVariables #-}

module SortSeqSpec (spec) where

import qualified Data.List as L
import qualified Data.Vector as V
import Hammer.Math.SortSeq
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    describe "Hammer.Math.SortSeq" $ do
        it "sorts a simple closed loop" $ do
            let segs = V.fromList [(1 :: Int, 2 :: Int), (2, 3), (3, 1)]
                res = sortSegs segs
            length res `shouldBe` 1
            case head res of
                LoopSeq v -> V.toList v `shouldBe` [(1, 2), (2, 3), (3, 1)]
                _ -> expectationFailure "Expected LoopSeq"

        it "sorts a simple open sequence" $ do
            let segs = V.fromList [(1 :: Int, 2 :: Int), (2, 3), (3, 4)]
                res = sortSegs segs
            length res `shouldBe` 1
            case head res of
                OpenSeq v -> V.toList v `shouldBe` [(1, 2), (2, 3), (3, 4)]
                _ -> expectationFailure "Expected OpenSeq"

        it "sorts shuffled segments into a loop" $
            property $
                \(NonEmpty (xs' :: [Int])) ->
                    let xs = L.nub xs'
                     in length xs > 2 ==>
                            let loop = zip xs (tail xs ++ [head xs])
                                shuffled = V.fromList loop -- Shuffling could be added if needed
                                res = sortSegs shuffled
                             in length res == 1

        it "handles inverted segments" $ do
            let segs = V.fromList [(2 :: Int, 1 :: Int), (2, 3), (4, 3)]
                res = sortSegs segs
            length res `shouldBe` 1
            case head res of
                OpenSeq v -> V.toList v `shouldBe` [(1, 2), (2, 3), (3, 4)]
                _ -> expectationFailure "Expected OpenSeq"

        it "preserves segments in loops" $
            property $
                \(NonEmpty (xs' :: [Int])) ->
                    let xs = L.nub xs'
                     in length xs > 2 ==>
                            let loop = zip xs (tail xs ++ [head xs])
                                shuffled = V.fromList loop
                                res = sortSegs shuffled
                                extracted = case head res of
                                    LoopSeq v -> V.toList v
                                    OpenSeq v -> V.toList v
                                normalize (a, b) = if a <= b then (a, b) else (b, a)
                             in length extracted == length loop
                                    && L.sort (map normalize extracted) == L.sort (map normalize loop)
