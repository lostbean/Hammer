{-# LANGUAGE LambdaCase #-}

module VoxConnSpec (spec) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as U
import Hammer.MicroGraph.Types
import Hammer.VoxBox
import Hammer.VoxConn
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    describe "Hammer.VoxConn" $ do
        it "grainFinder finds a simple grain" $ do
            let vbr = mkStdVoxBoxRange (VoxBoxDim 2 2 1)
                -- 2x2 grid:
                -- 1 1
                -- 0 0
                vbox = VoxBox vbr (VoxBoxOrigin 0 0 0) (VoxelDim 1 1 1) (U.fromList [1, 1, 0, 0] :: U.Vector Int)
                res = grainFinder (==) vbox
            res `shouldSatisfy` \case
                Just (vb, hmap) ->
                    HM.size hmap == 2
                        && U.length (grainID vb) == 4
                Nothing -> False

        it "grainFinder finds connected components" $ do
            let vbr = mkStdVoxBoxRange (VoxBoxDim 3 3 1)
                -- 1 0 1
                -- 1 0 1
                -- 1 1 1
                vbox = VoxBox vbr (VoxBoxOrigin 0 0 0) (VoxelDim 1 1 1) (U.fromList [1, 0, 1, 1, 0, 1, 1, 1, 1] :: U.Vector Int)
                res = grainFinder (==) vbox
            res `shouldSatisfy` \case
                Just (_, hmap) -> HM.size hmap == 2 -- One for 1s, one for 0s
                Nothing -> False
