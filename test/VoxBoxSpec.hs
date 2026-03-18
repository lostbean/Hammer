module VoxBoxSpec (spec) where

import qualified Data.Vector.Unboxed as U
import Hammer.VoxBox
import Linear.Vect
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    describe "Hammer.VoxBox" $ do
        it "VoxelPos addition and subtraction" $
            property $
                \(x1, y1, z1, x2, y2, z2) ->
                    let v1 = VoxelPos x1 y1 z1
                        v2 = VoxelPos x2 y2 z2
                     in (v1 #+# v2) #-# v2 == v1

        it "sizeVoxBoxRange" $ do
            let vbr = mkStdVoxBoxRange (VoxBoxDim 2 3 4)
            sizeVoxBoxRange vbr `shouldBe` 2 * 3 * 4

        it "isInBox" $ do
            let vbr = mkStdVoxBoxRange (VoxBoxDim 10 10 10)
            isInBox vbr (VoxelPos 5 5 5) `shouldBe` True
            isInBox vbr (VoxelPos 10 10 10) `shouldBe` False -- 0 to 9 are in box
        it "getVoxelID and back" $
            property $
                \(Positive dx, Positive dy, Positive dz) ->
                    let vbr = mkStdVoxBoxRange (VoxBoxDim dx dy dz)
                     in property $ forAll (choose (0, dx - 1)) $ \x ->
                            forAll (choose (0, dy - 1)) $ \y ->
                                forAll (choose (0, dz - 1)) $ \z ->
                                    let p = VoxelPos x y z
                                        vid = vbr %@ p
                                     in vbr %# vid == p

        it "VoxBox lookup" $ do
            let vbr = mkStdVoxBoxRange (VoxBoxDim 2 2 2)
                vbox = VoxBox vbr (VoxBoxOrigin 0 0 0) (VoxelDim 1 1 1) (U.fromList [1 .. 8] :: U.Vector Int)
            vbox #! (VoxelPos 0 0 0) `shouldBe` (1 :: Int)
            vbox #! (VoxelPos 1 0 0) `shouldBe` (2 :: Int)
            vbox #! (VoxelPos 0 1 0) `shouldBe` (3 :: Int)
            vbox #! (VoxelPos 0 0 1) `shouldBe` (5 :: Int)
            vbox #!? (VoxelPos 2 2 2) `shouldBe` (Nothing :: Maybe Int)

        it "toFacePos and back" $ do
            let fvp = Fx (VoxelPos 1 2 3)
                fp = toFacePos fvp
                fvp' = toFaceVoxelPos (FacePos fp)
            fvp' `shouldBe` fvp

        it "isFaceX/Y/Z" $ do
            isFaceX (FacePos (VoxelPos 1 0 0)) `shouldBe` True
            isFaceY (FacePos (VoxelPos 0 1 0)) `shouldBe` True
            isFaceZ (FacePos (VoxelPos 0 0 1)) `shouldBe` True

        it "evalCentralVoxelPos" $ do
            let vbr = mkStdVoxBoxRange (VoxBoxDim 10 10 10)
                vbox = VoxBox vbr (VoxBoxOrigin 0 0 0) (VoxelDim 1 1 1) (U.empty :: U.Vector Int)
            evalCentralVoxelPos vbox (VoxelPos 0 0 0) `shouldBe` Vec3 0 0 0
            evalCentralVoxelPos vbox (VoxelPos 1 1 1) `shouldBe` Vec3 1 1 1
