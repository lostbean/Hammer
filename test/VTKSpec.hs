module VTKSpec (spec) where

import qualified Data.Vector.Unboxed as U
import Hammer.VTK
import Hammer.VoxBox
import Test.Hspec

spec :: Spec
spec = do
    describe "Hammer.VTK" $ do
        it "getExtendedVoxBox extends dimensions by 1" $ do
            let vbr = mkStdVoxBoxRange (VoxBoxDim 2 2 2)
                vbox = VoxBox vbr (VoxBoxOrigin 0 0 0) (VoxelDim 1 1 1) (U.fromList [0 .. 7] :: U.Vector Int)
                VoxBoxExt vbext = getExtendedVoxBox vbox
            vbrDim (dimension vbext) `shouldBe` VoxBoxDim 3 3 3

        it "renderFacePos correctly maps corners" $ do
            let vbr = mkStdVoxBoxRange (VoxBoxDim 2 2 2)
                vbox = VoxBox vbr (VoxBoxOrigin 0 0 0) (VoxelDim 1 1 1) (U.fromList [0 .. 7] :: U.Vector Int)
                vbext = getExtendedVoxBox vbox
                -- Fx (0,0,0) face corners in a 2x2x2 grid extended to 3x3x3
                -- (0,0,0), (0,1,0), (0,1,1), (0,0,1)
                -- IDs in 3x3x3:
                -- (0,0,0) -> 0
                -- (0,1,0) -> 3
                -- (0,1,1) -> 3 + 9 = 12
                -- (0,0,1) -> 9
                res = renderFacePos vbext (Fx (VoxelPos 0 0 0))
            res `shouldBe` (0, 3, 12, 9)
