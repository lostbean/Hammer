{-# LANGUAGE OverloadedStrings #-}

module MicroGraphSpec (spec) where

import qualified Data.HashMap.Strict as HM
import Hammer.MicroGraph
import Hammer.MicroGraph.Base
import Hammer.MicroGraph.Types
import Test.Hspec

spec :: Spec
spec = do
    describe "Hammer.MicroGraph.Base" $ do
        it "initMicroGraph creates an empty graph" $ do
            let mg = initMicroGraph :: MicroGraph Int Int Int Int
            getGrainIDList mg `shouldBe` []

        it "insertNewGrain adds a grain" $ do
            let gid = mkGrainID 1
                mg = insertNewGrain gid (10 :: Int) initMicroGraph
            getGrainIDList mg `shouldBe` [gid]
            case getGrainProp gid mg of
                Just (GrainProp _ v) -> v `shouldBe` 10
                _ -> expectationFailure "Expected GrainProp"

        it "insertNewFace adds a face and connects to grains" $ do
            let fid = mkFaceID (1, 2)
                mg = insertNewFace fid (100 :: Int) initMicroGraph
                gid1 = mkGrainID 1
                gid2 = mkGrainID 2
            getGrainIDList mg `shouldMatchList` [gid1, gid2]
            case getFaceProp fid mg of
                Just (FaceProp _ v) -> v `shouldBe` 100
                _ -> expectationFailure "Expected FaceProp"

        it "insertNewVertex and insertNewEdge" $ do
            let gid1 = mkGrainID 1
                gid2 = mkGrainID 2
                gid3 = mkGrainID 3
                gid4 = mkGrainID 4
                f12 = mkFaceID (1, 2)
                f23 = mkFaceID (2, 3)
                f13 = mkFaceID (1, 3)
                vid = mkVertexID (1, 2, 3, 4)
                eid = mkEdgeID' (f12, f23, f13)
                mg = insertNewVertex vid (1 :: Int) [eid] initMicroGraph
            case getVertexProp vid mg of
                Just (VertexProp v) -> v `shouldBe` 1
                _ -> expectationFailure "Expected VertexProp"
            -- Edge should be created as NullEdgeProp because it's a sub-level of Vertex
            case getEdgeProp eid mg of
                Just (NullEdgeProp _) -> True `shouldBe` True
                _ -> expectationFailure "Expected NullEdgeProp"
