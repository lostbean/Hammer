{-# LANGUAGE ScopedTypeVariables #-}

module TypesSpec (spec) where

import qualified Data.List as L
import Hammer.MicroGraph.Types
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    describe "Hammer.MicroGraph.Types" $ do
        it "fast3DSort property" $
            property $
                \(x :: Int, y, z) ->
                    let t = (x, y, z)
                        unlist [a, b, c] = (a, b, c)
                        unlist _ = error "Oops"
                     in all ((== fast3DSort t) . fast3DSort . unlist) (L.permutations [x, y, z])

        it "fast4DSort property" $
            property $
                \(x :: Int, y, z, w) ->
                    let t = (x, y, z, w)
                        unlist [a, b, c, d] = (a, b, c, d)
                        unlist _ = error "Oops"
                     in all ((== fast4DSort t) . fast4DSort . unlist) (L.permutations [x, y, z, w])
