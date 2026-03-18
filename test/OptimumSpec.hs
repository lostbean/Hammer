module OptimumSpec (spec) where

import Hammer.Math.Optimum
import Linear.Vect
import Test.Hspec

spec :: Spec
spec = do
    describe "Hammer.Math.Optimum" $ do
        it "lineSearch" $ do
            let res = lineSearch (\(Vec2 x y) -> (x * x + y * y, Vec2 (2 * x) (2 * y))) (Vec2 1 0) (Vec2 (-4) (-2))
            res `shouldSatisfy` (\x -> x > 0.1 && x < 0.3) -- Approximate value from original test
        it "bfgs" $ do
            let func (Vec2 x y) = (2 * x * x + 2 * x * y + 2 * y * y - 6 * x, Vec2 (4 * x + 2 * y - 6) (2 * x + 4 * y))
                res = bfgs defaultBFGS func (Vec2 100 431)
                Vec2 rx ry = res
            rx `shouldSatisfy` (\x -> abs (x - 2) < 1e-6)
            ry `shouldSatisfy` (\x -> abs (x + 1) < 1e-6)
