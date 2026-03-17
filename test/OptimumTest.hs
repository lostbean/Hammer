module Main (main) where

import Hammer.Math.Optimum
import Linear.Vect

testLS :: Double
testLS = lineSearch (\(Vec2 x y) -> (x * x + y * y, Vec2 (2 * x) (2 * y))) (Vec2 1 0) (Vec2 (-4) (-2))

testBFGS :: Vec2 Double
testBFGS = bfgs defaultBFGS func (Vec2 100 431)
  where
    func (Vec2 x y) = (2 * x * x + 2 * x * y + 2 * y * y - 6 * x, Vec2 (4 * x + 2 * y - 6) (2 * x + 4 * y))

main :: IO ()
main = do
    putStrLn $ "testLS: " ++ show testLS
    putStrLn $ "testBFGS: " ++ show testBFGS
