module Main (main) where

import qualified Data.List as L

import Hammer.MicroGraph.Types

testFast3 :: (Ord a) => (a, a, a) -> Bool
testFast3 t@(x, y, z) =
    let
        unlist [a, b, c] = (a, b, c)
        unlist _ = error "[GrainsGraph] Oops! List size different than 3"
        func = all ((== fast3DSort t) . fast3DSort . unlist)
     in
        func $ L.permutations [x, y, z]

testFast4 :: (Ord a) => (a, a, a, a) -> Bool
testFast4 t@(x, y, z, w) =
    let
        unlist [a, b, c, d] = (a, b, c, d)
        unlist _ = error "[GrainsGraph] Oops! List size different than 4"
        func = all ((== fast4DSort t) . fast4DSort . unlist)
     in
        func $ L.permutations [x, y, z, w]

main :: IO ()
main = do
    putStrLn $ "testFast3 (1,2,3): " ++ show (testFast3 (1 :: Int, 2, 3))
    putStrLn $ "testFast3 (3,1,2): " ++ show (testFast3 (3 :: Int, 1, 2))
    putStrLn $ "testFast4 (1,2,3,4): " ++ show (testFast4 (1 :: Int, 2, 3, 4))
    putStrLn $ "testFast4 (4,2,1,3): " ++ show (testFast4 (4 :: Int, 2, 1, 3))
