module Hammer.Parallel.Vector
    ( autoParVector 
    , parVector
    ) where

import GHC.Conc (numCapabilities)
import Control.Parallel.Strategies
import qualified Data.Vector.Generic as G

autoParVector :: (Traversable v, G.Vector v a, NFData a) => Strategy (v a)
autoParVector vec = parVector (G.length vec `div` (100 * numCapabilities)) vec

parVector :: (Traversable v, G.Vector v a, NFData a) => Int -> Strategy (v a)
parVector minChunk vecO = innerPar vecO >> return vecO
  where
    innerPar :: (Traversable v, G.Vector v a, NFData a) => Strategy (v a) 
    innerPar vec
      | vLen > minChunk = let
        half = vLen `div` 2
        v1 = G.unsafeSlice 0 half vec
        v2 = G.unsafeSlice half (vLen - half) vec
        in do
          _ <- innerPar v1
          _ <- innerPar v2
          return vec
      | otherwise = rparWith (evalTraversable rdeepseq) $ vec 
      where
        vLen = G.length vec