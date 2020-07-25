module Hammer.Parallel.Vector
    ( parChunkEval
    , autoChunkEval
    , autoParVector 
    , autoParFilter 
    , parVector
    , parFilter
    , Control.Parallel.Strategies.runEval
    ) where

import GHC.Conc (numCapabilities)
import Control.Parallel.Strategies
import qualified Data.Vector.Generic as G

autoParVector :: (G.Vector v a, NFData (v a)) => v a -> Eval (v a)
autoParVector = autoChunkEval id

autoParFilter :: (G.Vector v a, NFData (v a)) => (a -> Bool) -> v a -> Eval (v a)
autoParFilter func = autoChunkEval (G.filter func)

parVector :: (G.Vector v a, NFData (v a)) => Int -> v a -> Eval (v a)
parVector = parChunkEval id

parFilter :: (G.Vector v a, NFData (v a)) => Int -> (a -> Bool) -> v a -> Eval (v a)
parFilter minChunk func = parChunkEval (G.filter func) minChunk

autoChunkEval :: (G.Vector v a, G.Vector v b, NFData (v b)) => (v a -> v b) -> v a -> Eval (v b)
autoChunkEval func vec
  | numCapabilities > 1 = parChunkEval func (G.length vec `div` (10 * numCapabilities)) vec
  | otherwise           = rseq (func vec)

parChunkEval :: (G.Vector v a, G.Vector v b, NFData (v b)) => (v a -> v b) -> Int -> v a -> Eval (v b)
parChunkEval func minChunk = fmap G.concat . innerPar
  where
    innerPar vec
      | vLen > minChunk = let
        half = vLen `div` 2
        v1 = G.unsafeSlice 0 half vec
        v2 = G.unsafeSlice half (vLen - half) vec
        in do
          a <- innerPar v1
          b <- innerPar v2
          return (a ++ b)
      | otherwise = do
          e <- rparWith rdeepseq (func vec)
          return [e]
      where
        vLen = G.length vec