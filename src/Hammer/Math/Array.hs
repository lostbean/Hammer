{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Hammer.Math.Array
       ( -- * MultiArray
         MultiArray (bounds, array)
       , IxArray    (..)
       , mkMultiArray
         -- * Tools
       , binarySearch
       , shuffleVector
       ) where

import qualified Data.Vector.Generic         as V
import qualified Data.Vector.Generic.Mutable as VM
import qualified Data.Vector.Unboxed         as VU

import           Data.Vector.Generic (Vector)
import           Data.Bits           (shiftR)

import           System.Random

-- ====================================== MultiArray ====================================

data (IxArray i)=> MultiArray i e = MultiArray
                   { bounds :: (i, i)
                   , array  :: VU.Vector e
                   } deriving (Show)

class (Ord a)=> IxArray a where
  ix        :: (a, a) -> Int -> a
  pos       :: (a, a) -> a -> Int
  rangeIx   :: (a, a) -> [a]
  rangePos  :: (a, a) -> [Int]
  ixSize    :: (a, a) -> Int

instance IxArray (Int, Int, Int) where
  ix ((xi, yi, zi), (xf, yf, _)) = let
    dx = 1 + xf - xi
    dy = 1 + yf - yi
    dxy = dx * dy
    in \p -> let
      (z, xy) = quotRem p  dxy
      (y, x)  = quotRem xy dy
      in (xi + x, yi + y, zi + z)
  pos ((xi, yi, zi), (xf, yf, _)) = let
    dx = 1 + xf - xi
    dy = 1 + yf - yi
    dxy = dx * dy
    in \(x,y,z) -> (x - xi) + dx*(y - yi) + dxy*(z - zi)
  rangeIx ((xi, yi, zi), (xf, yf, zf)) = [(i,j,k) | i <- [xi..xf]
                                                  , j <- [yi..yf]
                                                  , k <- [zi..zf]]
  rangePos bd = [0 .. (ixSize bd) - 1]
  ixSize ((xi, yi, zi), (xf, yf, zf)) = let
    dx = 1 + xf - xi
    dy = 1 + yf - yi
    dz = 1 + zf - zi
    in dx * dy * dz

mkMultiArray :: (IxArray i, VU.Unbox e)=> (i, i) -> VU.Vector e -> MultiArray i e
mkMultiArray bd vec
  | sb == sv  = MultiArray bd vec
  | otherwise = error "[MultiArray] Array size and input vector don't match!"
  where
    sb = ixSize bd
    sv = V.length vec

-- ======================================= Vector Tools ==================================

binarySearch :: (Ord a, Vector v a) => v a -> a -> Int
binarySearch vec x = loop 0 (V.length vec - 1)
  where
    loop !l !u
      | u <= l    = l
      | otherwise = case compare x' x of
        LT -> loop (k+1) u
        GT -> loop l     k
        EQ -> k
      where
        k  = (u + l) `shiftR` 1
        x' = V.unsafeIndex vec k

-- | Shuffles a vector by random swaps where the random seed is given by the size of the
-- input vector. Therefore, vectors with the same size will have the same shuffling pattern. 
shuffleVector :: (Vector v a)=> v a -> v a
shuffleVector vec = V.modify shuffle vec
  where
    imax = V.length vec - 1
    seed = mkStdGen imax
    v1   = VU.enumFromN 0 imax
    shuffle mvec = let
      func gen i = do
        let (n, newgen) = randomR (0, imax) gen
        VM.swap mvec i n
        return newgen
      in VU.foldM'_ func seed v1
