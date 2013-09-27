{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Module to sort and classify sequence of segments.
For example:

> sortSegs $ V.fromList [(1::Int,2::Int), (10,1), (2,5), (10,5)]
> [LoopSeq fromList [(1,2),(2,5),(5,10),(10,1)]]

> sortSegs $ V.fromList [(1::Int,11::Int), (10,1), (2,5), (10,5)]
> [OpenSeq (1,11) fromList [(11,2),(2,5),(5,10),(10,1)]]

-}
module Hammer.Math.SortSeq
  ( SeqSeg  (..)
  , SeqComp (..)
  , Seq (..)
    -- * Sorting sequences
  , sortSegs
  , sortSegsIndirect
    -- * Filter sequence type
  , splitOpenLoop
  , getOneOpen
  , getOpens
  , getOneLoop
  , getLoops
  ) where

import           Control.Applicative ((<$>))
import           Control.Monad.ST    (runST)
import           Data.Vector         (Vector)

import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as VM
import qualified Data.List           as L

--import           Debug.Trace
--dbg a = trace (">> " ++ show a) a

-- ====================================== Data types ====================================

-- | Basic class definition for sorting segments.
class (SeqComp (SeqUnit a))=> SeqSeg a where
  -- | What segments are made of.
  type SeqUnit a
  -- | Acquire the first element in the segment.
  getSeqHead :: a -> SeqUnit a
  -- | Acquire the last element in the segment.
  getSeqTail :: a -> SeqUnit a
  -- | How to invert the direction of the segment. The default implementation *doesn't*
  -- invert the segment ('id').
  seqInv :: a -> a
  seqInv = id

-- | Check the equality of two elements. If you need more complex equality test (e.g.
-- within a tolerance error) implement by yourself otherwise a standard implementation is
-- given with '==', therefore simple equality test can implemented by:
--
-- > instance SeqComp TheType
--
class (Eq a)=> SeqComp a where
  seqComp :: a -> a -> Bool
  seqComp = (==)

-- | 'Seq' holds the results of the sorting algorithm by classifying it in open or closed
-- sequences.
data Seq a
  = OpenSeq { openSeq :: Vector a }
  | LoopSeq { loopSeq :: Vector a }

-- ======================================= Instances =====================================

instance (SeqComp a)=> SeqSeg (a, a) where
  type SeqUnit (a, a) = a
  getSeqHead   = fst
  getSeqTail   = snd
  seqInv (a,b) = (b,a)

instance (Show a, SeqSeg a)=> Show (Seq a) where
  show (OpenSeq s) = "OpenSeq " ++ show s
  show (LoopSeq s) = "LoopSeq " ++ show s

-- ====================================== Sort algorithm =================================

-- | Sorts a vector of 'Int's used as reference (pointers) to another vector containing
-- the values.
sortSegsIndirect ::(SeqSeg b)=> Vector b -> Vector Int -> [Seq Int]
sortSegsIndirect v = fst . fastSortSegs (getSeqHead . (v V.!)) (getSeqTail . (v V.!)) id

-- | Classify a list segments in 'OpenSeq' and 'LoopSeq', open and closed sequences. The
-- resulting sequence is also aligned, it means that the tail of a segment match the head
-- of the following segment and vice-verse. Branches and inter-loops generate have multiple
-- solution and this function will return just one of them.
sortSegs :: (SeqSeg a)=> Vector a -> [Seq a]
sortSegs = fst . fastSortSegs getSeqHead getSeqTail seqInv

-- Internal function that actually does the sorting.
fastSortSegs :: (SeqComp b)=> (a -> b) -> (a -> b) -> (a -> a)
             -> Vector a -> ([Seq a], Vector a)
fastSortSegs getHead getTail inv es = runST $ do
  v <- V.thaw es
  let
    maxsize = (V.length es) - 1

    mkSeq i xi f xf
      | xi `seqComp` xf  = LoopSeq <$> seg
      | otherwise        = OpenSeq <$> seg
      where
        len = f - i + 1
        seg = V.unsafeFreeze $ VM.slice i len v

    getFs i = do
      f <- VM.read v i
      return (getHead f, getTail f)

    -- Revert the sequence between two given positions
    revert a b = let
      l        = min a b
      u        = max a b
      n        = u - l + 1
      (q, _)   = n `quotRem` 2
      forward  = V.enumFromN     l      q
      backward = V.enumFromStepN u (-1) q
      in do
        V.zipWithM_ (VM.swap v) forward backward
        V.mapM_ invert (V.enumFromN l n)

    -- Invert the segment at given position
    invert i = do
      x <- VM.read v i
      VM.write v i (inv x)

    -- Finds chunks of sequences
    runSeq breaks start
      | start > maxsize = return breaks
      | otherwise = do
        (ta, tb)     <- getFs start
        -- There are two possible solution:
        -- -|=========|++++++|
        -- -|====|====|++++++|
        -- First scan the sequence from tb
        (endB, tagA) <- scanForward tb start (start + 1)
        -- Then search for segments from ta
        (endA, tagB) <- scanForward ta endB  (endB + 1)
        -- if the start position is equal to the final position in the second search
        if endA == endB
           -- then nothing was found and the only sequence is in the first scan
          then do
          brk <- mkSeq start ta endB tagA
          runSeq (brk:breaks) (endB + 1)
          -- else revert the first scan to match to the second one
          else do
          revert start endB
          brk <- mkSeq start tagA endA tagB
          runSeq (brk:breaks) (endA + 1)

    -- Build a sequence by looking for the segment containing a element equals 'target'
    -- and swapping it with the segement in the next position.
    -- > 'target' is the head or tail of a segment that is looking for a match
    -- > 'ref' is the position that contains the 'target' element
    -- > 'i' is the current position that ranges from 'ref+1' until the last position
    scanForward target ref i
      | i <= maxsize = do
        (a, b) <- getFs i
        foo a b i
      | otherwise = return (ref, target)
      where
        foo a b n
          | target `seqComp` a = VM.swap v n (ref+1) >> scanForward b (ref+1) (ref+2)
          | target `seqComp` b = VM.swap v n (ref+1) >> invert (ref+1) >> scanForward a (ref+1) (ref+2)
          | n < maxsize        = scanForward target ref (n+1)
          | otherwise          = return (ref, target)

  breaks <- runSeq [] 0
  imv    <- V.unsafeFreeze v
  return (breaks, imv)

-- ==================================== Process results ==================================

-- | Split the result in open sequences (first element) and closed loops (second element).
splitOpenLoop :: [Seq a] -> ([Seq a], [Seq a])
splitOpenLoop = let
  isOpen (OpenSeq _) = True
  isOpen _           = False
  in L.partition isOpen

-- | Get only open sequences.
getOpens :: (SeqSeg a)=> [Seq a] -> [Vector a]
getOpens = let
  foo (OpenSeq x) = x
  foo _           = error "[SeqSeg] Exepection LoopSeq. It must be a bug."
  in map foo . fst . splitOpenLoop

-- | Successfully returns if there is one and only one open sequence.
getOneOpen :: (SeqSeg a)=> [Seq a] -> Maybe (Vector a)
getOneOpen ss = case getLoops ss of
  [s] -> return s
  _   -> Nothing

-- | Get only closed loops sequences.
getLoops :: (SeqSeg a)=> [Seq a] -> [Vector a]
getLoops = let
  foo (LoopSeq x) = x
  foo _           = error "[SeqSeg] Exepection LoopSeq. It must be a bug."
  in map foo . snd . splitOpenLoop

-- | Successfully returns if there is one and only one closed loop.
getOneLoop :: (SeqSeg a)=> [Seq a] -> Maybe (Vector a)
getOneLoop ss = case getLoops ss of
  [s] -> return s
  _   -> Nothing
