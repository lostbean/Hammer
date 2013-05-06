{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module Hammer.Math.SortSeq
  ( getListSegs
  , getVecSegs
  , getVecSegsIndirect
    
  , splitOpenLoop
  , getOneLoop
  , getLoops

  , Seq (..)
  , SeqSeg  (..)
  , SeqInv  (..)
  , SeqComp (..)

  , closeSeq
  ) where

import           Control.Applicative     ((<$>))
import           Control.Monad           (liftM, zipWithM_)
import           Control.Monad.ST        (runST)
import           Data.Maybe              (isJust)
import           Data.Vector             (Vector)
  
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as VM
import qualified Data.List           as L

import           Debug.Trace
dbg a = trace (">> " ++ show a) a
        
-- ====================================== Data types ====================================
        
class (SeqComp (SeqUnit a))=> SeqSeg a where
  type SeqUnit a
  seqHead :: a -> SeqUnit a
  seqTail :: a -> SeqUnit a

class (Eq a)=> SeqComp a where
  seqComp :: a -> a -> Bool
  seqComp = (==)

class SeqInv a where
  seqInv :: a -> a
  seqInv = id

instance (SeqComp a)=> SeqSeg (a, a) where
  type SeqUnit (a, a) = a
  seqHead = fst
  seqTail = snd

instance SeqInv (a, a) where
  seqInv (a,b) = (b,a) 

instance SeqInv Int

instance (Show a, Show u, Show (c a), SeqSeg a)=> Show (Seq c a u) where
  show (OpenSeq a b s) = "OpenSeq " ++ show (a,b) ++ " " ++ show s
  show (LoopSeq s)     = "LoopSeq " ++ show s
  
data Seq c a u
  = OpenSeq
    { seqIni  :: !u --(SeqUnit a)
    , seqEnd  :: !u --(SeqUnit a)
    , seqList :: c a
    }
  | LoopSeq { loopList :: c a }

-- ====================================== Vector ====================================

getVecSegsIndirect ::(SeqSeg b)=> Vector b -> Vector Int -> [Seq Vector Int (SeqUnit b)]
getVecSegsIndirect v = fst . sortSegs (seqHead . (v V.!)) (seqTail . (v V.!)) id

getVecSegs :: (SeqSeg a, SeqInv a)=> Vector a -> [Seq Vector a (SeqUnit a)]
getVecSegs = fst . sortSegs seqHead seqTail seqInv

sortSegs :: (SeqComp b)=> (a -> b) -> (a -> b) -> (a -> a)
         -> Vector a -> ([Seq Vector a b], Vector a)
sortSegs getHead getTail inv es = runST $ do
  v <- V.thaw es
  let
    maxsize = (V.length es) - 1

    mkSeq i xi f xf
      | xi `seqComp` xf  = LoopSeq       <$> seg
      | otherwise = OpenSeq xi xf <$> seg
      where
        len = f - i + 1 
        seg = V.unsafeFreeze $ VM.slice i len v
    
    getFs i = do
      f <- VM.read v i
      return (getHead f, getTail f)

    revert a b = let
      (q, r) = (abs (b - a)) `quotRem` 2
      n      = if r == 0 then q else q+1
      forward  = take n [a ..]
      backward = take n [b, b-1 ..]
      in zipWithM_ (VM.swap v) forward backward 
  
    runSeq breaks start
      | start > maxsize = return breaks
      | otherwise = do
        (ta, tb) <- getFs start
        (endB, tagA) <- scanForward tb start (start + 1)
        (endA, tagB) <- scanForward ta endB  (endB + 1)
        if endA == endB
          then do 
          brk <- mkSeq start ta endB tagA
          runSeq (brk:breaks) (endB + 1)
          else do
          brk <- mkSeq start tagB endA tagA
          revert start endB
          runSeq (brk:breaks) (endA + 1)

    invert i = do
      x <- VM.read v i
      VM.write v i (inv x)
    
    --scanForward Nothing ref _ = return (ref, Nothing)
    scanForward target ref i
      | i <= maxsize = do
        (a, b) <- getFs i
        foo a b i
      | otherwise = return (ref, target)
      where
        foo a b n
          | target `seqComp` a = VM.swap v n (ref+1) >> scanForward b (ref+1) (ref+2)
          | target `seqComp` b = VM.swap v n (ref+1) >> invert (ref+1) >> scanForward a (ref+1) (ref+2)
          | n < maxsize = scanForward target ref (n+1)
          | otherwise   = return (ref, target)
  
  breaks <- runSeq [] 0
  imv    <- V.unsafeFreeze v
  return (breaks, imv)

-- ====================================== List ====================================

closeSeq :: (SeqSeg a, SeqInv a)=>[a] -> Maybe [a]
closeSeq []        = Nothing
closeSeq [_]       = Nothing
closeSeq (seed:xs) = liftM (seed:) (getSeq (seqTail seed) xs) 
  where
    --nextRef :: (SeqSeg a, Eq (SeqUnit a))=> SeqUnit a -> a -> Maybe (SeqUnit a)
    nextRef x t
      | x == a && x /= b = Just (b, t)
      | x /= a && x == b = Just (a, seqInv t)
      | otherwise        = Nothing
      where
        a = seqHead t
        b = seqTail t

    --isIn :: (SeqSeg a, Eq (SeqUnit a))=> SeqUnit a -> a -> Bool
    isIn x    = isJust . nextRef x
    --isClosure :: (SeqSeg a, Eq (SeqUnit a))=> a -> Bool
    isClosure = isIn (seqHead seed)

    --getSeq :: (SeqSeg a, Eq (SeqUnit a))=> SeqUnit a -> [a] -> Maybe ([a])
    getSeq _ [] = Nothing
    getSeq ref rs = case getNext ref rs of
      Just (newRef, x, list)
        | null list -> if isClosure x
                       then Just [x]
                       else Nothing
        | otherwise -> do
          prox   <- getSeq newRef list
          return (x:prox)
      _             -> Nothing
      
    --getNext :: (SeqSeg a, Eq (SeqUnit a))=> SeqUnit a -> [a] -> Maybe (a, [a])
    getNext ref ss = case break (isIn ref) ss of
      ([], [])   -> Nothing
      (_ , [])   -> Nothing
      (as, b:bs) -> do
        (next, rest) <- nextRef ref b
        return (next, rest, as ++ bs)

-- | Classify a list segments in @OpenSeq@ and @LoopSeq@
-- Expect chunk of linear sequences. Branches and interconnections
-- might show unpredictable behavior.
getListSegs :: (SeqInv a, SeqSeg a)=> [a] -> [Seq [] a (SeqUnit a)]
getListSegs ps = let
  sl = let
    toSeq x
      | a == b    = LoopSeq [x]
      | otherwise = OpenSeq a b [x]
      where
        a = seqHead x
        b = seqTail x
    in map toSeq ps

  isIn i1 f1 (OpenSeq i2 f2 _) = i1 == i2 || i1 == f2
                              || f1 == i2 || f1 == f2
  isIn _ _ _ = False

  rev = reverse . map seqInv

  merge (OpenSeq i1 f1 sl1) (OpenSeq i2 f2 sl2)
    | i1 == f2 && i2 == f1 = LoopSeq (sl2 ++ sl1)
    | i1 == i2 && f1 == f2 = LoopSeq (sl2 ++ sl1)
    | i1 == f2 = OpenSeq i2 f1 (sl2       ++       sl1)
    | i2 == f1 = OpenSeq i1 f2 (sl1       ++       sl2)
    | i1 == i2 = OpenSeq f1 f2 ((rev sl1) ++       sl2)
    | f1 == f2 = OpenSeq i1 i2 (sl1       ++ (rev sl2))
  merge _ _ = error "[SeqSeg] Can't merge loops. It's a bug."
  
  func [] rs = rs
  func (x:xs) rs = case x of
    OpenSeq i f _ -> case break (isIn i f) xs of
      ([],[])    -> func xs (x:rs)
      (_, [])    -> func xs (x:rs)
      (as, b:bs) -> func (as ++ (merge x b):bs) rs
    LoopSeq _    -> func xs (x:rs) 
  in func sl []

splitOpenLoop :: [Seq c a u] -> ([Seq c a u], [Seq c a u])
splitOpenLoop = let
  isOpen (OpenSeq _ _ _) = True
  isOpen _               = False              
  in L.partition isOpen

getLoops :: (SeqSeg a)=> [Seq c a (SeqUnit a)] -> [c a]
getLoops = let
  foo (LoopSeq x) = x
  foo _           = error "[SeqSeg] Exepection LoopSeq. It must be a bug."
  in map foo . snd . splitOpenLoop 
    
getOneLoop :: (SeqSeg a)=> [Seq c a (SeqUnit a)] -> Maybe (c a)
getOneLoop ss = case getLoops ss of
  [s] -> return s
  _   -> Nothing