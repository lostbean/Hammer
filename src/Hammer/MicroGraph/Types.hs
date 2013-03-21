{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Hammer.MicroGraph.Types
  ( GrainID
  , FaceID
  , EdgeID
  , VertexID
    
  , mkGrainID
  , mkFaceID
  , mkVertexID
    
  , mkFaceID'
  , mkEdgeID'
  , mkVertexID'

  , mkMultiVertexID
    
  , mkMultiEdgeID'
  , mkMultiVertexID'
    
  , getAlterFaceID
  , getAlterEdgeID
  , getAlterVertexID
    
  , unGrainID
  , unFaceID
  , unEdgeID
  , unVertexID

  , MicroEdge      (..)
  , GrainProp      (..)
  , FaceProp       (..)
  , EdgeProp       (..)
  , VertexProp     (..)
  , MicroGraph     (..)
  , GrainHierarchy (..)
  , UpdateLevel    (..) 
  , HasPropValue   (..)
  , HasPropConn    (..)

  , closeSeq
  , SeqSeg (..)

  , classifySegs
  , splitOpenLoop
  , getOneLoop
  , getLoops
  , Seq (..)
    
  ) where

-- External modules
import qualified Data.List           as L
import qualified Data.IntSet         as IS
import qualified Data.Set            as S

import           Control.DeepSeq
import           Data.HashMap.Strict (HashMap)
import           Data.HashSet        (HashSet)
import           Data.Hashable       (Hashable, hashWithSalt)
import           Data.IntSet         (IntSet)
import           Data.Set            (Set)
import           Control.Monad       (liftM)
import           Data.Maybe          (isJust)

import           Debug.Trace
-- ==========================================================================================

newtype GrainID = GrainID Int deriving (Show, Eq, Ord)
                                        
data FaceID
  = FaceID
    {-# UNPACK #-} !GrainID
    {-# UNPACK #-} !GrainID
  | AlterFaceID
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !GrainID
    {-# UNPACK #-} !GrainID
  deriving (Show, Eq)
                       
data EdgeID
  = EdgeID
    !FaceID
    !FaceID
    !FaceID
  | AlterEdgeID
    {-# UNPACK #-} !Int
    !FaceID
    !FaceID
    !FaceID
  | MultiEdgeID (Set FaceID)
  | AlterMultiEdgeID {-# UNPACK #-} !Int (Set FaceID)
  deriving (Show, Eq)

data VertexID
  = VertexID
    {-# UNPACK #-} !GrainID
    {-# UNPACK #-} !GrainID
    {-# UNPACK #-} !GrainID
    {-# UNPACK #-} !GrainID
  | MultiVertexID IntSet
  | AlterVertexID {-# UNPACK #-} !Int IntSet
  deriving (Show, Eq)

mkGrainID :: Int -> GrainID 
mkGrainID = GrainID

mkFaceID :: (Int, Int) -> FaceID 
mkFaceID = let
  func (a, b) = FaceID (GrainID a) (GrainID b)
  in func . fast2DSort
    
mkVertexID :: (Int, Int, Int, Int) -> VertexID 
mkVertexID = let
  func (a, b, c, d) = VertexID (GrainID a) (GrainID b) (GrainID c) (GrainID d)
  in func . fast4DSort

mkMultiVertexID :: [Int] -> Maybe VertexID 
mkMultiVertexID = mkMultiVertexID' . map GrainID 


mkFaceID' :: (GrainID, GrainID) -> FaceID 
mkFaceID' = (\(a,b) -> FaceID  a b) . fast2DSort

mkEdgeID' :: (FaceID, FaceID, FaceID) -> EdgeID 
mkEdgeID' = (\(a,b,c) -> EdgeID a b c) . fast3DSort

mkVertexID' :: (GrainID, GrainID, GrainID, GrainID) -> VertexID 
mkVertexID' = (\(a,b,c,d) -> VertexID a b c d) . fast4DSort

mkMultiEdgeID' :: [FaceID] -> Maybe EdgeID 
mkMultiEdgeID' [a,b,c]        = return $ mkEdgeID' (a, b, c)
mkMultiEdgeID' xs@(_:_:_:_:_) = return . MultiEdgeID $ S.fromList xs
mkMultiEdgeID' _              = Nothing
    
mkMultiVertexID' :: [GrainID] -> Maybe VertexID 
mkMultiVertexID' [a,b,c,d]      = return $ mkVertexID' (a, b, c, d)
mkMultiVertexID' xs@(_:_:_:_:_) = return . MultiVertexID . IS.fromList $ map unGrainID xs
mkMultiVertexID' _              = Nothing


getAlterFaceID :: FaceID -> FaceID
getAlterFaceID (FaceID        a b) = AlterFaceID 3     a b
getAlterFaceID (AlterFaceID i a b) = AlterFaceID (i+1) a b

getAlterEdgeID :: EdgeID -> EdgeID
getAlterEdgeID (EdgeID a b c)          = AlterEdgeID 333 a b c
getAlterEdgeID (MultiEdgeID    s)      = AlterMultiEdgeID 333    s
getAlterEdgeID (AlterEdgeID is a b c)  = AlterEdgeID      (is+1) a b c
getAlterEdgeID (AlterMultiEdgeID is s) = AlterMultiEdgeID (is+1) s

getAlterVertexID :: VertexID -> VertexID
getAlterVertexID (VertexID a b c d) = let
  s = IS.fromList [unGrainID a, unGrainID b, unGrainID c, unGrainID d]
  in AlterVertexID 666 s
getAlterVertexID (MultiVertexID    s) = AlterVertexID 666    s
getAlterVertexID (AlterVertexID is s) = AlterVertexID (is+1) s
 

unGrainID :: GrainID -> Int
unGrainID (GrainID x) = x

unFaceID :: FaceID -> (Int, Int)
unFaceID (FaceID        a b) = (unGrainID a, unGrainID b) 
unFaceID (AlterFaceID _ a b) = (unGrainID a, unGrainID b) 

unEdgeID :: EdgeID -> Either (FaceID, FaceID, FaceID) (Set FaceID)
unEdgeID (EdgeID    a b c)      = Left (a, b, c)
unEdgeID (AlterEdgeID _ a b c)  = Left (a, b, c)
unEdgeID (MultiEdgeID  s)       = Right s
unEdgeID (AlterMultiEdgeID _ s) = Right s

unVertexID :: VertexID -> Either (Int, Int, Int, Int) IntSet
unVertexID (VertexID   a b c d) = Left  (unGrainID a, unGrainID b, unGrainID c, unGrainID d)
unVertexID (MultiVertexID    s) = Right s
unVertexID (AlterVertexID _  s) = Right s


instance NFData GrainID where
  rnf (GrainID x) = rnf x

instance NFData FaceID where
  rnf (FaceID        a b) = rnf a `seq` rnf b
  rnf (AlterFaceID _ a b) = rnf a `seq` rnf b

instance NFData EdgeID where
  rnf (EdgeID    a b c)      = rnf a `seq` rnf b `seq` rnf c
  rnf (MultiEdgeID   s)      = rnf s
  rnf (AlterEdgeID i a b c)  = rnf i `seq` rnf a `seq` rnf b `seq` rnf c
  rnf (AlterMultiEdgeID i s) = rnf i `seq` rnf s

instance NFData VertexID where
  rnf (VertexID  a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d
  rnf (MultiVertexID   s) = rnf s
  rnf (AlterVertexID _ s) = rnf s

instance Hashable GrainID where
  hashWithSalt i (GrainID x) = hashWithSalt i x

instance Hashable FaceID where
  hashWithSalt i (FaceID         a b) = hashWithSalt i (a,b)
  hashWithSalt i (AlterFaceID is a b) = let
    salt = i `hashWithSalt` is
    in hashWithSalt salt (a,b)

instance Hashable EdgeID where
  hashWithSalt i (EdgeID  a b c) = hashWithSalt i (a,b,c)
  hashWithSalt i (MultiEdgeID s) = let
    salt = i `hashWithSalt` (332 :: Int)
    in S.foldl' hashWithSalt salt s 
  hashWithSalt i (AlterEdgeID is a b c) = let
    salt = i `hashWithSalt` is
    in hashWithSalt salt (a,b,c) 
  hashWithSalt i (AlterMultiEdgeID is s) = let
    salt = i `hashWithSalt` is
    in S.foldl' hashWithSalt salt s 

instance Hashable VertexID where
  hashWithSalt i (VertexID a b c d) = hashWithSalt i (a,b,c,d)
  hashWithSalt i (MultiVertexID  s) = let
    salt = i `hashWithSalt` (665 :: Int)
    in IS.foldl' hashWithSalt salt s 
  hashWithSalt i (AlterVertexID is s) = let
    salt = i `hashWithSalt` is
    in IS.foldl' hashWithSalt salt s 

  
instance Ord FaceID where
  compare (AlterFaceID ai a1 a2) (AlterFaceID bi b1 b2) =
    case compare ai bi of
      EQ -> comp2DSort a1 a2 b1 b2
      x  -> x
  compare (AlterFaceID _ _ _) (FaceID _ _)        = GT 
  compare (FaceID _ _)        (AlterFaceID _ _ _) = LT
  compare (FaceID a1 a2)      (FaceID b1 b2)      = comp2DSort a1 a2 b1 b2

{-# INLINE comp2DSort #-}
comp2DSort :: (Ord a)=> a -> a -> a -> a -> Ordering
comp2DSort a1 a2 b1 b2 = let
  a = fast2DSort (a1, a2)
  b = fast2DSort (b1, b2)
  in compare a b

{-# INLINE fast4DSort #-}
fast4DSort :: (Ord a)=> (a,a,a,a) -> (a,a,a,a)
fast4DSort v@(a, b, c, d)
  | (a >= b) && (b >= c) && (c >= d) = v
  | otherwise                        = (a', b', c', d')
  where
    minab = min a b
    maxab = max a b
    mincd = min c d
    maxcd = max c d
    a'    = max maxab maxcd
    b'    = max (max minab mincd) (min maxab maxcd)
    c'    = min (max minab mincd) (min maxab maxcd)
    d'    = min minab mincd

{-# INLINE fast3DSort #-}
fast3DSort :: (Ord a)=> (a,a,a) -> (a,a,a)
fast3DSort v@(a, b, c)
  | (a >= b) && (b >= c) = v
  | otherwise            = (a', b', c')
  where
    minab = min a b
    maxab = max a b
    a'    = max maxab c
    b'    = max (min maxab c) minab
    c'    = min minab c

{-# INLINE fast2DSort #-}
fast2DSort :: (Ord a)=> (a,a) -> (a,a)
fast2DSort v@(a, b)
  | a >= b    = v
  | otherwise = (b, a)

-- Test fast sort for tuples
testFast3 :: (Ord a)=> (a,a,a) -> Bool
testFast3 t@(x,y,z) = let
  unlist [a,b,c] = (a,b,c)
  unlist _       = error "[GrainsGraph] Oops! List size different than 3" 
  func = all ((== fast3DSort t) . fast3DSort . unlist)
  in func $ L.permutations [x,y,z]

testFast4 :: (Ord a)=> (a,a,a,a) -> Bool
testFast4 t@(x,y,z,w) = let
  unlist [a,b,c,d] = (a,b,c,d)
  unlist _       = error "[GrainsGraph] Oops! List size different than 3" 
  func = all ((== fast4DSort t) . fast4DSort . unlist) 
  in func $ L.permutations [x,y,z,w]

-- ===================================== Graph data types ==================================

-- | Define the concept of edge (triple line in 3D microstructure or grain boundary in 2D)
data MicroEdge
  = DummyEdge
  | BadEdge  [VertexID]
  | HalfEdge VertexID
  | FullEdge VertexID VertexID
  deriving (Show, Eq)

data VertexProp a
  = VertexProp a
  | NullVertexProp
  deriving (Show)
                           
data EdgeProp   a
  = EdgeProp     MicroEdge a
  | NullEdgeProp MicroEdge
  deriving (Show)
                           
data FaceProp   a
  = FaceProp     (HashSet EdgeID) a
  | NullFaceProp (HashSet EdgeID)
  deriving (Show)
                           
data GrainProp  a
  = GrainProp     (HashSet FaceID) a
  | NullGrainProp (HashSet FaceID)
  deriving (Show)

-- | Define the microstructure graph with properties associated to
-- which one of the entities: 'GrainProp', 'FaceProp', 'EdgeProp' and 'VertexProp'.
data MicroGraph g f e v = MicroGraph
  { microGrains :: HashMap GrainID  (GrainProp  g)
  , microFaces  :: HashMap FaceID   (FaceProp   f)
  , microEdges  :: HashMap EdgeID   (EdgeProp   e)
  , microVertex :: HashMap VertexID (VertexProp v)
  } deriving (Show)

--  ---------------------------------- GrainHierarchy Class ---------------------------------

-- | Class defining the bottom-up relation for a microstructure graph,
-- i.e from the Level Vertex to the SubLevel Edge. The basic idea here, is
-- that a level 'l' can decomposed in one or more 'SubLevel l'.
-- EdgeID (1,2,3) -> FaceID (1,2), FaceID(2,3) and FaceID(1,3)
class (Hashable (SubLevel l), Eq (SubLevel l))=> GrainHierarchy l where
  type SubLevel     l :: *
  type SubLevelProp l :: * -> *
  -- | The function 'updateSubLevelProp' updates a 'SubLevelProp l a' based on
  -- by updating their reference to the level 'l'
  updateSubLevelProp  :: l -> Maybe (SubLevelProp l a) -> Maybe (SubLevelProp l a)
  
  {--
  -- | Auto generate connections form level l to the SubLevel l e.g. EdgeID 1 2 3 is
  -- connected to faces FaceID 1 2, Face 2 3 and Face 1 3. It doesn't work well for
  -- voxelized data due quadruple juction and triple line overlaping.
  --generateSubLevelConn :: l -> [SubLevel l]
  --}
  
--  -------------------------------- UpdateLevel Class --------------------------------------

class UpdateLevel l where
  updateLevelProp  :: a -> (a -> a -> a) -> Maybe (l a) -> Maybe (l a)

--  -------------------------------- HasPropValue Class -------------------------------------

class HasPropValue prop where
  getPropValue       :: prop v -> Maybe v

--  -------------------------------- HasPropConn Class --------------------------------------

class (HasPropValue prop)=> HasPropConn prop where
  type PropConn prop :: *
  getPropConn        :: prop v -> Maybe (PropConn prop)
  
  getPropBoth        :: prop v -> Maybe (v, PropConn prop)
  getPropBoth p = do
    v <- getPropValue p
    c <- getPropConn  p
    return (v, c)

-- ====================================== Close Seqeunce ====================================

closeSeq :: (SeqSeg a, Eq (SeqUnit a))=>[a] -> Maybe [a]
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
        
class SeqSeg a where
  type SeqUnit a
  seqHead :: (Eq (SeqUnit a))=> a -> SeqUnit a
  seqTail :: (Eq (SeqUnit a))=> a -> SeqUnit a
  seqInv  :: a -> a

instance SeqSeg (a, a) where
  type SeqUnit (a, a) = a
  seqHead = fst
  seqTail = snd
  seqInv (a,b) = (b,a) 

instance (Show (SeqUnit a), Show a, SeqSeg a)=> Show (Seq a) where
  show (OpenSeq a b s) = "OpenSeq " ++ show (a,b) ++ " " ++ show s
  show (LoopSeq s)     = "LoopSeq " ++ show s
  
data (SeqSeg a)=> Seq a
  = OpenSeq
    { seqIni  :: !(SeqUnit a)
    , seqEnd  :: !(SeqUnit a)
    , seqList :: [a]
    }
  | LoopSeq { loopList :: [a] }

-- | Classify a list segments in @OpenSeq@ and @LoopSeq@
-- Expect chunk of linear sequences. Branches and interconnections
-- might show unpredictable behavior.
classifySegs :: (Eq (SeqUnit a), SeqSeg a)=> [a] -> [Seq a]
classifySegs ps = let
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

splitOpenLoop :: (SeqSeg a)=> [Seq a] -> ([Seq a], [Seq a])
splitOpenLoop = let
  isOpen (OpenSeq _ _ _) = True
  isOpen _               = False              
  in L.partition isOpen

getLoops :: (Eq (SeqUnit a), SeqSeg a)=> [a] -> [[a]]
getLoops = let
  foo (LoopSeq x) = x
  foo _           = error "[SeqSeg] Exepection LoopSeq. It must be a bug."
  in map foo . fst . splitOpenLoop . classifySegs 
    
getOneLoop :: (Eq (SeqUnit a), SeqSeg a)=> [a] -> Maybe [a]
getOneLoop ss = case getLoops ss of
  [s] -> return s
  _   -> Nothing