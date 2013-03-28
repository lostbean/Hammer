{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Hammer.VoxBox.MicroVoxel
  ( MicroVoxel
  , getMicroVoxel
  , groupFaces
  , FacePos (..)
  , groupEdges
  ) where

import qualified Data.IntSet           as IS
import qualified Data.Set              as S
import qualified Data.HashMap.Strict   as HM
import qualified Data.HashSet          as HS
import qualified Data.Vector           as V


import           Data.IntSet           (IntSet)
import           Data.Hashable         (Hashable, hashWithSalt)
import           Data.HashMap.Strict   (HashMap)
import           Data.HashSet          (HashSet)
import           Data.Maybe            (isJust, catMaybes, mapMaybe)
import           Data.Vector           (Vector)

import           Control.DeepSeq
import           Control.Applicative
import           Data.List

import           Hammer.MicroGraph
import           Hammer.VoxBox.Base
import           Hammer.VoxBox.VoxConnFinder

import           Debug.Trace

-- ================================ Find Microstruture Graph ================================

type MicroVoxel = MicroGraph
                  (Vector VoxelPos)
                  (Vector FaceVoxelPos)
                  (Vector EdgeVoxelPos)
                  VoxelPos

data VoxElems = VoxElems
              { faceElems   :: [(FaceVoxelPos, FaceID)]
              , edgeElems   :: [(EdgeVoxelPos, EdgeID)]
              , vertexElems :: [(VoxelPos, VertexID)]
              } deriving (Show)

getMicroVoxel :: VoxBox Int -> (MicroVoxel, VoxBox GrainID)
getMicroVoxel vbox = case grainFinder vbox of
  Just (vboxGID, grainSet) -> let
    vbr       = dimension vbox
    allPos    = scanMicro vbr
    initElems = VoxElems [] [] []
    elems     = foldl' (getInterfaces vboxGID) initElems allPos 
  
    faceSet = groupFaces vbr (faceElems elems)
    edgeSet = groupEdges vbr (edgeElems elems)

    hmf = HM.map (FaceProp HS.empty . V.map toFaceVoxelPos) (snd faceSet)
    hme = HM.map (EdgeProp DummyEdge . V.map toEdgeVoxelPos) (snd edgeSet)
    func (k,v) = (mkGrainID k, GrainProp HS.empty v)
    hmg = HM.fromList . map func $ HM.toList grainSet

    initMicro  = MicroGraph hmg hmf hme HM.empty
    initMicro2 = foldl' (flip insertFaceConn) initMicro (HM.keys $ snd faceSet)
    foo = buildMicroVoxel edgeSet faceSet
    micro = foldl' foo initMicro2 (vertexElems elems)
    in (micro, vboxGID)


buildMicroVoxel :: EdgeSet -> FaceSet -> MicroVoxel -> (VoxelPos, VertexID) -> MicroVoxel
buildMicroVoxel eset fset mv (p, pid) = let
  getRef (VoxelPos x y z) = VoxelPos (x*2-1) (y*2-1) (z*2-1)
  pref = getRef p
  es = mapMaybe func [eXp, eXm, eYp, eYm, eZp, eZm]
  func x = HM.lookup (EdgePos x) (fst eset)
  eXp = pref #+# xDir
  eXm = pref #-# xDir
  eYp = pref #+# yDir
  eYm = pref #-# yDir
  eZp = pref #+# zDir
  eZm = pref #-# zDir

  foo x = HM.lookup (FacePos x) (fst fset)
  fXYpp = pref #+# getXYplus
  fXYpm = pref #+# getXYminus
  fXYmm = pref #-# getXYplus
  fXYmp = pref #-# getXYminus
  fXZpp = pref #+# getXZplus
  fXZpm = pref #+# getXZminus
  fXZmm = pref #-# getXZplus
  fXZmp = pref #-# getXZminus
  fYZpp = pref #+# getYZplus
  fYZpm = pref #+# getYZminus
  fYZmm = pref #-# getYZplus
  fYZmp = pref #-# getYZminus

  fs = mapMaybe foo [ fXYpp, fXYpm, fXYmp, fXYmm
                    , fXZpp, fXZpm, fXZmp, fXZmm
                    , fYZpp, fYZpm, fYZmp, fYZmm ]

  eXpF = mapMaybe foo [fXYpp, fXYpm, fXZpp, fXZpm]
  eXmF = mapMaybe foo [fXYmp, fXYmm, fXZmp, fXZmm]
  
  eYpF = mapMaybe foo [fXYpp, fXYmp, fYZpp, fYZpm]
  eYmF = mapMaybe foo [fXYpm, fXYmm, fYZmp, fYZmm]

  eZpF = mapMaybe foo [fXZpp, fXZmp, fYZpp, fYZmp]
  eZmF = mapMaybe foo [fXZpm, fXZmm, fYZpm, fYZmm]

  fau acc (Just eid, fs) = insertEdgeConn eid fs acc
  fau acc _              = acc

  mv' = foldl' fau mv [ (func eXp, eXpF), (func eXm, eXmF)
                      , (func eYp, eYpF), (func eYm, eYmF)
                      , (func eZp, eZpF), (func eZm, eZmF)]

  sameFace a b = isFaceConn a b undefined
  ee = partition ((== 1) . length) $ myGroup fs

  myGroup [] = []
  myGroup  (x:xs) = let
    (g, r) = partition (\a -> not (a == x) && sameFace a x) xs
    in (x:g):(myGroup r)

  funcDBG
    | (not . null . snd) ee = traceShow ee
    | otherwise = id
                  
  in funcDBG $ insertNewVertex const pid p es mv'
  
scanMicro :: VoxBoxRange -> [VoxelPos]
scanMicro VoxBoxRange{..} = let
  (xmax, ymax, zmax) = getVoxBoxDim vbrDim
  in [VoxelPos x y z | z <- [0..zmax-1], y <- [0..ymax-1], x <- [0..xmax-1]]

getInterfaces :: VoxBox GrainID -> VoxElems -> VoxelPos -> VoxElems
getInterfaces vbox@VoxBox{..} VoxElems{..} pos = let
  
  v    = pos
  vx   = pos #+# (VoxelPos (-1)   0    0 )
  vy   = pos #+# (VoxelPos   0  (-1)   0 )
  vz   = pos #+# (VoxelPos   0    0  (-1))
  vxy  = pos #+# (VoxelPos (-1) (-1)   0 )
  vyz  = pos #+# (VoxelPos   0  (-1) (-1))
  vzx  = pos #+# (VoxelPos (-1)   0  (-1))
  vxyz = pos #+# (VoxelPos (-1) (-1) (-1))

  p    = vbox#!v
  px   = vbox#!vx
  py   = vbox#!vy
  pz   = vbox#!vz
  pxy  = vbox#!vxy
  pyz  = vbox#!vyz
  pzx  = vbox#!vzx
  pxyz = vbox#!vxyz

  fx = checkFace p px
  fy = checkFace p py
  fz = checkFace p pz

  ex = checkEgde p py pyz pz
  ey = checkEgde p pz pzx px
  ez = checkEgde p py pxy px

  emx = checkEgde px pxy pxyz pzx
  emy = checkEgde py pyz pxyz pxy
  emz = checkEgde pz pyz pxyz pzx
  
  validEdges = catMaybes [ex, ey, ez, emx, emy, emz]
  validNeigh = catMaybes [p, px, py, pz, pxy, pyz, pzx, pxyz]
  
  vertex = checkVertex validNeigh validEdges

  foo (Just v, f) = return (f pos, v)
  foo _           = Nothing

  fooFst (Just v, f) = return (f pos, fst v)
  fooFst _           = Nothing
 
  fs = mapMaybe foo    [(fx, Fx), (fy, Fy), (fz, Fz)]
  es = mapMaybe fooFst [(ex, Ex), (ey, Ey), (ez, Ez)]
  new_vs = case vertex of
    Just (vid, _) -> (pos, vid) : vertexElems
    _             -> vertexElems
  
  in VoxElems { faceElems   = fs ++ faceElems
              , edgeElems   = es ++ edgeElems
              , vertexElems = new_vs }

  


dbg a = trace ("::::::>> " ++ show a) a

checkFace :: Maybe GrainID -> Maybe GrainID -> Maybe FaceID
checkFace (Just a) (Just b)
  | a /= b    = return $ mkFaceID' (a, b)
  | otherwise = Nothing
checkFace _ _ = Nothing

-- | WARNING: In this function the sequence of the arguments matters!!! 
-- Insert in the following sequence: A -> B -> C -> D where A* is the
-- current @VoxelPos@.
--
--   D | A*
--   -----
--   C | B
-- 
-- The function will scan the faces: AB, BC, DC and DA
checkEgde :: Maybe GrainID -> Maybe GrainID -> Maybe GrainID
          -> Maybe GrainID -> Maybe (EdgeID, [FaceID]) 
checkEgde pa pb pc pd
  | not (isJust pa)     = Nothing
  | nun == 4 && nf == 4 = out
  | nun == 3 && nf == 3 = out
  | otherwise           = Nothing
  where
    out = do
      newE <- mkMultiEdgeID' fs
      return (newE, fs)
    nun = length uns
    nf  = length fs
    uns = nub ns       -- list of unique neighbours
    ns = catMaybes [pa, pb, pc, pd]
    fs = catMaybes [ab, bc, cd, da]
    ab = checkFace pa pb
    bc = checkFace pb pc
    cd = checkFace pc pd
    da = checkFace pd pa
    
checkVertex :: [GrainID] -> [(EdgeID, a)] -> Maybe (VertexID, [EdgeID])
checkVertex ns es
  | nun <= 3             = Nothing
  | ne  >= 3 && nue >= 3 = getV
  -- | ne  >= 3 = getV
  | otherwise            = Nothing
  where
    ne   = length es
    nun  = length uns
    nue  = length ues
    -- list of unique neighbours
    uns  = nub ns 
    -- list of unique edges (avoid create vertex for touching edges)
    ues  = nubBy (\a b -> fst a == fst b) es
    getV = do
      newV <- mkMultiVertexID' uns
      return (newV, map fst es) 

    
-- -------------------------- Scan Faces ---------------------------------------

type FaceSet = (HashMap FacePos FaceID, HashMap FaceID (Vector FacePos))

groupFaces :: VoxBoxRange -> [(FaceVoxelPos, FaceID)] -> FaceSet
groupFaces vbr fs = let
  hmf   :: HashMap VoxelPos FaceID
  hmf   = HM.fromList $ map (\(k,v) -> (toFacePos k, v)) fs
  
  faces :: VoxConn (HashMap VoxelPos) FacePos
  faces = finderVoxConn hmf vbrf 

  foo acc@(accPOS, accFID) k v = case HM.lookup (vbrf %# k) hmf of
    Just fid -> let
      newAccPOS = V.foldl' (\acc p -> HM.insert p finalFID acc) accPOS v
      (finalFID, newAccFID) = insertUniqueHM v fid getAlterFaceID accFID
      in (newAccPOS, newAccFID)
    _        -> acc
      
  vbrf    = getExtVBR vbr
  zeroSet = (HM.empty, HM.empty)
  -- (HashMap Int (Vector grid))
  in case voxConnList faces of
    Just x -> HM.foldlWithKey' foo zeroSet  x
    _      -> zeroSet

getExtVBR :: VoxBoxRange -> VoxBoxRange
getExtVBR vbr = let
  (dx, dy, dz) = getVoxBoxDim $ vbrDim vbr
  in vbr {vbrDim = VoxBoxDim (dx*2-1) (dy*2-1) (dz*2-1)}

toFacePos :: FaceVoxelPos -> VoxelPos 
toFacePos f = let
  foo (VoxelPos x y z) = VoxelPos (x*2) (y*2) (z*2)
  in case f of
    Fx p -> foo p #-# VoxelPos 1 0 0
    Fy p -> foo p #-# VoxelPos 0 1 0
    Fz p -> foo p #-# VoxelPos 0 0 1

toFaceVoxelPos :: FacePos -> FaceVoxelPos
toFaceVoxelPos f@(FacePos (VoxelPos x y z))
  | isFaceX f = Fx $ p #+# VoxelPos 1 0 0
  | isFaceY f = Fy $ p #+# VoxelPos 0 1 0
  | isFaceZ f = Fz $ p #+# VoxelPos 0 0 1
  | otherwise = error "Can't convert to FaceVoxelPos."
  where p = VoxelPos (div x 2) (div y 2) (div z 2)

newtype FacePos = FacePos VoxelPos deriving (Eq)

instance Hashable FacePos where
  hashWithSalt i (FacePos x) = hashWithSalt i x

instance NFData FacePos where
  rnf (FacePos p) = rnf p

instance Show FacePos where
  show (FacePos (VoxelPos x y z)) = "FacePos " ++ show x ++ " " ++ show y ++ " " ++ show z

isFace :: FacePos -> Bool
isFace (FacePos (VoxelPos x y z)) = let
  oz = odd z
  ex = even x
  ey = even y
  in (oz && (ex && ey)) ||
     (not oz && ((ex && not ey) || (not ex && ey)))  

isFaceX :: FacePos -> Bool
isFaceX (FacePos (VoxelPos x y z)) = odd x && even y && even z

isFaceY :: FacePos -> Bool
isFaceY (FacePos (VoxelPos x y z)) = even x && odd y && even z

isFaceZ :: FacePos -> Bool
isFaceZ (FacePos (VoxelPos x y z)) = even x && even y && odd z


instance GridConn FacePos where
  toGridType             = FacePos
  toVoxelPos (FacePos x) = x
  getConnPos             = getConnFacePos
  getCrossConnPos        = getCrossConnFacePos

-- | Trick function 
getConnFacePos :: CartesianDir -> FacePos -> Maybe (FacePos, FacePos)
getConnFacePos dir p@(FacePos vp) = let
  add2p = FacePos . (vp #+#)
  px = add2p $ VoxelPos 1 0 0
  py = add2p $ VoxelPos 0 1 0
  pz = add2p $ VoxelPos 0 0 1
  in case dir of
    XDir
      | (isFaceY p  || isFaceZ p ) -> return $ (p, add2p $ VoxelPos 2 0 0) 
      | (isFaceY px || isFaceZ px) -> return $ (add2p $ VoxelPos (-1) 0 0, px)
      | otherwise -> Nothing
    YDir
      | (isFaceX p  || isFaceZ p ) -> return $ (p, add2p $ VoxelPos 0 2 0)
      | (isFaceX py || isFaceZ py) -> return $ (add2p $ VoxelPos 0 (-1) 0, py)
      | otherwise -> Nothing
    ZDir
      | (isFaceX p  || isFaceY p ) -> return $ (p, add2p $ VoxelPos 0 0 2)
      | (isFaceX pz || isFaceY pz) -> return $ (add2p $ VoxelPos 0 0 (-1), pz)
      | otherwise -> Nothing
    _    -> Nothing

getCrossConnFacePos :: CartesianDir -> FacePos -> Vector (FacePos, CrossDir)
getCrossConnFacePos dir p@(FacePos vp) = let 
  foo d = let
    func = FacePos . (vp #+#) . getDeltaPos
    in (func d, d)
  cross = V.fromList $ case dir of
    XDir
      | isFaceX p -> [XYminus, XYplus, XZminus, XZplus]
      | isFaceY p -> [XYminus, XYplus]
      | isFaceZ p -> [XZminus, XZplus]
      | otherwise -> []
    YDir
      | isFaceX p -> [YXminus, YXplus]
      | isFaceY p -> [YXminus, YXplus, YZminus, YZplus]
      | isFaceZ p -> [YZminus, YZplus]
      | otherwise -> []
    ZDir
      | isFaceX p -> [ZXminus, ZXplus]
      | isFaceY p -> [ZYminus, ZYplus]
      | isFaceZ p -> [ZYminus, ZYplus, ZXminus, ZXplus]
      | otherwise -> []
  in V.map foo cross

instance HasConn FaceID where
  isConn      = isConnBase isFaceConn
  isCrossConn = isConnBase isFaceConn
  
isConnBase :: (a -> a -> b -> Bool) -> Maybe a -> Maybe a -> b -> Bool
isConnBase f (Just a)  (Just b)  c = f a b c
isConnBase _ _         _         _ = False

isFaceConn :: FaceID -> FaceID -> a -> Bool
isFaceConn a b _ = unFaceID a == unFaceID b


-- -------------------------- Scan Edges ---------------------------------------

type EdgeSet = (HashMap EdgePos EdgeID, HashMap EdgeID (Vector EdgePos))

groupEdges :: VoxBoxRange -> [(EdgeVoxelPos, EdgeID)] -> EdgeSet
groupEdges vbr fs = let
  hmf   :: HashMap VoxelPos EdgeID
  hmf   = HM.fromList $ map (\(k,v) -> (toEdgePos k, v)) fs
  
  faces :: VoxConn (HashMap VoxelPos) EdgePos
  faces = finderVoxConn hmf vbrf 

  foo acc@(accPOS, accEID) k v = case HM.lookup (vbrf %# k) hmf of
    Just fid -> let
      newAccPOS = V.foldl' (\acc p -> HM.insert p finalFID acc) accPOS v
      (finalFID, newAccEID) = insertUniqueHM v fid getAlterEdgeID accEID
      in (newAccPOS, newAccEID)
    _        -> acc
      
  vbrf    = getExtVBR vbr
  zeroSet = (HM.empty, HM.empty)
  -- (HashMap Int (Vector grid))
  in case voxConnList faces of
    Just x -> HM.foldlWithKey' foo zeroSet  x
    _      -> zeroSet

toEdgePos :: EdgeVoxelPos -> VoxelPos 
toEdgePos f = let
  foo (VoxelPos x y z) = VoxelPos (x*2) (y*2) (z*2)
  in case f of
    Ex p -> foo p #-# VoxelPos 0 1 1
    Ey p -> foo p #-# VoxelPos 1 0 1
    Ez p -> foo p #-# VoxelPos 1 1 0

toEdgeVoxelPos :: EdgePos -> EdgeVoxelPos
toEdgeVoxelPos f@(EdgePos (VoxelPos x y z))
  | isEdgeX f = Ex $ p #+# VoxelPos 0 1 1
  | isEdgeY f = Ey $ p #+# VoxelPos 1 0 1
  | isEdgeZ f = Ez $ p #+# VoxelPos 1 1 0
  | otherwise = error "Can't convert to FaceVoxelPos."
  where p = VoxelPos (div x 2) (div y 2) (div z 2)



newtype EdgePos = EdgePos VoxelPos deriving (Eq)

instance Hashable EdgePos where
  hashWithSalt i (EdgePos x) = hashWithSalt i x

instance NFData EdgePos where
  rnf (EdgePos p) = rnf p

instance Show EdgePos where
  show (EdgePos (VoxelPos x y z)) = "EdgePos " ++ show x ++ " " ++ show y ++ " " ++ show z

isEdge :: EdgePos -> Bool
isEdge (EdgePos (VoxelPos x y z)) = let
  ez = even z
  ox = odd x
  oy = odd y
  in (ez && (ox && oy)) ||
     (not ez && ((ox && not oy) || (not ox && oy)))  

isEdgeX :: EdgePos -> Bool
isEdgeX (EdgePos (VoxelPos x y z)) = even x && odd y && odd z

isEdgeY :: EdgePos -> Bool
isEdgeY (EdgePos (VoxelPos x y z)) = odd x && even y && odd z

isEdgeZ :: EdgePos -> Bool
isEdgeZ (EdgePos (VoxelPos x y z)) = odd x && odd y && even z


instance GridConn EdgePos where
  toGridType             = EdgePos
  toVoxelPos (EdgePos x) = x
  getConnPos             = getConnEdgePos
  getCrossConnPos        = getCrossConnEdgePos

-- | Trick function 
getConnEdgePos :: CartesianDir -> EdgePos -> Maybe (EdgePos, EdgePos)
getConnEdgePos dir p@(EdgePos vp) = let
  add2p = EdgePos . (vp #+#)
  px = add2p $ VoxelPos 1 0 0
  py = add2p $ VoxelPos 0 1 0
  pz = add2p $ VoxelPos 0 0 1
  in case dir of
    XDir
      | isEdgeX p  -> return $ (p, add2p $ VoxelPos 2 0 0) 
      | isEdgeX px -> return $ (add2p $ VoxelPos (-1) 0 0, px)
      | otherwise  -> Nothing
    YDir
      | isEdgeY p  -> return $ (p, add2p $ VoxelPos 0 2 0)
      | isEdgeY py -> return $ (add2p $ VoxelPos 0 (-1) 0, py)
      | otherwise  -> Nothing
    ZDir
      | isEdgeZ p  -> return $ (p, add2p $ VoxelPos 0 0 2)
      | isEdgeZ pz -> return $ (add2p $ VoxelPos 0 0 (-1), pz)
      | otherwise  -> Nothing
    _    -> Nothing

getCrossConnEdgePos :: CartesianDir -> EdgePos -> Vector (EdgePos, CrossDir)
getCrossConnEdgePos dir p@(EdgePos vp) = let 
  foo d = let
    func = EdgePos . (vp #+#) . getDeltaPos
    in (func d, d)
  cross = V.fromList $ case dir of
    XDir
      | isEdgeX p -> [XYminus, XYplus, XZminus, XZplus]
      | isEdgeY p -> [XYminus, XYplus]
      | isEdgeZ p -> [XZminus, XZplus]
      | otherwise -> []
    YDir
      | isEdgeX p -> [YXminus, YXplus]
      | isEdgeY p -> [YXminus, YXplus, YZminus, YZplus]
      | isEdgeZ p -> [YZminus, YZplus]
      | otherwise -> []
    ZDir
      | isEdgeX p -> [ZXminus, ZXplus]
      | isEdgeY p -> [ZYminus, ZYplus]
      | isEdgeZ p -> [ZYminus, ZYplus, ZXminus, ZXplus]
      | otherwise -> []
  in V.map foo cross

instance HasConn EdgeID where
  isConn      = isConnBase isEdgeConn
  isCrossConn = isConnBase isEdgeConn

isEdgeConn :: EdgeID -> EdgeID -> a -> Bool
isEdgeConn a b _ = let
  foo (Left a)  (Left b)  = a == b
  foo (Right a) (Right b) = a == b
  foo _          _        = False
  in foo (unEdgeID a) (unEdgeID b)


