{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hammer.VoxBox.MicroVoxel
       ( MicroVoxel
       , getMicroVoxel
       , benchmarkMicroVoxel
       ) where

import qualified Data.HashMap.Strict         as HM
import qualified Data.HashSet                as HS
import qualified Data.Vector                 as V
import qualified Hammer.Math.SparseMatrix    as SP

import           Data.HashMap.Strict      (HashMap)
import           Data.Maybe               (catMaybes, mapMaybe)
import           Data.Vector              (Vector)
import           Hammer.Math.SparseMatrix (Sparse3)

import           Data.List
import           Criterion

import           Hammer.MicroGraph
import           Hammer.VoxBox.Base
import           Hammer.VoxBox.VoxConnFinder

import           Debug.Trace
--dbg a = trace ("::::::>> " ++ show a) a

-- ================================ Find Microstruture Graph =============================

type MicroVoxel = MicroGraph
                  (Vector VoxelPos)
                  (Vector FaceVoxelPos)
                  (Vector EdgeVoxelPos)
                  VoxelPos

type FaceElems   = [(FaceVoxelPos, FaceID)]
type EdgeElems   = [(EdgeVoxelPos, EdgeID)]
type VertexElems = [(VoxelPos, VertexID)]

getMicroVoxel :: (VoxBox GrainID, HashMap Int (Vector VoxelPos)) -> (MicroVoxel, VoxBox GrainID)
getMicroVoxel (vboxGID, grainSet) = let
  vbr     = dimension vboxGID
  allPosV = V.fromList $ getRangePos vbr

  felems  = getFaces vboxGID allPosV
  faceSet = groupFaces vbr felems

  eelems  = getEdges faceSet allPosV
  edgeSet = groupEdges vbr eelems

  velems  = getVertex vboxGID edgeSet allPosV

  func (k,v) = (mkGrainID k, GrainProp HS.empty v)
  hmf = HM.map (FaceProp HS.empty . V.map toFaceVoxelPos) (snd faceSet)
  hme = HM.map (EdgeProp DummyEdge . V.map toEdgeVoxelPos) (snd edgeSet)
  hmg = HM.fromList . map func $ HM.toList grainSet

  initMicro  = MicroGraph hmg hmf hme HM.empty
  initMicro2 = foldl' (flip insertFaceConn) initMicro (HM.keys $ snd faceSet)
  foo   = buildMicroVoxel edgeSet faceSet
  micro = foldl' foo initMicro2 velems
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
  fXYpp = pref #+# deltaXYplus
  fXYpm = pref #+# deltaXYminus
  fXYmm = pref #-# deltaXYplus
  fXYmp = pref #-# deltaXYminus
  fXZpp = pref #+# deltaXZplus
  fXZpm = pref #+# deltaXZminus
  fXZmm = pref #-# deltaXZplus
  fXZmp = pref #-# deltaXZminus
  fYZpp = pref #+# deltaYZplus
  fYZpm = pref #+# deltaYZminus
  fYZmm = pref #-# deltaYZplus
  fYZmp = pref #-# deltaYZminus

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

  in insertNewVertex pid p es mv'

getFaces :: VoxBox GrainID -> V.Vector VoxelPos -> FaceElems
getFaces vbox@VoxBox{..} = V.foldl' go []
  where
  go acc pos = let
    v    = pos
    vx   = pos #+# (VoxelPos (-1)   0    0 )
    vy   = pos #+# (VoxelPos   0  (-1)   0 )
    vz   = pos #+# (VoxelPos   0    0  (-1))
    p    = vbox#!v
    px   = vbox#!vx
    py   = vbox#!vy
    pz   = vbox#!vz
    fx = checkFace p px
    fy = checkFace p py
    fz = checkFace p pz

    foo (Just f, func) = return (func pos, f)
    foo _              = Nothing

    fs = mapMaybe foo    [(fx, Fx), (fy, Fy), (fz, Fz)]
    in fs ++ acc

getEdges :: FaceSet -> V.Vector VoxelPos -> EdgeElems
getEdges (hmFID, _) = V.foldl' go []
  where
  go acc pos = let
    v    = pos
    vx   = pos #+# (VoxelPos (-1)   0    0 )
    vy   = pos #+# (VoxelPos   0  (-1)   0 )
    vz   = pos #+# (VoxelPos   0    0  (-1))

    foo x = HM.lookup (FacePos $ toFacePos x) hmFID

    fx   = foo $ Fx v
    fxy  = foo $ Fx vy
    fxz  = foo $ Fx vz

    fy   = foo $ Fy v
    fyz  = foo $ Fy vz
    fyx  = foo $ Fy vx

    fz   = foo $ Fz v
    fzx  = foo $ Fz vx
    fzy  = foo $ Fz vy

    ex = checkEgde fy fyz fz fzy
    ey = checkEgde fx fxz fz fzx
    ez = checkEgde fx fxy fy fyx

    fooFst (Just e, func) = return (func pos, fst e)
    fooFst _              = Nothing

    es = mapMaybe fooFst [(ex, Ex), (ey, Ey), (ez, Ez)]

    in es ++ acc

getVertex :: VoxBox GrainID -> EdgeSet -> V.Vector VoxelPos -> VertexElems
getVertex vbox@VoxBox{..} (hmEID, _) = let
  go acc pos = let
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

    foo x = HM.lookup (EdgePos $ toEdgePos x) hmEID

    ex  = foo $ Ex v
    exm = foo $ Ex vx
    ey  = foo $ Ey v
    eym = foo $ Ey vy
    ez  = foo $ Ez v
    ezm = foo $ Ez vz

    validEdges = catMaybes [ex, ey, ez, exm, eym, ezm]
    validNeigh = catMaybes [p, px, py, pz, pxy, pyz, pzx, pxyz]

    in case checkVertex validNeigh validEdges of
      Just (vid, _) -> (pos, vid) : acc
      _             -> acc
  in V.foldl go []


checkFace :: Maybe GrainID -> Maybe GrainID -> Maybe FaceID
checkFace (Just a) (Just b)
  | a /= b    = return $ mkFaceID' (a, b)
  | otherwise = Nothing
checkFace _ _ = Nothing

-- The function will scan the faces: AB, BC, DC and DA
checkEgde :: Maybe FaceID -> Maybe FaceID -> Maybe FaceID
          -> Maybe FaceID -> Maybe (EdgeID, [FaceID])
checkEgde fa fb fc fd
  | nfc >= 3 && nuf >= 3 = out
  | otherwise            = Nothing
  where
    out = do
      newE <- mkMultiEdgeID' fs
      return (newE, ufs)
    nfc = length fs
    nuf = length ufs
    ufs = nub fs -- list of unique faces
    fs  = catMaybes [fa, fb, fc, fd]

checkVertex :: [GrainID] -> [EdgeID] -> Maybe (VertexID, [EdgeID])
checkVertex ns es
  | nun <= 3             = Nothing
  | ne  >= 3 && nue >= 3 = getV
  | otherwise            = Nothing
  where
    ne   = length es
    nun  = length uns
    nue  = length ues
    -- list of unique neighbours
    uns  = nub ns
    -- list of unique edges (avoid create vertex for touching edges)
    ues  = nub es
    getV = do
      newV <- mkMultiVertexID' uns
      return (newV, ues)

-- -------------------------- commun scan tools ---------------------------------------

getExtVBR :: VoxBoxRange -> VoxBoxRange
getExtVBR vbr = let
  (dx, dy, dz) = getVoxBoxDim $ vbrDim vbr
  in vbr {vbrDim = VoxBoxDim (dx*2-1) (dy*2-1) (dz*2-1)}

isConnBase :: (a -> a -> b -> Bool) -> Maybe a -> Maybe a -> b -> Bool
isConnBase f (Just a)  (Just b)  c = f a b c
isConnBase _ _         _         _ = False

-- -------------------------- Scan Faces ---------------------------------------

type FaceSet = (HashMap FacePos FaceID, HashMap FaceID (Vector FacePos))

fp2spIn :: FaceVoxelPos -> (Int, Int, Int)
fp2spIn = vp2spIn . toFacePos

vp2spIn :: VoxelPos -> (Int, Int, Int)
vp2spIn (VoxelPos x y z) = (x,y,z)

groupFacesSP :: VoxBoxRange -> [(FaceVoxelPos, FaceID)] -> FaceSet
groupFacesSP vbr fs = let
  (so, sd) = let
    VoxelPos  x0 y0 z0 = vbrOrigin vbr
    VoxBoxDim dx dy dz = vbrDim    vbr
    in (SP.Sparse3Org (x0,y0,z0), SP.Sparse3Dim (dx,dy,dz))

  hmf :: Sparse3 FaceID
  hmf = SP.mkSparse3 so sd . V.map (\(!k, !v) -> (fp2spIn k, v)) $ V.fromList fs

  faces :: VoxConn Sparse3 FacePos
  faces = finderVoxConn hmf vbrf

  foo acc@(accPOS, accFID) k v = case SP.lookup (vp2spIn $ vbrf %# k) hmf of
    Just fid -> let
      newAccPOS = V.foldl' (\acu p -> HM.insert p finalFID acu) accPOS v
      (finalFID, newAccFID) = insertUniqueHM v fid getAlterFaceID accFID
      in (newAccPOS, newAccFID)
    _        -> acc

  vbrf    = getExtVBR vbr
  zeroSet = (HM.empty, HM.empty)
  -- (HashMap Int (Vector grid))
  in case voxConnList faces of
    Just x -> HM.foldlWithKey' foo zeroSet  x
    _      -> zeroSet


groupFaces :: VoxBoxRange -> [(FaceVoxelPos, FaceID)] -> FaceSet
groupFaces vbr fs = let
  hmf :: HashMap VoxelPos FaceID
  hmf = HM.fromList $ map (\(!k, !v) -> let f = toFacePos k in f `seq` (f , v)) fs

  faces :: VoxConn (HashMap VoxelPos) FacePos
  faces = finderVoxConn hmf vbrf

  foo acc@(accPOS, accFID) k v = case HM.lookup (vbrf %# k) hmf of
    Just fid -> let
      newAccPOS = V.foldl' (\acu p -> HM.insert p finalFID acu) accPOS v
      (finalFID, newAccFID) = insertUniqueHM v fid getAlterFaceID accFID
      in (newAccPOS, newAccFID)
    _        -> acc

  vbrf    = getExtVBR vbr
  zeroSet = (HM.empty, HM.empty)
  -- (HashMap Int (Vector grid))
  in case voxConnList faces of
    Just x -> HM.foldlWithKey' foo zeroSet  x
    _      -> zeroSet


instance GridConn FacePos where
  toGridType             = FacePos
  toVoxelPos (FacePos x) = x
  getConnPos             = getConnFacePos
  getCrossConnPos        = getCrossConnFacePos
  {-# INLINE toGridType #-}
  {-# INLINE toVoxelPos #-}
  {-# INLINE getConnPos #-}
  {-# INLINE getCrossConnPos #-}

instance HasConn FaceID where
  isConn      = isConnBase isFaceConn
  isCrossConn = isConnBase isFaceConn
  {-# INLINE isConn #-}
  {-# INLINE isCrossConn #-}

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

getCrossConnFacePos :: CartesianDir -> FacePos -> V.Vector (FacePos, CrossDir)
getCrossConnFacePos dir p@(FacePos vp) = V.map (\(a, b) -> (FacePos a, b)) $ case dir of
  XDir
    | isFaceX p -> V.fromList [getXYminus vp, getXYplus vp, getXZminus vp, getXZplus vp]
    | isFaceY p -> V.fromList [getXYminus vp, getXYplus vp]
    | isFaceZ p -> V.fromList [getXZminus vp, getXZplus vp]
    | otherwise -> V.empty
  YDir
    | isFaceX p -> V.fromList [getYXminus vp, getYXplus vp]
    | isFaceY p -> V.fromList [getYXminus vp, getYXplus vp, getYZminus vp, getYZplus vp]
    | isFaceZ p -> V.fromList [getYZminus vp, getYZplus vp]
    | otherwise -> V.empty
  ZDir
    | isFaceX p -> V.fromList [getZXminus vp, getZXplus vp]
    | isFaceY p -> V.fromList [getZYminus vp, getZYplus vp]
    | isFaceZ p -> V.fromList [getZYminus vp, getZYplus vp, getZXminus vp, getZXplus vp]
    | otherwise -> V.empty

{-# INLINE isFaceConn #-}
isFaceConn :: FaceID -> FaceID -> a -> Bool
isFaceConn a b _ = unFaceID a == unFaceID b

-- -------------------------- Scan Edges ---------------------------------------

type EdgeSet = (HashMap EdgePos EdgeID, HashMap EdgeID (Vector EdgePos))

groupEdges :: VoxBoxRange -> [(EdgeVoxelPos, EdgeID)] -> EdgeSet
groupEdges vbr es = let
  hme   :: HashMap VoxelPos EdgeID
  hme   = HM.fromList $ map (\(k,v) -> (toEdgePos k, v)) es

  edges :: VoxConn (HashMap VoxelPos) EdgePos
  edges = finderVoxConn hme vbre

  foo acc@(accPOS, accEID) k v = case HM.lookup (vbre %# k) hme of
    Just fid -> let
      newAccPOS = V.foldl' (\acu p -> HM.insert p finalFID acu) accPOS v
      (finalFID, newAccEID) = insertUniqueHM v fid getAlterEdgeID accEID
      in (newAccPOS, newAccEID)
    _        -> acc

  vbre    = getExtVBR vbr
  zeroSet = (HM.empty, HM.empty)
  -- (HashMap Int (Vector grid))
  in case voxConnList edges of
    Just x -> HM.foldlWithKey' foo zeroSet  x
    _      -> zeroSet

instance GridConn EdgePos where
  {-# INLINABLE toGridType #-}
  toGridType             = EdgePos
  {-# INLINABLE toVoxelPos #-}
  toVoxelPos (EdgePos x) = x
  {-# INLINABLE getConnPos #-}
  getConnPos             = getConnEdgePos
  {-# INLINABLE getCrossConnPos #-}
  getCrossConnPos        = getCrossConnEdgePos

instance HasConn EdgeID where
  isConn      = isConnBase isEdgeConn
  isCrossConn = isConnBase isEdgeConn
  {-# INLINABLE isConn #-}
  {-# INLINABLE isCrossConn #-}

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

getCrossConnEdgePos :: CartesianDir -> EdgePos -> V.Vector (EdgePos, CrossDir)
getCrossConnEdgePos dir p@(EdgePos vp) = V.map (\(a, b) -> (EdgePos a, b)) $ case dir of
  XDir
    | isEdgeX p -> V.fromList [getXYminus vp, getXYplus vp, getXZminus vp, getXZplus vp]
    | isEdgeY p -> V.fromList [getXYminus vp, getXYplus vp]
    | isEdgeZ p -> V.fromList [getXZminus vp, getXZplus vp]
    | otherwise -> V.empty
  YDir
    | isEdgeX p -> V.fromList [getYXminus vp, getYXplus vp]
    | isEdgeY p -> V.fromList [getYXminus vp, getYXplus vp, getYZminus vp, getYZplus vp]
    | isEdgeZ p -> V.fromList [getYZminus vp, getYZplus vp]
    | otherwise -> V.empty
  ZDir
    | isEdgeX p -> V.fromList [getZXminus vp, getZXplus vp]
    | isEdgeY p -> V.fromList [getZYminus vp, getZYplus vp]
    | isEdgeZ p -> V.fromList [getZYminus vp, getZYplus vp, getZXminus vp, getZXplus vp]
    | otherwise -> V.empty

isEdgeConn :: EdgeID -> EdgeID -> a -> Bool
isEdgeConn a b _ = let
  foo (Left e1)  (Left e2)  = e1 == e2
  foo (Right e1) (Right e2) = e1 == e2
  foo _          _          = False
  in foo (unEdgeID a) (unEdgeID b)

-- ================================ Benchmark functions ================================

benchmarkMicroVoxel :: VoxBox Int -> [Benchmark]
benchmarkMicroVoxel vboxdata = case grainFinder vboxdata of
  Just (vboxGID, _) -> let
    vbr     = dimension vboxdata
    allPosV = V.fromList $ getRangePos vbr
    o1 = snd $ groupFaces   vbr felems
    o2 = snd $ groupFacesSP vbr felems
    felems  = getFaces vboxGID allPosV
    --faceSet = groupFaces vbr felems

    --eelems  = getEdges faceSet allPosV
    --edgeSet = groupEdges vbr eelems

    --velems  = getVertex vboxGID edgeSet allPosV

    in traceShow (o1,o2) $ [ bench "grainFinder"  $ nf grainFinder vboxdata
       , bench "groupFaces"   $ nf (groupFaces vbr) felems
       , bench "groupFacesSP" $ nf (groupFacesSP vbr) felems
       --, bench "groupEdges"  $ nf (groupEdges vbr) eelems
       ]
  _ -> []
