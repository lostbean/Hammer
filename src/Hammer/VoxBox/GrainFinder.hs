{-# LANGUAGE RecordWildCards #-}

module Hammer.VoxBox.GrainFinder
  ( grainFinder
  , invGrainMap
  , getVectorGID 
  , BoxData (grainMap, shellBox, boxRange)
  ) where

import qualified Data.List             as L
import qualified Data.HashMap.Strict   as HM
import qualified Data.HashSet          as HS
import qualified Data.Vector           as V
  
import Data.Maybe                     (mapMaybe)
import Data.HashMap.Strict            (HashMap)
import Data.HashSet                   (HashSet)
import Data.Vector                    (Vector)  

import Hammer.MicroGraph.GrainsGraph  (GrainID, mkGrainID)
import Hammer.VoxBox.Base

import Debug.Trace

-- ================================================================================

data BoxData = BoxData
  { grainMap :: HashMap GrainID (HashSet VID)
  , shellBox :: HashMap VID GrainID
  , boxRange :: VoxBoxRange
  } deriving (Show)


getVectorGID :: BoxData -> Vector GrainID
getVectorGID box = let
  hmg = invGrainMap . grainMap $ box
  func i = case HM.lookup i hmg of
    Just x -> x
    _      -> mkGrainID (-1)
  in V.generate (HM.size hmg) func
 
invGrainMap :: HashMap GrainID (HashSet VID) -> HashMap VID GrainID
invGrainMap = let
  func acc k v = HS.foldl' (\bcc x -> HM.insert x k bcc) acc v
  in HM.foldlWithKey' func HM.empty
     
grainFinder :: (Eq a)=> VoxBox a -> Maybe BoxData
grainFinder vbox@VoxBox{..} = foo (toBR dimension)
  where
    toBR (VoxBoxDim x y z) = VoxBoxRange (VoxelPos 0 0 0) (VoxelPos x y z)
    merge = mergeBoxData vbox
    foo :: VoxBoxRange -> Maybe BoxData
    foo boxrange
      | isMinRange boxrange = let
        (VoxBoxRange p _) = boxrange
        in initDataBox dimension p
      | otherwise           = merge b1234 b5678
      where
        (b1, b2, b3, b4, b5, b6, b7, b8) = splitBox boxrange
        b12 = merge (foo =<< b1) (foo =<< b2)
        b34 = merge (foo =<< b3) (foo =<< b4)
        b56 = merge (foo =<< b5) (foo =<< b6)
        b78 = merge (foo =<< b7) (foo =<< b8)
        b1234 = merge b12 b34
        b5678 = merge b56 b78

initDataBox :: VoxBoxDim -> VoxelPos -> Maybe BoxData
initDataBox bdim p = do
  vid <- getVoxelID bdim p
  let
    gid  = mkGrainID vid
    glm  = HM.singleton gid (HS.singleton vid)
    shBx = HM.singleton vid gid
    bxrg = VoxBoxRange p p
  return $ BoxData glm shBx bxrg

scanWall :: CartesianDir -> VoxBoxRange -> [VoxelPos]
scanWall dir br = let
  (lx, ux, ly, uy, lz, uz) = getBoxLinRange br
  in case dir of
    XDir -> [VoxelPos lx j k | j <- [ly..uy], k <- [lz..uz]]
    YDir -> [VoxelPos i ly k | i <- [lx..ux], k <- [lz..uz]]
    ZDir -> [VoxelPos i j lz | i <- [lx..ux], j <- [ly..uy]]
    
isConn :: (Eq a) => VoxBox a -> CartesianDir -> VoxelPos -> Maybe (VID, VID)
isConn vbox dir p = let
  getVID = unsafeGetVoxelID (dimension vbox)
  foo x
    | vbox#!p == vbox#!(p#+#x) = return $ (getVID p, getVID $ p #+# x)
    | otherwise                = Nothing
  in case dir of
    XDir -> foo $ VoxelPos 1 0 0
    YDir -> foo $ VoxelPos 0 1 0
    ZDir -> foo $ VoxelPos 0 0 1
    
mixMaps :: BoxData -> (VID, VID) -> BoxData
mixMaps bd@BoxData{..} (p1, p2) = case foo of
  Just x -> x
  _      -> bd
  where
    foo = do
      ogid1 <- HM.lookup p1 shellBox
      ogid2 <- HM.lookup p2 shellBox
      if ogid1 < ogid2
        then takeToFrom ogid1 ogid2
        else takeToFrom ogid2 ogid1

    updateShell set shell gid = HS.foldl' (\acc k -> HM.adjust (const gid) k acc) shell set

    -- Take the VID set from gid1 to gid2
    -- gid1 -> Dominate grain ID
    -- gid2 -> its content will be merge into gid1
    takeToFrom gid1 gid2 = do
      set <- gidSet 
      return $ bd { grainMap = modGLM set, shellBox = modShB set }
      where
        modShB set = updateShell set shellBox gid1
        gidSet     = HM.lookup gid2 grainMap
        delGLM     = HM.delete gid2 grainMap
        modGLM set = HM.insertWith (HS.union) gid1 set delGLM

cleanInnerShell :: VoxBoxDim -> [VoxelPos] -> BoxData -> BoxData
cleanInnerShell bdim wall bd@BoxData{..} = let
  innerWall = filter (not . isOnEdge boxRange) wall
  innerVIDs = map (unsafeGetVoxelID bdim) innerWall 
  newSB     = L.foldl' (\acc k -> HM.delete k acc) shellBox innerVIDs
  in bd { shellBox = newSB }
    
mergeBoxData :: (Eq a)=> VoxBox a -> Maybe BoxData -> Maybe BoxData -> Maybe BoxData
mergeBoxData vbox (Just d1) (Just d2) = do
  (newBR, dir, wall) <- mergeVoxBoxRange (boxRange d1) (boxRange d2)
  let
    withConn    = mapMaybe (isConn vbox dir) wall
    uniGrainMap = HM.union (grainMap d1) (grainMap d2)
    uniShellBox = HM.union (shellBox d1) (shellBox d2)
    newBD       = BoxData uniGrainMap uniShellBox newBR
    mixBD       = L.foldl' mixMaps newBD withConn
  return $ cleanInnerShell (dimension vbox) wall mixBD 
mergeBoxData _ x@(Just _)        _ = x
mergeBoxData _ _        x@(Just _) = x
mergeBoxData _ _                 _ = Nothing

mergeVoxBoxRange :: VoxBoxRange -> VoxBoxRange -> Maybe (VoxBoxRange, CartesianDir, [VoxelPos])
mergeVoxBoxRange b1 b2
  | b1 == b2                           = Nothing
  | testX tur1 tul2 && testX blr1 bll2 = out bll1 tur2 XDir blr1 tul2
  | testX tur2 tul1 && testX blr2 bll1 = out bll2 tur1 XDir blr2 tul1
                                         
  | testY tul1 tll2 && testY bur1 blr2 = out bll1 tur2 YDir bur1 tll2
  | testY tul2 tll1 && testY bur2 blr1 = out bll2 tur1 YDir bur2 tll1
                                         
  | testZ tur1 bur2 && testZ tll1 bll2 = out bll1 tur2 ZDir bur2 tll1
  | testZ tur2 bur1 && testZ tll2 bll1 = out bll2 tur1 ZDir bur1 tll2
  | otherwise                          = Nothing
  where
    (bll1, bul1, bur1, blr1, tur1, tlr1, tll1, tul1) = getBox b1
    (bll2, bul2, bur2, blr2, tur2, tlr2, tll2, tul2) = getBox b2
    testX a b = a #+# (VoxelPos 1 0 0) == b
    testY a b = a #+# (VoxelPos 0 1 0) == b
    testZ a b = a #+# (VoxelPos 0 0 1) == b
    out p1 p2 dir w1 w2 = let
      br = VoxBoxRange p1 p2
      wl = VoxBoxRange w1 w2
      in return (br, dir, scanWall dir wl)
                                             
