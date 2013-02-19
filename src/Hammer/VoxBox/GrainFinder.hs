{-# LANGUAGE RecordWildCards #-}

module Hammer.VoxBox.GrainFinder
  ( grainFinder
  , getVectorGID 
  , BoxData (grainMap, shellBox, boxRange)
  ) where

import qualified Data.List             as L
import qualified Data.HashMap.Strict   as HM
import qualified Data.HashSet          as HS
import qualified Data.Vector           as V
  
import           Control.Applicative            ((<|>), (<$>))
import           Control.Monad                  (foldM_)
import           Control.Monad.ST               (runST)
import           Data.HashMap.Strict            (HashMap)
import           Data.HashSet                   (HashSet)
import           Data.Maybe                     (mapMaybe)
import           Data.Vector                    (Vector)
import           Data.Vector.Mutable            (write)
import           Hammer.MicroGraph.GrainsGraph  (GrainID, mkGrainID, unGrainID)
import           Hammer.VoxBox.Base

import           Debug.Trace

-- ================================================================================

data BoxData = BoxData
  { grainMap :: HashMap GrainID (Vector VoxelPos)
  , shellBox :: GrainMap
  , boxRange :: VoxBoxRange
  } deriving (Show)

getVectorGID :: BoxData -> Vector GrainID
getVectorGID = mapGID . shellBox

grainFinder :: (Eq a)=> VoxBox a -> Maybe BoxData
grainFinder vbox@VoxBox{..} = foo br
  where
    br    = VoxBoxRange (VoxelPos 0 0 0) dimension
    merge = mergeBoxData vbox
    foo :: VoxBoxRange -> Maybe BoxData
    foo boxrange
      | isMinRange boxrange = let
        (VoxBoxRange p _) = boxrange
        in initDataBox dimension p
      | otherwise         = merge b1234 b5678
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
    glm  = HM.singleton gid (V.singleton p)
    shBx = GrainMap bxrg (V.singleton gid)
    bxrg = VoxBoxRange p (VoxBoxDim 1 1 1)
  return $ BoxData glm shBx bxrg

scanWall :: CartesianDir -> VoxBoxRange -> [VoxelPos]
scanWall dir br = let
  (lx, ux, ly, uy, lz, uz) = getBoxLinRange br
  in case dir of
    XDir -> [VoxelPos lx j k | j <- [ly..uy], k <- [lz..uz]]
    YDir -> [VoxelPos i ly k | i <- [lx..ux], k <- [lz..uz]]
    ZDir -> [VoxelPos i j lz | i <- [lx..ux], j <- [ly..uy]]
    
isConn :: (Eq a) => VoxBox a -> CartesianDir -> VoxelPos -> Maybe (VoxelPos, VoxelPos)
isConn vbox dir p = let
  foo x
    | vbox#!p == vbox#!(p#+#x) = return $ (p, p #+# x)
    | otherwise                = Nothing
  in case dir of
    XDir -> foo $ VoxelPos 1 0 0
    YDir -> foo $ VoxelPos 0 1 0
    ZDir -> foo $ VoxelPos 0 0 1
    
mixMaps :: BoxData -> (VoxelPos, VoxelPos) -> BoxData
mixMaps bd@BoxData{..} (p1, p2) = case foo of
  Just x -> x
  _      -> bd
  where
    foo = do
      ogid1 <- shellBox %!? p1
      ogid2 <- shellBox %!? p2
      if ogid1 < ogid2
        then takeToFrom ogid1 ogid2
        else takeToFrom ogid2 ogid1

    -- Take the VID set from gid1 to gid2
    -- gid1 -> Dominate grain ID
    -- gid2 -> its content will be merge into gid1
    takeToFrom gid1 gid2 = do
      set <- gidSet 
      return $ bd { grainMap = modGLM set, shellBox = modShB set }
      where
        modShB set = updateGM set gid1 shellBox
        gidSet     = HM.lookup gid2 grainMap
        delGLM     = HM.delete gid2 grainMap
        modGLM set = HM.insertWith (V.++) gid1 set delGLM
   
mergeBoxData :: (Eq a)=> VoxBox a -> Maybe BoxData -> Maybe BoxData -> Maybe BoxData
mergeBoxData vbox (Just d1) (Just d2) = do
  (newBR, dir, wall) <- mergeVoxBoxRange (boxRange d1) (boxRange d2)
  uniShellBox        <- mergeGrainMap (shellBox d1) (shellBox d2)
  let
    withConn    = mapMaybe (isConn vbox dir) wall
    uniGrainMap = HM.union (grainMap d1) (grainMap d2)
    newBD       = BoxData uniGrainMap uniShellBox newBR
    mixBD       = L.foldl' mixMaps newBD withConn
  return $ mixBD
mergeBoxData _ x@(Just _)        _ = x
mergeBoxData _ _        x@(Just _) = x
mergeBoxData _ _                 _ = Nothing

mergeVoxBoxRange :: VoxBoxRange -> VoxBoxRange
                 -> Maybe (VoxBoxRange, CartesianDir, [VoxelPos])
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
      br = posRange2VoxBox p1 p2
      wl = posRange2VoxBox w1 w2
      in return (br, dir, scanWall dir wl)
                                             
-- --------------------------- GrainMap -----------------------------------
    
data GrainMap = GrainMap
  { mapBox    :: VoxBoxRange
  , mapGID    :: Vector GrainID
  } deriving (Show)
             
{-# INLINE (%@?) #-} 
(%@?) :: VoxBoxRange -> VoxelPos -> Maybe Int
(VoxBoxRange org dim) %@? pos = let
  pos' = pos #-# org
  in getVoxelID dim pos'

{-# INLINE (%@)  #-} 
(%@) :: VoxBoxRange -> VoxelPos -> Int
(VoxBoxRange org dim) %@ pos = let
  pos' = pos #-# org
  in unsafeGetVoxelID dim pos'
     
{-# INLINE (%!) #-} 
(%!) :: GrainMap -> VoxelPos -> GrainID
GrainMap{..} %! pos = mapGID V.! (mapBox %@ pos) 
                       
{-# INLINE (%!?) #-} 
(%!?) :: GrainMap -> VoxelPos -> Maybe GrainID
GrainMap{..} %!? pos = (mapGID V.!) <$> (mapBox %@? pos) 
    
getRangePos :: VoxBoxRange -> [VoxelPos]
getRangePos (VoxBoxRange (VoxelPos x y z) (VoxBoxDim dx dy dz)) = 
  [ VoxelPos i j k | i <- [x .. x + dx]
                   , j <- [y .. y + dy]
                   , k <- [z .. z + dz]]
  
mergeGrainMap :: GrainMap -> GrainMap -> Maybe GrainMap
mergeGrainMap gma gmb = do
  (new_range, dir, wall) <- mergeVoxBoxRange (mapBox gma) (mapBox gmb)
  let
    new_dim = vbrDim    new_range
    new_org = vbrOrigin new_range
    
    n = getSize (mapBox gma) + getSize (mapBox gmb)
    
    getSize = (\(x,y,z) -> abs $ x*y*z) . getVoxBoxDim . vbrDim
         
    getPosInRange :: GrainMap -> Int -> Maybe GrainID
    getPosInRange gm@GrainMap{..} i = do
      pos <- (new_org #+#) <$> getVoxelPos new_dim i
      let test
            | isInBox mapBox pos = return $ gm %! pos
            | otherwise          = Nothing
        in test
             
    func i = let
      n = getPosInRange gma i <|> getPosInRange gmb i
      in case n of
        Just x -> x
        _      -> error "[GrainFinder] unable to merge two GrainMap's."
        
  return $ GrainMap { mapBox = new_range, mapGID = V.generate n func }

-- ===================  Critical Peformance operations ==============================
  
updateGM :: Vector VoxelPos -> GrainID -> GrainMap -> GrainMap
updateGM set gid gm@GrainMap{..} = let
  in runST $ do
  vec <- V.unsafeThaw mapGID
  V.mapM_ (\p -> write vec (mapBox %@# p) gid) set
  new_vec <- V.unsafeFreeze vec
  return $ gm {mapGID = new_vec}
  
-- This is an optimazed version of %@#. Going a little bit WET for shake of performance.
-- according to GHC core all math op. are primop's
  
{-# INLINE (%@#)  #-} 
(%@#) :: VoxBoxRange -> VoxelPos -> Int
(VoxBoxRange (VoxelPos x0 y0 z0) (VoxBoxDim dx dy dz)) %@# (VoxelPos x y z) =
  (x-x0) + dx*(y-y0) + dx*dy*(z-z0)

-- ====================================================================================
 