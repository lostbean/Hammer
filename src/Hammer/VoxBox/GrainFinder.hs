{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Hammer.VoxBox.GrainFinder
  ( grainFinder
  ) where

import qualified Data.List             as L
import qualified Data.HashMap.Strict   as HM
import qualified Data.Vector           as V
  
import           Control.Applicative            ((<|>), (<$>))
import           Control.Monad.ST               (runST)
import           Data.HashMap.Strict            (HashMap)
import           Data.Maybe                     (mapMaybe)
import           Data.Vector                    (Vector)
import           Data.Vector.Mutable            (write)
import           Hammer.MicroGraph              (GrainID, mkGrainID)
import           Hammer.VoxBox.Base

import           Control.DeepSeq
import           Control.Parallel.Strategies

import           Debug.Trace

-- ================================================================================

data BoxData = BoxData
  { grainMap :: HashMap GrainID (Vector VoxelPos)
  , shellBox :: GrainMap
  , boxRange :: VoxBoxRange
  } deriving (Show)

instance NFData BoxData where
  rnf (BoxData gm sb br) = rnf gm `seq` rnf sb `seq` rnf br

getVectorGID :: BoxData -> Vector GrainID
getVectorGID = mapGID . shellBox

grainFinder :: (Eq a)=> VoxBox a -> Maybe (VoxBox GrainID, HashMap GrainID (Vector VoxelPos))
grainFinder vb = do
  boxdata <- grainFinder' vb
  return (vb { grainID = getVectorGID boxdata }, grainMap boxdata)
  
grainFinder' :: (Eq a)=> VoxBox a -> Maybe BoxData
grainFinder' vbox@VoxBox{..} = go dimension
  where
    merge          = mergeBoxData vbox
    mergeRange a b = merge (go =<< a) (go =<< b)
    
    go :: VoxBoxRange -> Maybe BoxData
    go boxrange
      | isMinRange boxrange = let
        (VoxBoxRange p _) = boxrange
        in initDataBox dimension p
      | sizeVoxBoxRange boxrange >= 500 = run rpar
      | otherwise                       = run rseq
      where
        (b1, b2, b3, b4, b5, b6, b7, b8) = splitBox boxrange
        run strat = runEval $ do
          b12 <- strat (mergeRange b1 b2)
          b34 <- strat (mergeRange b3 b4)
          b56 <- strat (mergeRange b5 b6)
          b78 <- strat (mergeRange b7 b8)
          rdeepseq b12
          rdeepseq b34
          rdeepseq b56
          rdeepseq b78
          b1234 <- strat (merge b12 b34)
          b5678 <- strat (merge b56 b78)
          rdeepseq b1234
          rdeepseq b5678
          return (merge b1234 b5678)

initDataBox :: VoxBoxRange -> VoxelPos -> Maybe BoxData
initDataBox vbrGlobal p = do
  vid <- getVoxelID vbrGlobal p
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
      compareBoth ogid1 ogid2
  
    compareBoth gid1 gid2
      -- Check if it has been megered before (GrainID_1 == GrainID_2)
      | gid1 == gid2 = return bd             
      | gid1 <  gid2 = takeToFrom gid1 gid2
      | otherwise    = takeToFrom gid2 gid1
      
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
  uniShellBox        <- mergeGrainMap    (shellBox d1) (shellBox d2)
  let
    withConn    = mapMaybe (isConn vbox dir) wall
    uniGrainMap = HM.union (grainMap d1) (grainMap d2)
    newBD       = BoxData uniGrainMap uniShellBox newBR
    mixBD       = L.foldl' mixMaps newBD withConn
  return $ mixBD
mergeBoxData _ a b = a <|> b

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
    (bll1, _, bur1, blr1, tur1, _, tll1, tul1) = getBox b1
    (bll2, _, bur2, blr2, tur2, _, tll2, tul2) = getBox b2
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
             
instance NFData GrainMap where
  rnf (GrainMap mb mg) = rnf mb `seq` rnf mg

{-# INLINE (%!) #-} 
(%!) :: GrainMap -> VoxelPos -> GrainID
GrainMap{..} %! pos = mapGID V.! (mapBox %@ pos) 
                       
{-# INLINE (%!?) #-} 
(%!?) :: GrainMap -> VoxelPos -> Maybe GrainID
GrainMap{..} %!? pos = (mapGID V.!) <$> (mapBox %@? pos) 
    
mergeGrainMap :: GrainMap -> GrainMap -> Maybe GrainMap
mergeGrainMap gma gmb = do
  (new_range, _, _) <- mergeVoxBoxRange (mapBox gma) (mapBox gmb)
  let
    new_size = getSize (mapBox gma) + getSize (mapBox gmb)
    
    getSize  = (\(x,y,z) -> abs $ x*y*z) . getVoxBoxDim . vbrDim
         
    getPosInRange :: GrainMap -> Int -> Maybe GrainID
    getPosInRange gm@GrainMap{..} i = do
      pos <- getVoxelPos new_range i
      let test
            | isInBox mapBox pos = return $ gm %! pos
            | otherwise          = Nothing
        in test
             
    func i = let
      n = getPosInRange gma i <|> getPosInRange gmb i
      in case n of
        Just x -> x
        _      -> error "[GrainFinder] unable to merge two GrainMap's."
        
  return $ GrainMap { mapBox = new_range
                    , mapGID = V.generate new_size func }

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
(VoxBoxRange (VoxelPos x0 y0 z0) (VoxBoxDim dx dy _)) %@# (VoxelPos x y z) =
  (x-x0) + dx*(y-y0) + dx*dy*(z-z0)

-- ====================================================================================
 