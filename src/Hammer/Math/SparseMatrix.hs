{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}

module Hammer.Math.SparseMatrix
  ( Sparse3
  , mkSparse3
  , singleton
  , emptySingleton
  , empty
  , Hammer.Math.SparseMatrix.lookup
  , adjust

  , mergeSparse3
  , mergeSparse3_X
  , mergeSparse3_Y
  , mergeSparse3_Z
  , mergeSparse3_Y_M

  , testR
  , testX
  , testY
  , testZ
  , Sparse3Org (..)
  , Sparse3Dim (..)
  ) where

--import Prelude hiding (lookup)
import qualified Data.Vector                  as V
import qualified Data.Vector.Unboxed          as VU
import qualified Data.Vector.Mutable          as VM
import qualified Data.Vector.Generic          as VG
import qualified Data.Vector.Generic.Mutable  as VGM
import qualified Data.Vector.Unboxed.Mutable  as UM
import qualified Data.Vector.Algorithms.Intro as VA

import Control.Applicative ((<|>))
import Control.Arrow (first)
import Control.DeepSeq
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST        (runST)
import Data.Maybe              (isJust)
import Data.Function (on)

data Sparse3 a =
  Sparse3
  { sparse3Value :: V.Vector a
  , xyMarker     :: VU.Vector Int
  , zMarker      :: VU.Vector Int
  , matrixSize   :: Sparse3Dim
  , matrixOrig   :: Sparse3Org
  } deriving (Show)

newtype Sparse3Dim = Sparse3Dim (Int, Int, Int) deriving (Show, Eq, NFData)
newtype Sparse3Org = Sparse3Org (Int, Int, Int) deriving (Show, Eq, NFData)
newtype LocalPos   = LocalPos   (Int, Int, Int) deriving (Show, Eq, Ord, NFData)

instance (NFData a)=> NFData (Sparse3 a) where
  rnf (Sparse3 sv xyM zM ms mo) = rnf sv `seq` rnf xyM `seq` rnf zM `seq` rnf ms `seq` rnf mo

testR, testX, testY, testZ :: Sparse3 Int
testR = mkSparse3 (Sparse3Org (0,0,0)) (Sparse3Dim (3,3,3)) (V.fromList [((0,0,0),10::Int),((1,1,1),0::Int)])
testX = mkSparse3 (Sparse3Org (3,0,0)) (Sparse3Dim (3,3,3)) (V.fromList [((4,0,0),11::Int),((4,1,1),1::Int)])
testY = mkSparse3 (Sparse3Org (0,3,0)) (Sparse3Dim (3,3,3)) (V.fromList [((0,4,0),12::Int),((1,4,1),2::Int)])
testZ = mkSparse3 (Sparse3Org (0,0,3)) (Sparse3Dim (3,3,3)) (V.fromList [((0,0,4),13::Int),((1,1,4),3::Int)])

singleton :: Sparse3Org -> a -> Sparse3 a
singleton so x = let
  sd  = Sparse3Dim (1,1,1)
  zV  = V.singleton x
  zM  = VU.singleton 0
  xyM = VU.singleton 0
  in Sparse3 zV xyM zM sd so

emptySingleton :: Sparse3Org -> Sparse3 a
emptySingleton so = empty so (Sparse3Dim (1,1,1))

empty :: Sparse3Org -> Sparse3Dim -> Sparse3 a
empty so sd = let
  xysize  = getXYMarkerSize sd
  xyM = VU.replicate xysize 0 --1
  zM  = VU.empty
  zV  = V.empty
  in Sparse3 zV xyM zM sd so

mkSparse3 :: Sparse3Org -> Sparse3Dim -> V.Vector ((Int, Int, Int), a) -> Sparse3 a
mkSparse3 so sd vec = let
  invec   = V.map (\(Just a, b) -> (a, b)) . V.filter (isJust . fst) $ V.map (first (toLocalPos so sd)) vec
  svec    = V.modify (VA.sortBy (compare `on` fst)) invec
  xyspace = getXYRange sd
  zsize   = V.length svec
  xysize  = getXYMarkerSize sd
  zmax    = zsize - 1
  xymax   = xysize - 1
  getPos (x, y) = getXYPos sd $ LocalPos (x, y, 0)
  in runST $ do
    xyMM <- UM.new xysize
    zMM  <- UM.new zsize
    zVM  <- VM.new zsize
    let
      scan _ [] = return ()
      scan i s@(xy:xys)
        | i > zmax && (xypos <= 0 || xypos >= xymax) = scan i xys
        | i > zmax = do
          UM.write xyMM (xypos+1) zsize
          scan i xys
        | xy == xyi = do
          UM.write zMM i zi
          VM.write zVM i vi
          scan (i+1) s
        | xy <  xyi = do
          UM.write xyMM (xypos+1) i
          scan i xys
        | otherwise = scan (i+1) s
        where
          (LocalPos (xi, yi, zi), vi) = svec V.! i
          xyi    = (xi, yi)
          xypos  = getPos xy
    scan 0 xyspace
    UM.write xyMM 0 0
    xyM <- VU.unsafeFreeze xyMM
    zM  <- VU.unsafeFreeze zMM
    zV  <- V.unsafeFreeze zVM
    return $ Sparse3 zV xyM zM sd so

lookup :: (Int, Int, Int) -> Sparse3 a -> Maybe a
lookup t sp = (sparse3Value sp V.!) <$> getZIndex t sp

adjust :: V.Vector ((Int,Int,Int), a) -> Sparse3 a -> Sparse3 a
adjust vec sp = let
  foo = V.map (\(Just i, x) -> (i,x)) . V.filter (isJust . fst) . V.map (\(t, x) -> (getZIndex t sp, x))
  sv  = V.update (sparse3Value sp) (foo vec)
  in sp {sparse3Value = sv}

-- | Merge 2 @Sparse3@ side by side along the x axis where
-- the first arguments is the lower one (lower x values than those in
-- the second argument).
mergeSparse3 :: (Show a)=> Sparse3 a -> Sparse3 a -> Maybe (Sparse3 a)
mergeSparse3 sa sb
  | (x0a + dxa) == x0b && testYZ = return $ mergeSparse3_X sa sb
  | (x0b + dxb) == x0a && testYZ = return $ mergeSparse3_X sb sa

  | (y0a + dya) == y0b && testZX = return $ mergeSparse3_Y sa sb
  | (y0b + dyb) == y0a && testZX = return $ mergeSparse3_Y sb sa

  | (z0a + dza) == z0b && testXY = return $ mergeSparse3_Z sa sb
  | (z0b + dzb) == z0a && testXY = return $ mergeSparse3_Z sb sa
  | otherwise                    = Nothing
  where
    testXY = dxa == dxb && dya == dyb
    testYZ = dya == dyb && dza == dzb
    testZX = dza == dzb && dxa == dxb
    (Sparse3Dim (dxa, dya, dza)) = matrixSize sa
    (Sparse3Dim (dxb, dyb, dzb)) = matrixSize sb
    (Sparse3Org (x0a, y0a, z0a)) = matrixOrig sa
    (Sparse3Org (x0b, y0b, z0b)) = matrixOrig sb

-- ================================ Internal Functions ===============================

getZIndex :: (Int, Int, Int) -> Sparse3 a -> Maybe Int
getZIndex t Sparse3{..} = do
  lt@(LocalPos (_,_,z)) <- toLocalPos matrixOrig matrixSize t
  let
    xypos = getXYPos matrixSize lt
    xymax = getXYMarkerSize matrixSize - 1
    zinit = VU.unsafeIndex xyMarker xypos
    znext
      | xypos >= xymax = VU.length zMarker
      | otherwise      = xyMarker VU.! (xypos+1)

  if zinit < znext
    then scanFor zMarker zinit (znext-1) z
    else Nothing

scanFor :: VU.Vector Int -> Int -> Int -> Int -> Maybe Int
scanFor vec ip fp v
  | ip >  fp  = Nothing
  | ip == fp  = test ip
  | otherwise = go ip fp
  where
    test i = if v == (vec VU.! i) then Just i else Nothing
    go i f
      | diff <= 1 = test i <|> test f
      | x == v    = Just pos
      | x >  v    = go i pos
      | otherwise = go pos f
      where
        diff = f - i
        pos  = (i + f) `quot` 2
        x    = vec VU.! pos

-- | Merge 2 @Sparse3@ side by side along the x axis where
-- the first arguments is the lower one (lower x values than those in
-- the second argument).
mergeSparse3_X :: Sparse3 a -> Sparse3 a -> Sparse3 a
mergeSparse3_X sa sb = let
  zm  = zMarker sa VU.++ zMarker sb
  xym = xyMarker sa VU.++ VU.map (+ zmaSize) (xyMarker sb)
  sv  = sparse3Value sa V.++ sparse3Value sb
  zmaSize = VU.length $ zMarker sa
  (Sparse3Dim (dxa, dya, _)) = matrixSize sa
  (Sparse3Dim (dxb, dyb, _)) = matrixSize sb
  md = Sparse3Dim (dxa+dxb, dya, dyb)
  in Sparse3 sv xym zm md (matrixOrig sa)


-- | Merge 2 @Sparse3@ side by side along the x axis where
-- the first arguments is the lower one (lower x values than those in
-- the second argument).
mergeSparse3_Y :: (Show a)=> Sparse3 a -> Sparse3 a -> Sparse3 a
mergeSparse3_Y sa sb = Sparse3 sv xym zm md (matrixOrig sa)
  where
    xyaSize = VU.length $ xyMarker sa
    xybSize = VU.length $ xyMarker sb

    (Sparse3Dim (dxa, dya, dza)) = matrixSize sa
    (Sparse3Dim (_,   dyb, _  )) = matrixSize sb
    md = Sparse3Dim (dxa, dya+dyb, dza)

    sliceAzm = getSlices (xyMarker sa) (zMarker sa) dya
    sliceBzm = getSlices (xyMarker sb) (zMarker sb) dyb
    sliceAsv = getSlices (xyMarker sa) (sparse3Value sa) dya
    sliceBsv = getSlices (xyMarker sb) (sparse3Value sb) dyb
    xyMADiff = getXYSizeArr $ xyMarker sa
    xyMBDiff = getXYSizeArr $ xyMarker sb
    sliceAxy = [(xyMADiff, i, dya) | i <- [0, dya .. (xyaSize-1)]]
    sliceBxy = [(xyMBDiff, i, dyb) | i <- [0, dyb .. (xybSize-1)]]

    zm  = inter sliceAzm sliceBzm
    sv  = inter sliceAsv sliceBsv
    xym = VU.postscanl' (+) 0 $ inter sliceAxy sliceBxy

-- | Merge 2 @Sparse3@ side by side along the x axis where
-- the first arguments is the lower one (lower x values than those in
-- the second argument).
mergeSparse3_Z :: (Show a)=> Sparse3 a -> Sparse3 a -> Sparse3 a
mergeSparse3_Z sa sb = let
  (Sparse3Dim (dxa, dya, dza)) = matrixSize sa
  (Sparse3Dim (_,   dyb, dzb)) = matrixSize sb
  md = Sparse3Dim (dxa, dya, dza+dzb)

  zbm = VU.map (+ dya) (zMarker sb)
  sliceAzm = getSlices (xyMarker sa) (zMarker sa) dya
  sliceBzm = getSlices (xyMarker sb) zbm          dyb
  sliceAsv = getSlices (xyMarker sa) (sparse3Value sa) dya
  sliceBsv = getSlices (xyMarker sb) (sparse3Value sb) dyb

  xym = VU.zipWith (+) (xyMarker sa) (xyMarker sb)
  zm  = inter sliceAzm sliceBzm
  sv  = inter sliceAsv sliceBsv

  in Sparse3 sv xym zm md (matrixOrig sa)

inter :: (VG.Vector v a) => [(v a, Int, Int)] -> [(v a, Int, Int)] -> v a
inter la lb = let
  func (v, !i, !s) = VG.slice i s v
  in VG.concat $ zipWith ((VG.++) `on` func) la lb

getSlices :: (VG.Vector v a)=> VU.Vector Int -> v a -> Int -> [(v a, Int, Int)]
getSlices xyV zV step = let
  xyVMax = VU.length xyV - 1
  zVSize = VG.length zV
  heads = map (xyV VU.!) [0, step .. xyVMax]
  func (i:f:is) = (zV, i, f-i) : func (f:is)
  func (i:_)    = [(zV, i, zVSize - i)]
  func []       = []
  in func heads

getXYSizeArr :: (Num a, VG.Vector v a)=> v a -> v a
getXYSizeArr vec = let
  func i x = if i > 0 then x - (vec VG.! (i-1)) else x
  in VG.imap func vec

toLocalPos :: Sparse3Org -> Sparse3Dim -> (Int, Int, Int) -> Maybe LocalPos
toLocalPos (Sparse3Org (x0, y0, z0)) (Sparse3Dim (dx, dy, dz)) (x, y, z)
  | lx >= 0 && lx < dx && ly >= 0 &&
    ly < dy && lz >= 0 && lz < dz = return $ LocalPos (lx, ly, lz)
  | otherwise = Nothing
  where
    lx = x - x0
    ly = y - y0
    lz = z - z0

getXYRange :: Sparse3Dim -> [(Int, Int)]
getXYRange (Sparse3Dim (dx, dy, _))= [(x, y) | x <- [0 .. dx-1], y <- [0 .. dy-1]]

getXYPos :: Sparse3Dim -> LocalPos -> Int
getXYPos (Sparse3Dim (_, dy , _)) (LocalPos (x, y ,_)) = x * dy + y

getXYMarkerSize :: Sparse3Dim -> Int
getXYMarkerSize (Sparse3Dim (dx, dy, _)) = dx * dy

-- ======================== monadic version ==============================

-- | Merge 2 @Sparse3@ side by side along the x axis where
-- the first arguments is the lower one (lower x values than those in
-- the second argument).
mergeSparse3_Y_M :: (Show a)=> Sparse3 a -> Sparse3 a -> Sparse3 a
mergeSparse3_Y_M sa sb = runST $ do
  let
    zmaSize = VU.length $ zMarker sa
    zmbSize = VU.length $ zMarker sb
    xyaSize = VU.length $ xyMarker sa
    xybSize = VU.length $ xyMarker sb
    (Sparse3Dim (dxa, dya, dza)) = matrixSize sa
    (Sparse3Dim (_,   dyb, _  )) = matrixSize sb
    md = Sparse3Dim (dxa, dya+dyb, dza)
  zmM  <- UM.new (zmaSize + zmbSize)
  svM  <- VM.new (zmaSize + zmbSize)
  xymM <- UM.new (xyaSize + xybSize)
  let
    sliceAzm = getSlices (xyMarker sa) (zMarker sa) dya
    sliceBzm = getSlices (xyMarker sb) (zMarker sb) dyb
    sliceAsv = getSlices (xyMarker sa) (sparse3Value sa) dya
    sliceBsv = getSlices (xyMarker sb) (sparse3Value sb) dyb
    xyMADiff = getXYSizeArr $ xyMarker sa
    xyMBDiff = getXYSizeArr $ xyMarker sb
    sliceAxy = [(xyMADiff, i, dya) | i <- [0, dya .. (xyaSize-1)]]
    sliceBxy = [(xyMBDiff, i, dyb) | i <- [0, dyb .. (xybSize-1)]]
  interM zmM  0 sliceAzm sliceBzm
  interM svM  0 sliceAsv sliceBsv
  interM xymM 0 sliceAxy sliceBxy
  zm  <- VU.unsafeFreeze zmM
  sv  <- V.unsafeFreeze svM
  xym <- VU.unsafeFreeze xymM
  return $ Sparse3 sv (VU.postscanl' (+) 0 xym) zm md (matrixOrig sa)

interM :: (VGM.MVector va a, VG.Vector vb a, PrimMonad m) =>
         va (PrimState m) a
         -> Int
         -> [(vb a, Int, Int)]
         -> [(vb a, Int, Int)]
         -> m ()
interM _ _ [] _ = return ()
interM _ _ _ [] = return ()
interM v (!i) ((va, !ia, !sa):la) ((vb, !ib, !sb):lb) = do
  moveSlice va v ia sa i
  moveSlice vb v ib sb (i+sa)
  interM v (i+sa+sb) la lb

{-# INLINE moveSlice #-}
moveSlice :: (VG.Vector va a, VGM.MVector vb a, PrimMonad m)
             => va a
             -> vb (PrimState m) a
             -> Int -> Int -> Int -> m ()
moveSlice va vb ia size ib
  | size <= 0 = return ()
  | otherwise = mapM_ func [ia..ia+size-1]
  where
    k = ib - ia
    func i = let x = VG.unsafeIndex va i in VGM.unsafeWrite vb (k+i) x
