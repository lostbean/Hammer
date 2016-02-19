{-# LANGUAGE
    BangPatterns
  , FlexibleContexts
  , FlexibleInstances
  , RecordWildCards
  , MultiParamTypeClasses
  #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Hammer.Math.Array
       ( -- * Multidimensional array
         Array      (shape, array)
       , IxArray    (..)
       , getAt
       , genArray
       , mkUnsafeArray
         -- * N-dim matrix
       , genMatrix2D
       , matrixToCols
       , matrixToRows
       , zeroMatrix2D
       , idIdMatrix2D
         -- * N-dim vector
       , zeroVector
         -- * Vector tools
       , binarySearch
       , shuffleVector
       ) where

import qualified Data.List                   as L
import qualified Data.Vector.Generic         as V
import qualified Data.Vector.Generic.Mutable as VM
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Generic         as VG

import           Data.Vector         (Vector)
import           Data.Bits           (shiftR)
import           Text.Printf         (printf)

import           System.Random

import           Hammer.Math.AlgebraBase

-- ====================================== Array ====================================

data Array i e
  = Array
  { shape :: (Shape i)
  , array :: VU.Vector e
  } deriving (Show, Eq)

data Shape i = Shape i deriving (Show, Eq)

class (Ord a)=> IxArray a where
  ix       :: Shape a -> Int -> a
  pos      :: Shape a -> a -> Int
  rangeIx  :: Shape a -> [a]
  rangePos :: Shape a -> [Int]
  linLen   :: Shape a -> Int
  ixSize   :: Shape a -> a

instance IxArray (Int, Int) where
  ix  (Shape (m, _))        = (\(j,i) -> (i,j)) . flip quotRem m
  pos (Shape (m, _)) (i, j) = i + j*m
  rangeIx (Shape (m, n))    = [(i, j) | j <- [0..n-1], i <- [0..m-1]]
  rangePos bd               = [0 .. (linLen bd) - 1]
  ixSize (Shape x)          = x
  linLen (Shape (m, n))     = (m * n)

instance IxArray (Int, Int, Int) where
  ix (Shape (m, n, _)) p = let
    (k, mn) = quotRem p  (m*n)
    (j, i)  = quotRem mn m
    in (i, j, k)
  pos (Shape (m, n, _)) (i,j,k) = i + j*m + k*m*n
  rangeIx (Shape (m, n, o)) = [(i,j,k) | k <- [0..o-1], j <- [0..n-1], i <- [0..m-1]]
  rangePos bd               = [0 .. (linLen bd) - 1]
  ixSize (Shape x)          = x
  linLen (Shape (m, n, o))  = (m * n * o)

getAt :: (IxArray i, VU.Unbox e)=> Array i e -> i -> e
getAt (Array{..}) p = array VU.! (pos shape p)

mkUnsafeArray :: (IxArray i, VU.Unbox e)=> Shape i -> VU.Vector e -> Array i e
mkUnsafeArray bd vec
  | sb == sv  = Array bd vec
  | otherwise = error "[Array] Array size and input vector don't match!"
  where
    sb = linLen bd
    sv = V.length vec

genArray :: (IxArray i, VU.Unbox e)=> (i -> e) -> Shape i -> Array i e
genArray func shape = let
  arr = VU.generate (linLen shape) (func . ix shape)
  in mkUnsafeArray shape arr

-- ======================================= Matrix2D ======================================

type Matrix2D  = Array (Int, Int) Double
type UVector = VU.Vector Double

isMatrixSameShape :: Matrix2D -> Matrix2D -> Bool
isMatrixSameShape mtr1 mtr2 = (ixSize $ shape mtr1) == (ixSize $ shape mtr2)

testShape :: Matrix2D -> Matrix2D -> a -> a
testShape mtr1 mtr2
  | isMatrixSameShape mtr1 mtr2 = id
  | otherwise = error $ "[AlgebraLargeMatrix] Sorry I can't do this operation in matrices with different shape: "
                ++ "m1 = " ++ show (ixSize $ shape mtr1) ++ " and m2 = " ++ show (ixSize $ shape mtr2)

genMatrix2D :: ((Int, Int) -> Double) -> Shape (Int, Int) -> Matrix2D
genMatrix2D = genArray

matrixToCols :: Matrix2D -> Vector UVector
matrixToCols Array{..} = let
  (rows, cols) = ixSize shape
  func c       = VU.slice (c*rows) rows array
  in V.generate cols func

matrixToRows :: Matrix2D -> Vector (VU.Vector Double)
matrixToRows Array{..} = let
  (rows, cols) = ixSize shape
  func r       = VU.generate cols (\c -> array VU.! (pos shape (r, c)))
  in V.generate rows func

-- TODO split Matrix class in 3: IdMatrix, Transposable, Inversable and make Matrix as super class
instance Transposable Matrix2D where
  transpose mtr = let
    rows = V.toList $ matrixToRows mtr
    arr  = VU.concat rows
    Shape (m, n) = shape mtr
    in mkUnsafeArray (Shape (n, m)) arr

zeroMatrix2D :: Shape (Int, Int) -> Matrix2D
zeroMatrix2D size = let
  a = VU.replicate (linLen size) 0
  in Array size a

idIdMatrix2D :: Int -> Matrix2D
idIdMatrix2D dime = genArray func (Shape (dime, dime))
  where func (i, j)
          | i == j    = 1
          | otherwise = 0

instance AbelianGroup Matrix2D where
  (&+) mtr1 mtr2 = let
    m = mtr1 { array = VU.zipWith (+) (array mtr1) (array mtr2)}
    in testShape mtr1 mtr2 m
  (&-) mtr1 mtr2 = let
    m = mtr1 { array = VU.zipWith (-) (array mtr1) (array mtr2)}
    in testShape mtr1 mtr2 m
  neg mtr = mtr {array = VU.map negate (array mtr)}
  zero = error "[Array] Use zeroMatrix2D instead."

instance MultiVec Matrix2D where
  scalarMul s mtr = mtr {array = VU.map (s*) (array mtr)}
  mapVec    f mtr = mtr {array = VU.map f    (array mtr)}

instance MultSemiGroup Matrix2D where
  mtr1 .*. mtr2 = let
    (m1, n1) = ixSize $ shape mtr1
    (m2, n2) = ixSize $ shape mtr2
    bd       = Shape (m1, n2)
    ps       = V.fromList $ rangeIx bd
    vrow     = matrixToRows mtr1
    vcol     = matrixToCols mtr2
    arr      = VU.map (\(i,j) -> (vrow V.! i) &. (vcol V.! j)) ps
    in if n1 == m2
       then Array bd arr
       else error $ "[AlgebraLargeMatrix] Sorry I can't do matrix multiplication with these shapes: "
            ++ "m1 = " ++ show (ixSize $ shape mtr1) ++ " and m2 = " ++ show (ixSize $ shape mtr2)
  one = error "[Array] Use idIdMatrix2D instead."

instance Ring Matrix2D

instance LeftModule Matrix2D UVector where
  lmul mtr vcol = let
    rows = matrixToRows mtr
    in VU.convert $ V.map (&. vcol) rows

instance RightModule UVector Matrix2D where
  rmul vrow mtr = let
    cols = matrixToCols mtr
    in VU.convert $ V.map (&. vrow) cols

instance Diagonal UVector Matrix2D where
  diagMtx vec = let
    shp = Shape (m, m)
    m = VU.length vec
    func x
      | i == j    = vec VU.! i
      | otherwise = 0
      where (i,j) = ix shp x
    arr = VU.generate (m*m) func
    in Array shp arr
  diagVec Array{..} = let
    Shape (m, n) = shape
    shp = Shape (m, m)
    in if m == n
       then VU.generate m (\i -> array VU.! (pos shp (i,i)))
       else error "[Array] Can't get diagonal. Non square matrix."
{--
instance VecFunctor Matrix2D Vec4 where
  vecMap     f (Matrix2D a b c d) = Mat4 (f a) (f b) (f c) (f d)
  vecFoldr   f (Matrix2D a b c d) = f a (f b (f c d))
  vecZipWith f (Matrix2D a1 b1 c1 d1) (Mat4 a2 b2 c2 d2) = Mat4 (f a1 a2) (f b1 b2) (f c1 c2) (f d1 d2)

instance MatFunctor Matrix2D where
  matMap     f (Matrix2D a b c d) = Mat4 (vecMap f a) (vecMap f b) (vecMap f c) (vecMap f d)
  matFoldr   f (Matrix2D a b c d) = f (vecFoldr f a) (f (vecFoldr f b) (f (vecFoldr f c) (vecFoldr f d)))
  matZipWith f (Matrix2D a1 b1 c1 d1) (Mat4 a2 b2 c2 d2) =
    Matrix2D (vecZipWith f a1 a2) (vecZipWith f b1 b2) (vecZipWith f c1 c2) (vecZipWith f d1 d2)
--}

instance PrettyShow Matrix2D where
  showPretty mtr@(Array shp _) = let
    rows = V.toList $ matrixToRows mtr
    foo  = L.intercalate "  " . map (\a -> printf "%3.3e " a) . VU.toList
    in show shp ++ "\n" ++ L.intercalate "\n" (map foo rows)

-- ================================= Algebra with large vectors ==========================

instance AbelianGroup (VU.Vector Double) where
  (&+) = VU.zipWith (+)
  (&-) = VU.zipWith (-)
  neg  = VU.map negate
  zero = error "[Array] Use zeroVector instead."

zeroVector :: Int -> VU.Vector Double
zeroVector = flip VU.replicate 0.0

instance MultiVec (VU.Vector Double) where
  scalarMul s = VU.map (s*)
  mapVec    f = VU.map f

instance DotProd (VU.Vector Double) where
  (&.) v1 v2 = VU.sum $ pointwise v1 v2

instance Pointwise (VU.Vector Double) where
  pointwise = VU.zipWith (*)

-- ======================================= Vector Tools ==================================

binarySearch :: (Ord a, VG.Vector v a) => v a -> a -> Int
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
shuffleVector :: (VG.Vector v a)=> v a -> v a
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

-- ======================================= Test suite ====================================

testIx2D :: (Int, Int) -> Bool
testIx2D s = let
  shp = Shape s
  ps  = V.fromList $ rangeIx shp :: Vector (Int, Int)
  ts  = V.imap (\i p -> i == (pos shp p) && p == (ix shp i)) ps
  in linLen shp == V.length ps && V.and ts

testIx3D :: (Int, Int, Int) -> Bool
testIx3D s = let
  shp = Shape s
  ps  = V.fromList $ rangeIx shp :: Vector (Int, Int, Int)
  ts  = V.imap (\i p -> i == (pos shp p) && p == (ix shp i)) ps
  in linLen shp == V.length ps && V.and ts

testMatrix :: Bool
testMatrix = let
  ma = genMatrix2D (\(i,j)-> fromIntegral $ i^(2::Int)-2*j) (Shape (5, 3))
  mb = genMatrix2D (\(i,j)-> fromIntegral $ j^(2::Int)-2*i) (Shape (3, 5))
  mc = genMatrix2D (\(i,j)-> fromIntegral $ (i^(4::Int))*j) (Shape (3, 5))
  shp = Shape (5::Int, 5::Int)
  mabt = mkUnsafeArray shp $ VU.fromList xs1
  xs1 :: [Double]
  xs1 = [ 20, 14, -4, -34, -76
        , 14, 11,  2, -13, -34
        , -4,  2, 20,  50,  92
        ,-34,-13, 50, 155, 302
        ,-76,-34, 92, 302, 596 ]
  mact = mkUnsafeArray shp $ VU.fromList xs2
  xs2 :: [Double]
  xs2 =  [ 0,  -66, -132, -198, -264
         , 0,  -49,  -98, -147, -196
         , 0,    2,    4,    6,    8
         , 0,   87,  174,  261,  348
         , 0,  206,  412,  618,  824 ]
  t1 = transpose ma == mb
  t2 = mabt == ma .*. mb
  t3 = transpose mact == ma .*. mc
  t4 = VU.fromList [20, 11, 20, 155, 596] == diagVec mabt
  in t1 && t2 && t3 && t4
