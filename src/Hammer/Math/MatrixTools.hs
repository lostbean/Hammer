{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , FlexibleContexts
  , FlexibleInstances
  , TypeSynonymInstances
  , BangPatterns
  #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
module Hammer.Math.MatrixTools
  ( symmEigen
  , qrGram
  , qrHouse
  , project
  , project'
  , projectUnsafe
  , householder
  ) where

import Data.List (foldl')

import Hammer.Math.AlgebraBase
import Hammer.Math.AlgebraVec ()
import Hammer.Math.AlgebraMat ()

-- =======================================================================================

instance OrthoMatrix Mat2 where
  orthoColsHouse = transpose . getQ

  orthoRowsGram (Mat2 a1 a2) = Mat2 e1 e2
    where
      e1 = safeNormalize (Vec2 1 0) a1
      e2 = safeNormalize (Vec2 0 1) $ schimi a2 e1

-- =======================================================================================

instance Hessenberg Mat3 where
  hessen m = q .*. m .*. q
    where q = getHH3 m

-- | Find the Householder transformation used for tridiagonalization of symmetric
-- matrices and for transforming non-symmetric matrices to a Hessenberg form.
getHH3 :: Mat3 -> Mat3
getHH3 m = let
  x = trimHead $ _1 $ transpose m
  a = let k = norm x in if _1 x > 0 then -k else k
  r = 0.5 * (a * a - (_1 x) * a)
  v = Vec2 (a / (2 * r)) 0
  u = extendHeadZero $ v &- (1 / (2 * r)) *& x
  in householder u

instance OrthoMatrix Mat3 where
  orthoColsHouse m = let
    q1 = getQ m
    m1 = (trimHead $ q1 .*. m) :: Mat2
    q2 = extendHeadWith 1 $ getQ m1
    in transpose q1 .*. transpose q2

  orthoRowsGram (Mat3 a1 a2 a3) = Mat3 e1 e2 e3
    where
      e1 = safeNormalize (Vec3 1 0 0) a1
      e2 = safeNormalize (Vec3 0 1 0) $ foldl' schimi a2 [e1]
      e3 = safeNormalize (Vec3 0 0 1) $ foldl' schimi a3 [e1, e2]

-- =======================================================================================

instance Hessenberg Mat4 where
  hessen m = let
    q1  = getHH4 m
    a1  = q1 .*. m .*. q1
    a1s = trimHead a1
    q2  = extendHeadWith 1 $ getHH3 a1s
    in q2 .*. a1 .*. q2

getHH4 :: Mat4 -> Mat4
getHH4 m = let
  x = trimHead $ _1 $ transpose m
  a = let k = norm x in if _1 x > 0 then -k else k
  r = 0.5 * (a * a - (_1 x) * a)
  v = Vec3 (a / (2 * r)) 0 0
  u = extendHeadZero $ v &- (1 / ( 2 * r)) *& x
  in householder u

instance OrthoMatrix Mat4 where
  orthoColsHouse m = let
    q1 = getQ m
    m1 = (trimHead $ q1 .*. m)  :: Mat3

    q2 = getQ m1
    m2 = (trimHead $ q2 .*. m1) :: Mat2
    q3 = getQ m2

    q2e = extendHeadWith 1 q2
    q3e = extendHeadWith 1 q3
    in transpose q1 .*. transpose q2e .*. transpose q3e

  orthoRowsGram (Mat4 a1 a2 a3 a4) = Mat4 e1 e2 e3 e4
    where
      e1 = safeNormalize (Vec4 1 0 0 0) a1
      e2 = safeNormalize (Vec4 0 1 0 0) $ schimi a2 e1
      e3 = safeNormalize (Vec4 0 0 1 0) $ foldl' schimi a3 [e1, e2]
      e4 = safeNormalize (Vec4 0 0 0 1) $ foldl' schimi a4 [e1, e2, e3]

-- =======================================================================================

-- | Calculates the eigenpairs (eigenvalues and eigenvector) of a given
-- *symmetric* matrix. It uses only OrthoMatrix decomposition. It can obtain correct
-- eigenvalues only (the eigenvectors should be post processed) if the
-- input matrix is a tridiagonal symmetric matrix or a upper Hessenberg
-- non-symmetric matrix.
symmEigen :: ( OrthoMatrix m, MultSemiGroup m, MatFunctor m
             , Diagonal v m , Dimension m, Transposable m
             , VecFunctor v Double)=> m -> (m, v)
symmEigen m = eigen q0 (r0 .*. q0) (10 * dim m)
  where
    (q0, r0) = qrHouse m
    eigen !u !a !count
      | count <= 0      = (u, diagVec a)
      | offdiag < limit = (u, diagVec a)
      | otherwise       = eigen (u .*. q) (r .*. q) (count - 1)
      where
        (diag, offdiag) = diagOffDiag a
        epsilon = 1e-10
        limit   = max (epsilon * diag) epsilon
        (q, r)  = qrHouse a

-- =======================================================================================

-- | Projects the first vector down to the hyperplane orthogonal to the second (unit) vector
project' :: (MultiVec v, UnitVector v u, DotProd v) => v -> u -> v
project' what dir = projectUnsafe what (fromNormal dir)

-- | Direction (second argument) is assumed to be a /unit/ vector!
projectUnsafe :: (MultiVec v, DotProd v) => v -> v -> v
projectUnsafe what dir = what &- dir &* (what &. dir)

project :: (MultiVec v, DotProd v) => v -> v -> v
project what dir = what &- dir &* ((what &. dir) / (dir &. dir))

-- | QR decomposition using Householder method.
qrHouse :: (OrthoMatrix m, MultSemiGroup m, Transposable m)=> m -> (m, m)
qrHouse m = (q, r)
  where q = orthoColsHouse m
        r = transpose q .*. m

-- | QR decomposition using Gram-Schmidt method. Less unstable the 'qrHouse'.
-- It also doesn't work properly when the matrix in created by an self outer product
-- e.g. "outer v v"
qrGram :: (OrthoMatrix m, MultSemiGroup m, Transposable m)=> m -> (m, m)
qrGram m = (transpose q, r)
  where q = orthoRowsGram $ transpose m
        r = q .*. m

-- | Householder matrix, see <http://en.wikipedia.org/wiki/Householder_transformation>.
-- In plain words, it is the reflection to the hyperplane orthogonal to the input vector.
-- The input vector is normalized before the Householder transformation.
householder :: (MultiVec v, DotProd v, IdMatrix m, MultiVec m, Tensor m v) => v -> m
householder v
  | l > 0     = idmtx &- ((2 / l) *& (outer v v))
  | otherwise = idmtx
  where l = normsqr v

-- ============================ DO NOT EXPORT OrthoMatrix algorithms =====================

-- | Find the orthogonal component of the fisrt vector in relation to
-- the second vector. Used by the Gram-Schimidt algorithm.
schimi :: (MultiVec g, DotProd g)=> g -> g -> g
schimi a b
  | lenb > 0  = a &- projAonB
  | otherwise = a
  where
    lenb     = b &. b
    projAonB = b &* ((a &. b) / lenb)

-- | Find the Householder transformation matrix. This is an orthogonal matrix.
getQ :: ( Num a, Ord a, UnitVector v1 u, Tensor m v1, MultiVec m, IdMatrix m
        , Transposable m, HasCoordinates m v1, HasCoordinates v1 a, HasE1 v1)
        => m -> m
getQ m = let
  x = _1 $ transpose m
  l = norm x
  a = if _1 x > 0 then -l else l
  u = x &+ vece1 a
  in householder u

-- | The Gershgorin circle is the sum of the absolute values of the non-diagonal entries.
diagOffDiag :: (VecFunctor a1 Double, MatFunctor a, Diagonal a1 a)=> a -> (Double, Double)
diagOffDiag m = let
  diag  = vecFoldr (+) $ vecMap abs (diagVec m)
  total = matFoldr (+) $ matMap abs m
  in (diag, total - diag)

safeNormalize :: (MultiVec v, DotProd v) => v -> v -> v
safeNormalize fallback vec
  | l > 0     = vec &* (1 / l)
  | otherwise = fallback
  where l = norm vec

class HasE1 a where
  vece1 :: Double -> a
instance HasE1 Vec2 where
  vece1 x = Vec2 x 0
instance HasE1 Vec3 where
  vece1 x = Vec3 x 0 0
instance HasE1 Vec4 where
  vece1 x = Vec4 x 0 0 0

-- ================================== Test ==================================

testData1 :: Mat3 -- Source <http://en.wikipedia.org/wiki/QR_decomposition>
testData1 = Mat3 (Vec3 12 (-51) 4) (Vec3 6 167 (-68)) (Vec3 (-4) 24 (-41))

testData2 :: Mat3 -- Source <Orthogonal Bases and the QR Algorithm> <by Peter J. Olver>
testData2 = Mat3 (Vec3 2 1 0) (Vec3 1 3 (-1)) (Vec3 0 (-1) 6)

testData3 :: Mat4
testData3 = Mat4 (Vec4 0 10 3 9) (Vec4 10 12 6 15) (Vec4 3 6 0 7) (Vec4 9 15 7 8)

testData4 :: Mat4 -- Source <Orthogonal Bases and the QR Algorithm> <by Peter J. Olver>
testData4 = Mat4 (Vec4 4 1 (-1) 2) (Vec4 1 4 1 (-1)) (Vec4 (-1) 1 4 1) (Vec4 2 (-1) 1 4)

testData5 :: Mat2 -- Source <Orthogonal Bases and the QR Algorithm> <by Peter J. Olver>
testData5 = Mat2 (Vec2 2 1) (Vec2 1 3)

testQR :: (MultSemiGroup g, AbelianGroup g, OrthoMatrix g, Matrix g) => g -> g
testQR m = m &- (q .*. r)
  where (q, r) = qrHouse m

testEigen :: ( VecFunctor t Double, OrthoMatrix a, MultiVec a, MultSemiGroup a
             , Matrix a, MatFunctor a, LeftModule a b, HasCoordinates t Double
             , HasCoordinates a b, DotProd b, Dimension a, Diagonal t a) =>
             a -> [Double]
testEigen m = map (normsqr . foo) $ take n [(_1, _1), (_2, _2), (_3, _3), (_4, _4)]
  where
    n = dim m
    foo (f1, f2) = (m &- (f1 value) *& idmtx) *. (f2 $ transpose vec)
    (vec, value) = symmEigen m
