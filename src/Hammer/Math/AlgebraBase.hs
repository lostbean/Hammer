{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DeriveGeneric              #-}

module Hammer.Math.AlgebraBase  where

import GHC.Generics (Generic)
import Data.Binary  (Binary)

--------------------------------------------------------------------------------
-- class declarations
class AbelianGroup g where
  (&+) :: g -> g -> g
  (&-) :: g -> g -> g
  neg  :: g -> g
  zero :: g

infixl 6 &+
infixl 6 &-

class MultSemiGroup r where
  (.*.) :: r -> r -> r
  one   :: r

class (AbelianGroup r, MultSemiGroup r) => Ring r

infixl 7 .*.

class LeftModule r m | r -> m, m -> r where
  lmul :: r -> m -> m
  (*.) :: r -> m -> m
  (*.) = lmul

class RightModule m r | r -> m, m -> r where
  rmul :: m -> r -> m
  (.*) :: m -> r -> m
  (.*) = rmul

-- I'm not really sure about this.. may actually degrade the performance in some cases?
{- RULES
 "matrix multiplication left"   forall m n x.  (n .*. m) *. x = n *. (m *. x)
 "matrix multiplication right"  forall m n x.  x .* (m .*. n) = (x .* m) .* n
 -}

infixr 7 *.
infixl 7 .*

class (AbelianGroup v)=> MultiVec v where
  mapVec    :: (Double -> Double) -> v -> v
  scalarMul :: Double -> v -> v
  (*&)      :: Double -> v -> v
  (&*)      :: v -> Double -> v
  (*&) s v = scalarMul s v
  (&*) v s = scalarMul s v

infixr 7 *&
infixl 7 &*

{-# RULES
"scalar multiplication left"   forall s t x.  t *& (s *& x) = (t*s) *& x
"scalar multiplication right"  forall s t x.  (x &* s) &* t = x &* (s*t)
  #-}

class DotProd v where
  (&.) :: v -> v -> Double

  norm :: v -> Double
  norm = sqrt . lensqr

  normsqr :: v -> Double
  normsqr v = (v &. v)

  len :: v -> Double
  len = norm

  lensqr :: v -> Double
  lensqr = normsqr

  dotprod :: v -> v -> Double
  dotprod = (&.)

infix 7 &.

{-# RULES
"len/square 1"   forall x.  (len x)*(len x) = lensqr x
"len/square 2"   forall x.  (len x)^2 = lensqr x
"norm/square 1"  forall x.  (norm x)*(norm x) = normsqr x
"norm/square 2"  forall x.  (norm x)^2 = normsqr x
 #-}

class (MultiVec v, DotProd v) => UnitVector v u | v -> u, u -> v where
  mkNormal         :: v -> u       -- ^ normalizes the input
  toNormalUnsafe   :: v -> u       -- ^ does not normalize the input!
  fromNormal       :: u -> v
  fromNormalRadius :: Double -> u -> v
  fromNormalRadius t n = t *& fromNormal n

-- | Cross product
class CrossProd v where
  crossprod :: v -> v -> v
  (&^)      :: v -> v -> v
  (&^) = crossprod

-- | Pointwise multiplication
class Pointwise v where
  pointwise :: v -> v -> v
  (&!)      :: v -> v -> v
  (&!) = pointwise

infix 7 &^
infix 7 &!

class HasCoordinates v x | v -> x where
  _1 :: v -> x
  _2 :: v -> x
  _3 :: v -> x
  _4 :: v -> x

-- | Conversion between vectors (and matrices) of different dimensions.
class Extend u v where
  extendTailZero :: u -> v           -- ^ example: @extendTailZero (Vec2 5 6) = Vec4 5 6 0 0@
  extendTailWith :: Double -> u -> v -- ^ example: @extendTailWith 1 (Vec2 5 6) = Vec4 5 6 1 1@
  trimTail       :: v -> u           -- ^ example: @trimTail (Vec4 5 6 7 8) = Vec2 5 6@
  extendHeadZero :: u -> v           -- ^ example: @extendHeadZero (Vec2 5 6) = Vec4 0 0 5 6@
  extendHeadWith :: Double -> u -> v -- ^ example: @extendHeadWith 1 (Vec2 5 6) = Vec4 1 1 5 6@
  trimHead       :: v -> u           -- ^ example: @trimHead (Vec4 5 6 7 8) = Vec2 7 8@

-- ================================== Matrix ==================================

-- | Makes a diagonal matrix from a vector and extracts the diagonal from matrices.
class Diagonal s t | t -> s where
  diagMtx :: s -> t
  diagVec :: t -> s

class (Transposable a, Inversable a, IdMatrix a)=> Matrix a

class Transposable m where
  transpose :: m -> m

class Inversable m where
  inverse   :: m -> m

class IdMatrix m where
  idmtx     :: m

{-# RULES
"transpose is an involution"  forall m. transpose (transpose m) = m
"inverse is an involution"    forall m. inverse (inverse m) = m
 #-}

class (Transposable m, IdMatrix m, Inversable m) => Orthogonal m o | m -> o, o -> m where
  fromOrtho     :: o -> m
  toOrthoUnsafe :: m -> o

class Hessenberg m where
  -- | Find the tridiagonalization of symmetric matrices or transforms non-symmetric
  -- matrices to a Hessenberg form. Uses Householder transformation.
  hessen :: m -> m

-- | Class of orthogonalization function to calculate the orthonormal matrix of an
-- m-by-m square matrix A with. This can be used for QR decompositon where Q is an
-- orthogonal matrix and R is an upper triangular matrix.
class OrthoMatrix m where
  -- | Calculates the orthogonal matrix using the Householder reflection (or Householder
  -- transformation).
  orthoRowsHouse :: (Transposable m)=> m -> m
  orthoRowsHouse = transpose . orthoColsHouse . transpose

  -- | Implements the modified Gram-Schmidt algorithm to calculate an orthonormal basis of a
  -- matrix. The modified version has more stability in finite precision calculations.
  -- This particular implementation does the calculation over the matrix's rows.
  orthoRowsGram :: m -> m

  -- | Same as @orthoRowsBasis@ but applied to the matrix's columns. It has a default instance.
  orthoColsHouse :: m -> m

  -- | Same as @orthoRowsBasis@ but applied to the matrix's columns. It has a default instance.
  orthoColsGram :: (Transposable m)=> m -> m
  orthoColsGram = transpose . orthoRowsGram . transpose

-- | Outer product (could be unified with Diagonal?)
class Tensor t v | t -> v where
  outer :: v -> v -> t

class Determinant m where
  det :: m -> Double

class Dimension a where
  dim :: a -> Int

class PrettyShow v where
  -- | Pretty print things. It might not be fast but it's pretty.
  showPretty :: v -> String

-- | Allows to treat algebra linear elements as containers. It operates at
-- vector (rank 1) level.
class VecFunctor a b | a -> b where
  -- | Map a function over the element.
  vecMap     :: (b -> b) -> a -> a
  -- | Right fold a reduction function over the element. The initial
  -- value is the last value of the element.
  vecFoldr   :: (b -> b -> b) -> a -> b
  -- | Zip two elements of the same type with a function.
  vecZipWith :: (b -> b -> b) -> a -> a -> a

-- | Allows to treat algebra linear elements as containers. It operates at
-- matrix (rank 2) level.
class MatFunctor a where
  -- | Map a function over the element.
  matMap     :: (Double -> Double) -> a -> a
  -- | Right fold a reduction function over the element. The initial
  -- value is the last value of the element.
  matFoldr   :: (Double -> Double -> Double) -> a -> Double
  -- | Zip two elements of the same type with a function.
  matZipWith :: (Double -> Double -> Double) -> a -> a -> a

-- ================================== Vec / Mat datatypes ================================

data Vec2 = Vec2 {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Eq, Read, Show, Generic)
data Vec3 = Vec3 {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Eq, Read, Show, Generic)
data Vec4 = Vec4 {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Eq, Read, Show, Generic)

-- | The components are /row/ vectors
data Mat2 = Mat2 !Vec2 !Vec2              deriving (Eq, Read, Show, Generic)
data Mat3 = Mat3 !Vec3 !Vec3 !Vec3        deriving (Eq, Read, Show, Generic)
data Mat4 = Mat4 !Vec4 !Vec4 !Vec4 !Vec4  deriving (Eq, Read, Show, Generic)

-- ================================== Binary Serialization ===============================

instance Binary Vec2
instance Binary Vec3
instance Binary Vec4

instance Binary Mat2
instance Binary Mat3
instance Binary Mat4

-- ====================================== Derived functions ==============================

normalize :: (MultiVec v, DotProd v) => v -> v
normalize v = scalarMul (1.0/(len v)) v

{-# RULES "normalize is idempotent"  forall x. normalize (normalize x) = normalize x #-}

distance :: (MultiVec v, DotProd v) => v -> v -> Double
distance x y = norm (x &- y)

-- | the angle between two vectors
angle :: (MultiVec v, DotProd v) => v -> v -> Double
angle x y = acosSafe $ (x &. y) / (norm x * norm y)

-- | the angle between two unit vectors
angle' :: (MultiVec v, UnitVector v u, DotProd v) => u -> u -> Double
angle' x y = acosSafe (fromNormal x &. fromNormal y)

-- | Safe version of 'acos' function where its boudaries (@1@ and @-1@) acepts a small
-- truncation error like @1.00000000000001@ instead of returning @NaN@. The value is
-- round to the closest valid boundary.
acosSafe :: (Floating a, Ord a)=> a -> a
acosSafe x
  -- error limmit 1e-12
  | x >  1 && x < ( 1.000000000001) = acos 1
  | x < -1 && x > (-1.000000000001) = acos (-1)
  | otherwise                       = acos x
{-# INLINE acosSafe #-}

-- | Projects the first vector down to the hyperplane orthogonal to the second (unit) vector
project' :: (MultiVec v, UnitVector v u, DotProd v) => v -> u -> v
project' what dir = projectUnsafe what (fromNormal dir)

-- | Direction (second argument) is assumed to be a /unit/ vector!
projectUnsafe :: (MultiVec v, DotProd v) => v -> v -> v
projectUnsafe what dir = what &- dir &* (what &. dir)

project :: (MultiVec v, DotProd v) => v -> v -> v
project what dir = what &- dir &* ((what &. dir) / (dir &. dir))

-- | Since unit vectors are not a group, we need a separate function.
flipNormal :: UnitVector v n => n -> n
flipNormal = toNormalUnsafe . neg . fromNormal

qrHouse :: (OrthoMatrix m, MultSemiGroup m, Transposable m)=> m -> (m, m)
qrHouse m = (q, r)
  where q = orthoColsHouse m
        r = transpose q .*. m

qrGram :: (OrthoMatrix m, MultSemiGroup m, Transposable m)=> m -> (m, m)
qrGram m = (transpose q, r)
  where q = orthoRowsGram $ transpose m
        r = q .*. m

-- | Calculates the eigenpairs (eigenvalues and eigenvector) of a given
-- *symmetric* matrix. It uses only OrthoMatrix decomposition. It can obtain correct
-- eigenvalues only (the eigenvectors should be post processed) if the
-- input matrix is a tridiagonal symmetric matrix or a upper Hessenberg
-- non-symmetric matrix.
symmEigen :: ( OrthoMatrix m, MultSemiGroup m, MatFunctor m
             , Diagonal v m , Dimension m, Transposable m
             , VecFunctor v Double)=> m -> (m, v)
symmEigen m = let
  (q0, r0) = qrGram m
  eigen !u !a !count
    | count <= 0      = (u, diagVec a)
    | offdiag < limit = (u, diagVec a)
    | otherwise       = eigen (u .*. q) (r .*. q) (count - 1)
    where
      (diag, offdiag) = diagOffDiag a
      epsilon = 1e-10
      limit   = max (epsilon * diag) epsilon
      (q, r)  = qrGram a
  in eigen q0 (r0 .*. q0) (10 * dim m)

-- | Householder matrix, see <http://en.wikipedia.org/wiki/Householder_transformation>.
-- In plain words, it is the reflection to the hyperplane orthogonal to the input vector.
-- The input vector is normalized before the Householder transformation.
householder :: (MultiVec v, DotProd v, IdMatrix m, MultiVec m, Tensor m v) => v -> m
householder v
  | l > 0     = idmtx &- ((2 / l) *& (outer v v))
  | otherwise = idmtx
  where l = normsqr v

mkVec2 :: (Double, Double) -> Vec2
mkVec3 :: (Double, Double, Double) -> Vec3
mkVec4 :: (Double, Double, Double, Double) -> Vec4

mkVec2 (x,y)     = Vec2 x y
mkVec3 (x,y,z)   = Vec3 x y z
mkVec4 (x,y,z,w) = Vec4 x y z w

unVec2 :: Vec2 -> (Double, Double)
unVec3 :: Vec3 -> (Double, Double, Double)
unVec4 :: Vec4 -> (Double, Double, Double, Double)

unVec2 (Vec2 x y)     = (x,y)
unVec3 (Vec3 x y z)   = (x,y,z)
unVec4 (Vec4 x y z w) = (x,y,z,w)

-- ============================ DO NOT EXPORT OrthoMatrix algorithms ========================

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

-- =========================================================================================
