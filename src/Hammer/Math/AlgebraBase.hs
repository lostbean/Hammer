{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Hammer.Math.AlgebraBase  where

--------------------------------------------------------------------------------
-- class declarations
class AbelianGroup g where
  (&+) :: g -> g -> g
  (&-) :: g -> g -> g
  neg  :: g -> g
  zero :: g

infixl 6 &+
infixl 6 &- 

vecSum :: AbelianGroup g => [g] -> g
vecSum l = foldl (&+) zero l 

class MultSemiGroup r where
  (.*.) :: r -> r -> r
  one   :: r

class (AbelianGroup r, MultSemiGroup r) => Ring r 

infixl 7 .*. 

-- was: ringProduct :: Ring r => [r] -> r
semigroupProduct :: MultSemiGroup r => [r] -> r 
semigroupProduct l = foldl (.*.) one l

class LeftModule r m where
  lmul :: r -> m -> m
  (*.) :: r -> m -> m
  (*.) = lmul

class RightModule m r where
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

class AbelianGroup v => MultiVec v where
  mapVec    :: (Double -> Double) -> v -> v
  scalarMul :: Double -> v -> v
  (*&) ::      Double -> v -> v 
  (&*) ::      v -> Double -> v 
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
  norm    :: v -> Double
  normsqr :: v -> Double
  len     :: v -> Double
  lensqr  :: v -> Double
  len = norm
  lensqr = normsqr
  dotprod :: v -> v -> Double
  normsqr v = (v &. v)  
  norm = sqrt . lensqr
  dotprod = (&.)

infix 7 &.

{-# RULES
"len/square 1"   forall x.  (len x)*(len x) = lensqr x
"len/square 2"   forall x.  (len x)^2 = lensqr x
"norm/square 1"  forall x.  (norm x)*(norm x) = normsqr x
"norm/square 2"  forall x.  (norm x)^2 = normsqr x
  #-}

normalize :: (MultiVec v, DotProd v) => v -> v
normalize v = scalarMul (1.0/(len v)) v

distance :: (MultiVec v, DotProd v) => v -> v -> Double
distance x y = norm (x &- y)

-- | the angle between two vectors
angle :: (MultiVec v, DotProd v) => v -> v -> Double 
angle x y = acos $ (x &. y) / (norm x * norm y)

-- | the angle between two unit vectors
angle' {- ' CPP is sensitive to primes -} :: (MultiVec v, UnitVector v u, DotProd v) => u -> u -> Double 
angle' x y = acos (fromNormal x &. fromNormal y)

{-# RULES
"normalize is idempotent"  forall x. normalize (normalize x) = normalize x
  #-}

class (MultiVec v, DotProd v) => UnitVector v u | v->u, u->v  where
  mkNormal         :: v -> u       -- ^ normalizes the input
  toNormalUnsafe   :: v -> u       -- ^ does not normalize the input!
  fromNormal       :: u -> v
  fromNormalRadius :: Double -> u -> v
  fromNormalRadius t n = t *& fromNormal n 

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

class HasCoordinates v x | v->x where
  _1 :: v -> x
  _2 :: v -> x
  _3 :: v -> x
  _4 :: v -> x      

-- | conversion between vectors (and matrices) of different dimensions
class Extend u v where
  extendTailZero :: u -> v          -- ^ example: @extendTailZero (Vec2 5 6) = Vec4 5 6 0 0@
  extendTailWith :: Double -> u -> v   -- ^ example: @extendTailWith 1 (Vec2 5 6) = Vec4 5 6 1 1@
  trimTail       :: v -> u          -- ^ example: @trimTail (Vec4 5 6 7 8) = Vec2 5 6@
  extendHeadZero :: u -> v          -- ^ example: @extendHeadZero (Vec2 5 6) = Vec4 0 0 5 6@
  extendHeadWith :: Double -> u -> v   -- ^ example: @extendHeadWith 1 (Vec2 5 6) = Vec4 1 1 5 6@
  trimHead       :: v -> u          -- ^ example: @trimHead (Vec4 5 6 7 8) = Vec2 7 8@

-- | makes a diagonal matrix from a vector
class Diagonal s t | t->s where
  diag :: s -> t

class Matrix m where
  transpose :: m -> m 
  inverse :: m -> m
  idmtx :: m

{-# RULES
"transpose is an involution"  forall m. transpose (transpose m) = m
"inverse is an involution"    forall m. inverse (inverse m) = m
  #-}
  
class Matrix m => Orthogonal m o | m->o, o->m where  
  fromOrtho     :: o -> m 
  toOrthoUnsafe :: m -> o
  
class (AbelianGroup m, Matrix m) => MatrixNorms m where
  frobeniusNorm  :: m -> Double       -- ^ the frobenius norm (= euclidean norm in the space of matrices)
  matrixDistance :: m -> m -> Double  -- ^ euclidean distance in the space of matrices
  operatorNorm   :: m -> Double       -- ^ (euclidean) operator norm (not implemented yet)
  matrixDistance m n = frobeniusNorm (n &- m)
  operatorNorm = error "operatorNorm: not implemented yet"
  
-- | Outer product (could be unified with Diagonal?)
class Tensor t v | t->v where
  outer :: v -> v -> t
    
class Determinant m where
  det :: m -> Double    

class Dimension a where
  dim :: a -> Int
     
-- | Householder matrix, see <http://en.wikipedia.org/wiki/Householder_transformation>.  
-- In plain words, it is the reflection to the hyperplane orthogonal to the input vector.
householder :: (MultiVec v, UnitVector v u, Matrix m, MultiVec m, Tensor m v) => u -> m
householder u = idmtx &- (2 *& outer v v) 
  where v = fromNormal u

householderOrtho :: (MultiVec v, UnitVector v u, Matrix m, MultiVec m, Tensor m v, Orthogonal m o) => u -> o
householderOrtho = toOrthoUnsafe . householder

-- | \"Projective\" matrices have the following form: the top left corner
-- is an any matrix, the bottom right corner is 1, and the top-right
-- column is zero. These describe the affine orthogonal transformation of
-- the space one dimension less.
class (MultiVec v, Orthogonal n o, Diagonal v n) => Projective v n o m p 
    | m->p, p->m, p->o, o->p, p->n, n->p, p->v, v->p, n->o, n->v, v->n where
  fromProjective     :: p -> m
  toProjectiveUnsafe :: m -> p
  orthogonal         :: o -> p
  linear             :: n -> p
  translation        :: v -> p
  scaling            :: v -> p

--------------------------------------------------------------------------------
-- Vec / Mat datatypes
 
data Vec2 = Vec2 {-# UNPACK #-} !Double {-# UNPACK #-} !Double 
  deriving (Eq,Read,Show)
data Vec3 = Vec3 {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double 
  deriving (Eq,Read,Show)
data Vec4 = Vec4 {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double 
  deriving (Eq,Read,Show)

-- | The components are /row/ vectors 
data Mat2 = Mat2 !Vec2 !Vec2              deriving (Eq,Read,Show)
data Mat3 = Mat3 !Vec3 !Vec3 !Vec3        deriving (Eq,Read,Show)
data Mat4 = Mat4 !Vec4 !Vec4 !Vec4 !Vec4  deriving (Eq,Read,Show)

mkVec2 :: (Double,Double) -> Vec2
mkVec3 :: (Double,Double,Double) -> Vec3
mkVec4 :: (Double,Double,Double,Double) -> Vec4

mkVec2 (x,y)     = Vec2 x y 
mkVec3 (x,y,z)   = Vec3 x y z
mkVec4 (x,y,z,w) = Vec4 x y z w

  