{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Hammer.Math.AlgebraMat where

import           Data.List                   (intercalate)
import           Data.List                   (foldl')

import           Foreign
import           System.Random
  
import           Hammer.Math.AlgebraBase
import           Hammer.Math.AlgebraVec()

--------------------------------------------------------------------------------                    
-- Mat2 instances

instance HasCoordinates Mat2 Vec2 where
  _1 (Mat2 x _) = x
  _2 (Mat2 _ y) = y
  _3 _ = error "has only 2 coordinates"
  _4 _ = error "has only 2 coordinates"

instance Matrix Mat2 where
  transpose (Mat2 row1 row2) = 
    Mat2 (Vec2 (_1 row1) (_1 row2)) 
         (Vec2 (_2 row1) (_2 row2)) 
  idmtx = Mat2 (Vec2 1 0) (Vec2 0 1)
  inverse (Mat2 (Vec2 a b) (Vec2 c d)) = 
    Mat2 (Vec2 (d*r) (-b*r)) (Vec2 (-c*r) (a*r)) 
    where r = 1.0 / (a*d - b*c)

instance AbelianGroup Mat2 where
  (&+) (Mat2 r1 r2) (Mat2 s1 s2) = Mat2 (r1 &+ s1) (r2 &+ s2)
  (&-) (Mat2 r1 r2) (Mat2 s1 s2) = Mat2 (r1 &- s1) (r2 &- s2)
  neg  (Mat2 r1 r2)              = Mat2 (neg r1) (neg r2)  
  zero = Mat2 zero zero  
  
instance MultiVec Mat2 where
  scalarMul s (Mat2 r1 r2) = Mat2 (g r1) (g r2) where g = scalarMul s
  mapVec    f (Mat2 r1 r2) = Mat2 (g r1) (g r2) where g = mapVec f

instance MultSemiGroup Mat2 where
  (.*.) (Mat2 r1 r2) n = 
    let (Mat2 c1 c2) = transpose n
    in Mat2 (Vec2 (r1 &. c1) (r1 &. c2))
            (Vec2 (r2 &. c1) (r2 &. c2))
  one = idmtx 

instance Ring Mat2

instance LeftModule Mat2 Vec2 where
  lmul (Mat2 row1 row2) v = Vec2 (row1 &. v) (row2 &. v) 
  
instance RightModule Vec2 Mat2 where
  rmul v mt = lmul (transpose mt) v

instance Diagonal Vec2 Mat2 where
  diagMtx (Vec2 x y) = Mat2 (Vec2 x 0) (Vec2 0 y)
  diagVec m = Vec2 (_1 $ _1 m) (_2 $ _2 m)

instance OrthoMatrix Mat2 where
  orthoColsHouse = transpose . getQ

  orthoRowsGram (Mat2 a1 a2) = let
    e1 = normalize a1
    e2 = normalize $ foldl' schimi a2 [e1]
    in Mat2 e1 e2

instance Tensor Mat2 Vec2 where
  outer (Vec2 a b) (Vec2 x y) = Mat2
    (Vec2 (a*x) (a*y))
    (Vec2 (b*x) (b*y))

instance Determinant Mat2 where
  det (Mat2 (Vec2 a b) (Vec2 c d)) = a*d - b*c 

instance Storable Mat2 where
  sizeOf    _ = 2 * sizeOf (undefined::Vec2)
  alignment _ = alignment  (undefined::Vec2)
  
  peek q = do
    let p = castPtr q :: Ptr Vec2
        k = sizeOf (undefined::Vec2)
    r1 <- peek        p 
    r2 <- peekByteOff p k
    return (Mat2 r1 r2)
    
  poke q (Mat2 r1 r2) = do
    let p = castPtr q :: Ptr Vec2
        k = sizeOf (undefined::Vec2)
    poke        p   r1
    pokeByteOff p k r2

instance Random Mat2 where
  random = randomR (Mat2 v1 v1 , Mat2 v2 v2) where 
    v1 = Vec2 (-1) (-1) 
    v2 = Vec2   1    1
  randomR (Mat2 a b, Mat2 c d) gen = 
    let (x,gen1) = randomR (a,c) gen
        (y,gen2) = randomR (b,d) gen1
    in (Mat2 x y, gen2)
          
instance Dimension Mat2 where dim _ = 2
     
instance Pointwise Mat2 where
  pointwise (Mat2 x1 y1) (Mat2 x2 y2) = Mat2 (x1 &! x2) (y1 &! y2)
        
instance VecFunctor Mat2 Vec2 where
  vecMap     f (Mat2 a b) = Mat2 (f a) (f b)
  vecFoldr   f (Mat2 a b) = f a b
  vecZipWith f (Mat2 a1 b1) (Mat2 a2 b2) = Mat2 (f a1 a2) (f b1 b2)
      
instance MatFunctor Mat2 where
  matMap     f (Mat2 a b) = Mat2 (vecMap f a) (vecMap f b)
  matFoldr   f (Mat2 a b) = f (vecFoldr f a) (vecFoldr f b)
  matZipWith f (Mat2 a1 b1) (Mat2 a2 b2) = Mat2 (vecZipWith f a1 a2) (vecZipWith f b1 b2)
 
instance PrettyShow Mat2 where
  showPretty (Mat2 a b) = intercalate "\n" ["", showPretty a, showPretty b]
  
--------------------------------------------------------------------------------   
-- Mat3 instances

instance HasCoordinates Mat3 Vec3 where
  _1 (Mat3 x _ _) = x
  _2 (Mat3 _ y _) = y
  _3 (Mat3 _ _ z) = z
  _4 _ = error "has only 3 coordinates"  

instance Matrix Mat3 where

  transpose (Mat3 row1 row2 row3) = 
    Mat3 (Vec3 (_1 row1) (_1 row2) (_1 row3)) 
         (Vec3 (_2 row1) (_2 row2) (_2 row3)) 
         (Vec3 (_3 row1) (_3 row2) (_3 row3)) 
         
  idmtx = Mat3 (Vec3 1 0 0) (Vec3 0 1 0) (Vec3 0 0 1)
  
  inverse (Mat3 (Vec3 a b c) (Vec3 e f g) (Vec3 i j k)) = 
    Mat3 (Vec3 (d11*r) (d21*r) (d31*r))  
         (Vec3 (d12*r) (d22*r) (d32*r))  
         (Vec3 (d13*r) (d23*r) (d33*r))  
    where
      r = 1.0 / ( a*d11 + b*d12 + c*d13 )

      d11 = f*k - g*j
      d12 = g*i - e*k
      d13 = e*j - f*i

      d31 = b*g - c*f
      d32 = c*e - a*g
      d33 = a*f - b*e

      d21 = c*j - b*k 
      d22 = a*k - c*i 
      d23 = b*i - a*j 

instance AbelianGroup Mat3 where
  (&+) (Mat3 r1 r2 r3) (Mat3 s1 s2 s3) = Mat3 (r1 &+ s1) (r2 &+ s2) (r3 &+ s3)
  (&-) (Mat3 r1 r2 r3) (Mat3 s1 s2 s3) = Mat3 (r1 &- s1) (r2 &- s2) (r3 &- s3)
  neg  (Mat3 r1 r2 r3)                 = Mat3 (neg r1) (neg r2) (neg r3) 
  zero = Mat3 zero zero zero 

instance MultiVec Mat3 where
  scalarMul s (Mat3 r1 r2 r3) = Mat3 (g r1) (g r2) (g r3) where g = scalarMul s
  mapVec    f (Mat3 r1 r2 r3) = Mat3 (g r1) (g r2) (g r3) where g = mapVec f

instance MultSemiGroup Mat3 where
  (.*.) (Mat3 r1 r2 r3) n = 
    let (Mat3 c1 c2 c3) = transpose n
    in Mat3 (Vec3 (r1 &. c1) (r1 &. c2) (r1 &. c3))
            (Vec3 (r2 &. c1) (r2 &. c2) (r2 &. c3))
            (Vec3 (r3 &. c1) (r3 &. c2) (r3 &. c3))
  one = idmtx 

instance Ring Mat3

instance LeftModule Mat3 Vec3 where
  lmul (Mat3 row1 row2 row3) v = Vec3 (row1 &. v) (row2 &. v) (row3 &. v)
  
instance RightModule Vec3 Mat3 where
  rmul v mt = lmul (transpose mt) v

instance Diagonal Vec3 Mat3 where
  diagMtx (Vec3 x y z) = Mat3 (Vec3 x 0 0) (Vec3 0 y 0) (Vec3 0 0 z)
  diagVec m = Vec3 (_1 $ _1 m) (_2 $ _2 m) (_3 $ _3 m)
  
instance Hessenberg Mat3 where
  hessen m = q .*. m .*. q
    where q = getHH3 m
          
-- | Find the Householder transformation used for tridiagonalization of symmetric
-- matrices and for transforming non-symmetric matrices to a Hessenberg form.
getHH3 :: Mat3 -> Mat3
getHH3 m = let
  x = trimHead $ _1 $ transpose m
  a = let k = norm x in if _1 x > 0 then -k else k
  r = 0.5 * (a*a - (_1 x) * a)
  v = Vec2 (a/(2*r)) 0
  u = extendHeadZero $ v &- (1/(2*r)) *& x
  in householder u

instance OrthoMatrix Mat3 where
  orthoColsHouse m = let
    q1 = getQ m
    m1 = (trimHead $ q1 .*. m) :: Mat2
    q2 = extendHeadWith 1 $ getQ m1
    in transpose q1 .*. transpose q2

  orthoRowsGram (Mat3 a1 a2 a3) = let
    e1 = normalize a1
    e2 = normalize $ foldl' schimi a2 [e1]
    e3 = normalize $ foldl' schimi a3 [e1, e2]
    in Mat3 e1 e2 e3

instance Tensor Mat3 Vec3 where
  outer (Vec3 a b c) (Vec3 x y z) = Mat3
    (Vec3 (a*x) (a*y) (a*z))
    (Vec3 (b*x) (b*y) (b*z))
    (Vec3 (c*x) (c*y) (c*z))

instance Determinant Mat3 where
  det (Mat3 r1 r2 r3) = det (r1,r2,r3)

instance Storable Mat3 where
  sizeOf    _ = 3 * sizeOf (undefined::Vec3)
  alignment _ = alignment  (undefined::Vec3)
  
  peek q = do
    let p = castPtr q :: Ptr Vec3
        k = sizeOf (undefined::Vec3)
    r1 <- peek        p 
    r2 <- peekByteOff p (k  )
    r3 <- peekByteOff p (k+k)
    return (Mat3 r1 r2 r3)
    
  poke q (Mat3 r1 r2 r3) = do
    let p = castPtr q :: Ptr Vec3
        k = sizeOf (undefined::Vec3)
    poke        p       r1
    pokeByteOff p (k  ) r2
    pokeByteOff p (k+k) r3

instance Random Mat3 where
  random = randomR (Mat3 v1 v1 v1 , Mat3 v2 v2 v2) where
    v1 = Vec3 (-1) (-1) (-1)
    v2 = Vec3   1    1    1
  randomR (Mat3 a b c, Mat3 d e f) gen = 
    let (x,gen1) = randomR (a,d) gen
        (y,gen2) = randomR (b,e) gen1
        (z,gen3) = randomR (c,f) gen2  
    in (Mat3 x y z, gen3)
   
instance Dimension Mat3 where dim _ = 3

instance Pointwise Mat3 where
  pointwise (Mat3 x1 y1 z1) (Mat3 x2 y2 z2) = Mat3 (x1 &! x2) (y1 &! y2) (z1 &! z2)
    
instance VecFunctor Mat3 Vec3 where
  vecMap     f (Mat3 a b c) = Mat3 (f a) (f b) (f c)
  vecFoldr   f (Mat3 a b c) = f a (f b c)
  vecZipWith f (Mat3 a1 b1 c1) (Mat3 a2 b2 c2) = Mat3 (f a1 a2) (f b1 b2) (f c1 c2)

instance MatFunctor Mat3 where
  matMap     f (Mat3 a b c) = Mat3 (vecMap f a) (vecMap f b) (vecMap f c)
  matFoldr   f (Mat3 a b c) = f (vecFoldr f a) (f (vecFoldr f b) (vecFoldr f c))
  matZipWith f (Mat3 a1 b1 c1) (Mat3 a2 b2 c2) =
    Mat3 (vecZipWith f a1 a2) (vecZipWith f b1 b2) (vecZipWith f c1 c2)

instance PrettyShow Mat3 where
  showPretty (Mat3 a b c) = intercalate "\n" ["", showPretty a, showPretty b, showPretty c]
  
--------------------------------------------------------------------------------
-- Mat4 instances

instance HasCoordinates Mat4 Vec4 where
  _1 (Mat4 x _ _ _) = x
  _2 (Mat4 _ y _ _) = y
  _3 (Mat4 _ _ z _) = z
  _4 (Mat4 _ _ _ w) = w

instance Matrix Mat4 where
  transpose (Mat4 row1 row2 row3 row4) = 
    Mat4 (Vec4 (_1 row1) (_1 row2) (_1 row3) (_1 row4)) 
         (Vec4 (_2 row1) (_2 row2) (_2 row3) (_2 row4)) 
         (Vec4 (_3 row1) (_3 row2) (_3 row3) (_3 row4)) 
         (Vec4 (_4 row1) (_4 row2) (_4 row3) (_4 row4)) 
  idmtx = Mat4 (Vec4 1 0 0 0) (Vec4 0 1 0 0) (Vec4 0 0 1 0) (Vec4 0 0 0 1)
  inverse = error "inverse/Mat4: not implemented yet"

instance AbelianGroup Mat4 where
  (&+) (Mat4 r1 r2 r3 r4) (Mat4 s1 s2 s3 s4) = Mat4 (r1 &+ s1) (r2 &+ s2) (r3 &+ s3) (r4 &+ s4)
  (&-) (Mat4 r1 r2 r3 r4) (Mat4 s1 s2 s3 s4) = Mat4 (r1 &- s1) (r2 &- s2) (r3 &- s3) (r4 &- s4)
  neg  (Mat4 r1 r2 r3 r4)                    = Mat4 (neg r1) (neg r2) (neg r3) (neg r4) 
  zero = Mat4 zero zero zero zero
  
instance MultiVec Mat4 where
  scalarMul s (Mat4 r1 r2 r3 r4) = Mat4 (g r1) (g r2) (g r3) (g r4) where g = scalarMul s
  mapVec    f (Mat4 r1 r2 r3 r4) = Mat4 (g r1) (g r2) (g r3) (g r4) where g = mapVec f

instance MultSemiGroup Mat4 where
  (.*.) (Mat4 r1 r2 r3 r4) n = 
    let (Mat4 c1 c2 c3 c4) = transpose n
    in Mat4 (Vec4 (r1 &. c1) (r1 &. c2) (r1 &. c3) (r1 &. c4))
            (Vec4 (r2 &. c1) (r2 &. c2) (r2 &. c3) (r2 &. c4))
            (Vec4 (r3 &. c1) (r3 &. c2) (r3 &. c3) (r3 &. c4))
            (Vec4 (r4 &. c1) (r4 &. c2) (r4 &. c3) (r4 &. c4))
  one = idmtx 

instance Ring Mat4

instance LeftModule Mat4 Vec4 where
  lmul (Mat4 row1 row2 row3 row4) v = Vec4 (row1 &. v) (row2 &. v) (row3 &. v) (row4 &. v)
  
instance RightModule Vec4 Mat4 where
  rmul v mt = lmul (transpose mt) v

instance Diagonal Vec4 Mat4 where
  diagMtx (Vec4 x y z w) = Mat4 (Vec4 x 0 0 0) (Vec4 0 y 0 0) (Vec4 0 0 z 0) (Vec4 0 0 0 w)
  diagVec m = Vec4 (_1 $ _1 m) (_2 $ _2 m) (_3 $ _3 m) (_4 $ _4 m)
 
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
  r = 0.5 * (a*a - (_1 x) * a)
  v = Vec3 (a/(2*r)) 0 0
  u = extendHeadZero $ v &- (1/(2*r)) *& x
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

  orthoRowsGram (Mat4 a1 a2 a3 a4) = let
    e1 = normalize a1
    e2 = normalize $ foldl' schimi a2 [e1]
    e3 = normalize $ foldl' schimi a3 [e1, e2]
    e4 = normalize $ foldl' schimi a4 [e1, e2, e3]
    in Mat4 e1 e2 e3 e4

instance Tensor Mat4 Vec4 where
  outer (Vec4 a b c d) (Vec4 x y z w) = Mat4
    (Vec4 (a*x) (a*y) (a*z) (a*w))
    (Vec4 (b*x) (b*y) (b*z) (b*w))
    (Vec4 (c*x) (c*y) (c*z) (c*w))
    (Vec4 (d*x) (d*y) (d*z) (d*w))

instance Determinant Mat4 where
  det = error "det/Mat4: not implemented yet" 
  -- det (Mat4 r1 r2 r3 r4) = 

instance Storable Mat4 where
  sizeOf    _ = 4 * sizeOf (undefined::Vec4)
  alignment _ = alignment  (undefined::Vec4)
  
  peek q = do
    let p = castPtr q :: Ptr Vec4
        k = sizeOf (undefined::Vec4)
    r1 <- peek        p 
    r2 <- peekByteOff p (k  )
    r3 <- peekByteOff p (k+k)
    r4 <- peekByteOff p (3*k)
    return (Mat4 r1 r2 r3 r4)
    
  poke q (Mat4 r1 r2 r3 r4) = do
    let p = castPtr q :: Ptr Vec4
        k = sizeOf (undefined::Vec4)
    poke        p       r1
    pokeByteOff p (k  ) r2
    pokeByteOff p (k+k) r3
    pokeByteOff p (3*k) r4

instance Random Mat4 where
  random = randomR (Mat4 v1 v1 v1 v1, Mat4 v2 v2 v2 v2) where
    v1 = Vec4 (-1) (-1) (-1) (-1)
    v2 = Vec4   1    1    1    1
  randomR (Mat4 a b c d, Mat4 e f g h) gen = 
    let (x,gen1) = randomR (a,e) gen
        (y,gen2) = randomR (b,f) gen1
        (z,gen3) = randomR (c,g) gen2  
        (w,gen4) = randomR (d,h) gen3  
    in (Mat4 x y z w, gen4)
    
instance Dimension Mat4 where dim _ = 4
    
instance Pointwise Mat4 where
  pointwise (Mat4 x1 y1 z1 w1) (Mat4 x2 y2 z2 w2) = Mat4 (x1 &! x2) (y1 &! y2) (z1 &! z2) (w1 &! w2)
    
instance VecFunctor Mat4 Vec4 where
  vecMap     f (Mat4 a b c d) = Mat4 (f a) (f b) (f c) (f d)
  vecFoldr   f (Mat4 a b c d) = f a (f b (f c d))
  vecZipWith f (Mat4 a1 b1 c1 d1) (Mat4 a2 b2 c2 d2) = Mat4 (f a1 a2) (f b1 b2) (f c1 c2) (f d1 d2)

instance MatFunctor Mat4 where
  matMap     f (Mat4 a b c d) = Mat4 (vecMap f a) (vecMap f b) (vecMap f c) (vecMap f d)
  matFoldr   f (Mat4 a b c d) = f (vecFoldr f a) (f (vecFoldr f b) (f (vecFoldr f c) (vecFoldr f d)))
  matZipWith f (Mat4 a1 b1 c1 d1) (Mat4 a2 b2 c2 d2) =
    Mat4 (vecZipWith f a1 a2) (vecZipWith f b1 b2) (vecZipWith f c1 c2) (vecZipWith f d1 d2)
 
instance PrettyShow Mat4 where
  showPretty (Mat4 a b c d) = intercalate "\n" ["", showPretty a, showPretty b, showPretty c, showPretty d]

--------------------------------------------------------------------------------
-- Extend instances
                                    
instance Extend Mat2 Mat3 where
  extendHeadZero   (Mat2 p q) = Mat3 zero (extendHeadZero p) (extendHeadZero q)
  extendHeadWith w (Mat2 p q) = Mat3 (Vec3 w 0 0) (extendHeadZero p) (extendHeadZero q)
  trimHead       (Mat3 _ p q) = Mat2 (trimHead p) (trimHead q)
  extendTailZero   (Mat2 p q) = Mat3 (extendTailZero p) (extendTailZero q) zero
  extendTailWith w (Mat2 p q) = Mat3 (extendTailZero p) (extendTailZero q) (Vec3 0 0 w)
  trimTail       (Mat3 p q _) = Mat2 (trimTail p) (trimTail q)

instance Extend Mat2 Mat4 where
  extendHeadZero   (Mat2 p q) = Mat4 zero zero (extendHeadZero p) (extendHeadZero q)
  extendHeadWith w (Mat2 p q) = Mat4 (Vec4 w 0 0 0) (Vec4 0 w 0 0) (extendHeadZero p) (extendHeadZero q)
  trimHead     (Mat4 _ _ p q) = Mat2 (trimHead p) (trimHead q)
  extendTailZero   (Mat2 p q) = Mat4 (extendTailZero p) (extendTailZero q) zero zero
  extendTailWith w (Mat2 p q) = Mat4 (extendTailZero p) (extendTailZero q) (Vec4 0 0 w 0) (Vec4 0 0 0 w)
  trimTail     (Mat4 p q _ _) = Mat2 (trimTail p) (trimTail q)

instance Extend Mat3 Mat4 where
  extendHeadZero   (Mat3 p q r) = Mat4 zero (extendHeadZero p) (extendHeadZero q) (extendHeadZero r)
  extendHeadWith w (Mat3 p q r) = Mat4 (Vec4 w 0 0 0) (extendHeadZero p) (extendHeadZero q) (extendHeadZero r)
  trimHead       (Mat4 _ p q r) = Mat3 (trimHead p) (trimHead q) (trimHead r)
  extendTailZero   (Mat3 p q r) = Mat4 (extendTailZero p) (extendTailZero q) (extendTailZero r) zero
  extendTailWith w (Mat3 p q r) = Mat4 (extendTailZero p) (extendTailZero q) (extendTailZero r) (Vec4 0 0 0 w)
  trimTail (Mat4 p q r _) = Mat3 (trimTail p) (trimTail q) (trimTail r)

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
