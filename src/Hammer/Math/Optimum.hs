{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Hammer.Math.Optimum
       ( lineSearch
       , bfgs
       , defaultBFGS
       , BFGScfg (..) 
       ) where

import Hammer.Math.Algebra

import Debug.Trace
dbg s x = trace (s ++ show x) x

-- =================================== Line Search =======================================

-- | Inexact line search algorithm using strong Wolfe conditions.
lineSearch :: (MultiVec v, DotProd v, AbelianGroup v)=>
              (v -> (Double, v)) -> v -> v -> Double
lineSearch func x p
  | dphi0 < 0 = upper 0 1
  | otherwise = 1
  where
    dir = p
    c1  = 0.0001
    c2  = 0.3

    eval a = let
      (f, g) = func (x &+ a *& dir)
      in (f, dir &. g)

    (phi0, dphi0) = eval 0

    upper :: Int -> Double -> Double
    upper n1 a
      | n1 > 200   = trace "Cant find: upper" a
      | wolfe0     = zoom 0 (a / 2, a)
      | wolfe1     = zoom 0 (0, a)
      | wolfe2     = a
      | dphia >= 0 = zoom 0 (a, a / 2)
      | otherwise  = upper (n1+1) (a * 2)
      where
        (phia, dphia) = eval a
        phiOlda = fst $ eval (a/2)
        wolfe0  = (wolfe1 || phia >= phiOlda) && n1 > 0
        wolfe1  = phia > phi0 + c1 * a * dphi0
        wolfe2  = abs (dphia) <= -c2 * dphi0

    zoom :: Int -> (Double, Double) -> Double
    zoom n2 (al, ah)
      | n2 > 500               = trace "Cant find: zoom" a
      | abs (ah - al) < 1e-5   = a
      | wolfe1                 = zoom (n2+1) (al, a)
      | phia >= phial          = zoom (n2+1) (al, a)
      | wolfe2                 = a
      | dphia * (ah - al) >= 0 = zoom (n2+1) (a, al)
      | otherwise              = zoom (n2+1) (a, ah)
      where
        a = (al + ah) / 2
        (phia, dphia) = eval a
        phial = fst $ eval (al)
        wolfe1 = phia > phi0 + c1 * a * dphi0
        wolfe2 = abs (dphia) <= -c2 * dphi0

-- ======================================= BFGS ==========================================

data BFGScfg =
  BFGScfg
  { epsi   :: Double
  , tol    :: Double
  , niter  :: Int
  } deriving (Show)

defaultBFGS :: BFGScfg
defaultBFGS = BFGScfg { epsi = 1e-9, tol = 1e-7, niter = 100 }

-- | Quasi-Newton algorithm BFGS for function minimization. Implementation based on:
-- "Springer Series in Operations Research and Financial Engineering"
bfgs :: ( MultiVec v, DotProd v, AbelianGroup v, Tensor m v
        , Matrix m, LeftModule m v, MultSemiGroup m, MultiVec m)
        => BFGScfg -> (v -> (Double, v)) -> v -> v
bfgs BFGScfg{..} func x = go 0 (x, g, idmtx)
  where
    (_, g) = func x
    go counter (x0, g0, h0)
      | counter > niter       = trace "No conversion" x1
      | norm g1 < epsi        = x1
      | norm (x0 &- x1) < tol = x1
      | counter < 1           = go (counter + 1) (x1, g1, h1a)
      | otherwise             = go (counter + 1) (x1, g1, h1)
      where
        dir = neg (h0 *. g0)
        a1  = lineSearch func x0 dir
        x1  = x0 &+ a1 *& dir

        (_, g1) = func x1
        s0 = (x1 &- x0)
        y0 = (g1 &- g0)
        p0 = 1 / (y0 &. s0)

        -- Improve first H guess
        h1a = ((y0 &. s0) / (y0 &. y0)) *& idmtx
        -- Update H using estimation technique
        h1 = let
          ma = idmtx &- (p0 *& (outer s0 y0))
          mb = idmtx &- (p0 *& (outer y0 s0))
          mc = p0 *& (outer s0 s0)
          in ma .*. h0 .*. mb &+ mc

-- =================================== Test functions=====================================

testLS   = lineSearch (\(Vec2 x y) -> (x*x + y*y, Vec2 (2*x) (2*y))) (Vec2 1 0) (Vec2 (-4) (-2))
testBFGS = bfgs defaultBFGS func (Vec2 100 (431))
  where func (Vec2 x y) = dbg "func: " (2*x*x + 2*x*y + 2*y*y - 6*x, Vec2 (4*x + 2*y - 6) (2*x + 4*y))
