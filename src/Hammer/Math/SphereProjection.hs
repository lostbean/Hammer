module Hammer.Math.SphereProjection where

import Hammer.Math.Vector


-- | Define types of sphere to plane projections. 
data SphereProjection =
    Lambert      SphereProjSymm
  | Sterographic SphereProjSymm
  deriving (Show)

-- | Define symmetry types. Reflection through the origin (v = -v) or no symmetry at all. 
data SphereProjSymm =
    InvProjSymm
  | NoProjSymm
  deriving (Show)

-- | Project points from a sphere's surface (SO3) on a plane.
-- It projects only one half-sphere (+Z) or allow to symmetrize
-- a SO3 point by point reflection (invert direction of the vector SO3).
getSO3ProjFunc :: SphereProjection -> (Vec3 -> Maybe Vec2)
getSO3ProjFunc projType = case projType of
  Lambert      NoProjSymm  -> lambert . normalize
  Lambert      InvProjSymm -> lambert . symmetrize . normalize
  Sterographic NoProjSymm  -> stero   . normalize
  Sterographic InvProjSymm -> stero   . symmetrize . normalize
  _                        -> \x -> Nothing
  where
    symmetrize v@(Vec3 x y z)
      | z > 0 = v
      | otherwise = neg v
    -- Here are implemented Cartesian -> Cartesian transformations.
    -- lambert transformation: spherical coord. (theta, omega) => polar coord. (theta, 2* sin (omega/2))
    -- where theta is the azimuthal angle to X axis and omega is the angle to Z axis
    lambert (Vec3 x y z)
      | z < 0     = Nothing
      | otherwise = let
        k = sqrt(2/(1+z))
        x' = k * x
        y' = k * y
        in Just $ Vec2 x' y'
    -- Sterographic transformation: spherical coord. (theta, omega) => polar coord. (theta, 2* tan (omega/2))
    -- where theta is the azimuthal angle to X axis and omega is the angle to Z axis
    stero (Vec3 x y z)
      | z < 0     = Nothing
      | otherwise = let
        k = (1+ z)
        x' = x / k
        y' = y / k
        in Just $ Vec2 x' y'

-- | Get the maximum distance form origin (max. radius) of a given projection.
getSO3ProjMaxDist :: SphereProjection -> Double
getSO3ProjMaxDist projType = case projType of
  Lambert      _ -> sqrt (2)
  Sterographic _ -> 1
  
