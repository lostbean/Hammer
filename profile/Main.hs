{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Data.Vector.Unboxed as VU

import           Data.List
import           Options.Applicative
import           Prelude

import           Hammer.MicroGraph
import           Hammer.VTK
import           Hammer.VoxBox
import           Hammer.VoxConn

import           TestGrainFinder

data Tester =
  Tester
  { run_test_GrainFinder    :: Maybe String
  , run_profile_GrainFinder :: Maybe String
  , run_profile_VTKRender   :: Maybe String
  , run_test_suit           :: Bool
  } deriving (Show)

tester :: Parser Tester
tester = Tester
  <$> (optional . strOption)
      (  long "grainfinder-test"
      <> short 'g'
      <> metavar "VTK_OUT"
      <> help "Test GrainFinder in a standard voxelized microstructure.")
  <*> (optional . strOption)
      (  long "grainfinder-profile"
      <> short 'G'
      <> metavar "VTK_OUT"
      <> help "Profile GrainFinder in a standard voxelized microstructure.")
  <*> (optional . strOption)
      (  long "vtkRender-profile"
      <> short 'r'
      <> metavar "VTK_OUT"
      <> help "Profile VTKRender module" )
   <*> switch
      (  long "test-suit"
      <> short 't'
      <> help "Run test suit." )

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> tester)
      ( fullDesc
      <> progDesc "Test and profile hammer library."
      <> header "Hammer library" )

run :: Tester -> IO ()
run Tester{..} = do
  if run_test_suit
    then runChecker
    else putStrLn "Skiping test suit."
  case run_profile_GrainFinder of
    Just x -> profile_GrainFinder x
    _      -> putStrLn "Skiping GrainFinder profile."

  case run_test_GrainFinder of
    Just x -> test_GrainFinder x
    _      -> putStrLn "Skiping GrainFinder test."

  case run_profile_VTKRender of
    Just x -> profile_VTKRender x
    _      -> putStrLn "Skiping VTKRender profile."

profile_VTKRender fout = let
  dx = 10
  dy = 10
  dz = 10
  vbox = VoxBox { dimension = mkStdVoxBoxRange (VoxBoxDim dx dy dz)
                , origin    = VoxBoxOrigin  0    0    0
                , spacing   = VoxelDim      0.25 0.25 0.25
                , grainID   = vec }
  vec   = VU.generate (dx*dy*dz) id
  vtk   = renderVoxBoxVTK vbox attrs
  attrs = [mkPointAttr "GrainID" (vec VU.!)]
  in writeUniVTKfile (fout ++ ".vtr") True vtk

profile_GrainFinder fout = let
  dx = 100
  dy = 100
  dz = 100
  rawVBox = VoxBox { dimension = mkStdVoxBoxRange (VoxBoxDim dx   dy   dz)
                   , origin    = VoxBoxOrigin  0    0    0
                   , spacing   = VoxelDim      0.25 0.25 0.25
                   , grainID   = vec }
  vec   = VU.replicate (dx*dy*dz) (2::Int)
  in case grainFinder (==) rawVBox of
    Just (vbox, _) -> do
      let
        vec   = VU.map unGrainID $ grainID vbox
        vtk   = renderVoxBoxVTK vbox attrs
        attrs = [mkPointAttr "GrainID" (\a -> vec VU.! a)]
      writeUniVTKfile (fout ++ ".vtr") True vtk
    _ -> putStrLn "Unable to find grains."

test_GrainFinder fout = case (getMicroVoxel . resetGrainIDs) <$> grainFinder (==) vboxTest of
  Nothing -> putStrLn "Sorry, I can't get the MicroVoxel"
  Just (micro, vboxGID) -> let
    vec   = grainID vboxGID
    vtk   = renderVoxBoxVTK vboxTest attrs
    attrs = [mkPointAttr "GrainID" (\a -> unGrainID $ vec VU.! a)]
    in do
      writeUniVTKfile (fout ++ ".vtr")        True   vtk
      writeUniVTKfile (fout ++ "_faces.vtp")  True $ renderMicroFacesVTK  vboxTest micro
      writeUniVTKfile (fout ++ "_edges.vtp")  True $ renderMicroEdgesVTK  vboxTest micro
      writeUniVTKfile (fout ++ "_vertex.vtp") True $ renderMicroVertexVTK vboxTest micro

vboxTest = VoxBox { dimension = mkStdVoxBoxRange (VoxBoxDim 21   15   1)
                  , origin    = VoxBoxOrigin  0    0    0
                  , spacing   = VoxelDim      1    1    1
                  , grainID   = g }
  where
    g :: VU.Vector Int
    g = VU.fromList
          [2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4
          ,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4
          ,2,2,2,2,2,3,2,1,1,1,1,1,4,4,3,4,4,4,4,4,4
          ,2,2,2,2,2,3,2,1,2,2,4,1,4,4,3,4,4,4,4,4,4
          ,2,2,2,2,2,3,2,1,1,2,4,1,4,4,3,4,4,4,4,4,4
          ,2,2,2,2,2,3,2,2,2,2,4,4,4,4,3,4,4,4,4,4,4
          ,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4
          ,2,2,2,2,3,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,3,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,3,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,3,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,3,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4]



vboxTest2 = VoxBox { dimension = mkStdVoxBoxRange (VoxBoxDim 21   15   5)
                  , origin    = VoxBoxOrigin  0    0    0
                  , spacing   = VoxelDim      1    1    1
                  , grainID   = g }
  where
    g :: VU.Vector Int
    g = VU.fromList
          [2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4

          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,3,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,3,3,3,3,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,3,3,2,3,3,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,3,3,2,3,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,3,2,3,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4

          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,3,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,3,3,3,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,3,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4
          ,2,2,3,2,2,2,2,2,2,2,3,3,3,3,4,4,4,4,4,4,4
          ,2,2,3,2,2,2,2,2,2,2,3,4,4,4,4,4,4,4,4,4,4
          ,2,2,3,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4

          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4
          ,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4
          ,2,2,2,2,2,3,2,1,1,1,1,1,4,4,3,4,4,4,4,4,4
          ,2,2,2,2,2,3,2,1,2,2,4,1,4,4,3,4,4,4,4,4,4
          ,2,2,2,2,2,3,2,1,1,2,4,1,4,4,3,4,4,4,4,4,4
          ,2,2,2,2,2,3,2,2,2,2,4,4,4,4,3,4,4,4,4,4,4
          ,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4
          ,2,2,2,2,3,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,3,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,3,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,2,3,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4
          ,3,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4

          ,2,2,2,2,2,2,2,2,2,1,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,1,4,4,4,4,4,4,4,4,4,4,4
          ,2,1,1,1,1,1,1,1,1,1,4,4,4,4,4,4,4,4,4,4,4
          ,2,1,2,2,2,2,2,2,2,1,4,4,4,4,4,4,4,4,4,4,4
          ,2,1,2,1,1,1,1,2,2,1,4,4,4,4,4,4,4,4,4,4,4
          ,2,1,2,2,2,2,1,2,2,1,4,4,4,4,4,4,4,4,4,4,4
          ,2,1,2,1,1,1,1,2,2,1,4,4,4,4,4,4,4,4,4,4,4
          ,2,1,2,1,2,2,2,2,2,1,4,4,4,4,4,4,4,4,4,4,4
          ,2,1,1,1,2,1,1,1,2,1,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,1,2,1,2,1,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,1,2,2,2,1,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,1,1,1,1,1,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,1,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,1,4,4,4,4,4,4,4,4,4,4,4
          ,2,2,2,2,2,2,2,2,2,1,4,4,4,4,4,4,4,4,4,4,4

          ]
