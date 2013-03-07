{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Data.Vector                            as V

import           Data.List
import           Hammer.MicroGraph
import           Hammer.Render.VTK.VTKRender
import           Hammer.Render.VoxBoxVTK
import           Hammer.Texture.Harmonics.BaseFunctions
import           Hammer.VoxBox.Base
import           Hammer.VoxBox.GrainFinder
import           Hammer.VoxBox.MicroVoxel
import           Numeric.Container                       (add, scale, buildMatrix)
import           Options.Applicative
import           Prelude
import           System.Random
import           TestGrainFinder

data Tester =
  Tester
  { run_proflie_harmonics   :: Maybe String
  , run_test_GrainFinder    :: Maybe String
  , run_profile_GrainFinder :: Maybe String
  , run_profile_VTKRender   :: Maybe String
  , run_test_suit           :: Bool
  } deriving (Show)

tester :: Parser Tester
tester = Tester
  <$> parseMaybeOpt
      (  long "harmonics-test"
      <> short 'a'
      <> metavar "VTK_OUT"
      <> help "Test spherical harmonics module" )
  <*> parseMaybeOpt
      (  long "grainfinder-test"
      <> short 'g'
      <> metavar "VTK_OUT"
      <> help "Test GrainFinder in a standard voxelized microstructure.")
  <*> parseMaybeOpt
      (  long "grainfinder-profile"
      <> short 'G'
      <> metavar "VTK_OUT"
      <> help "Profile GrainFinder in a standard voxelized microstructure.")
  <*> parseMaybeOpt
      (  long "vtkRender-profile"
      <> short 'r'
      <> metavar "VTK_OUT"
      <> help "Profile VTKRender module" )
  <*> switch
      (  long "test-suit"
      <> short 't'
      <> help "Run test suit." )

parseMaybeOpt cfg = (Just <$> strOption cfg) <|> pure Nothing


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
    else print "Skiping test suit."
  
  case run_proflie_harmonics of
    Just x -> profile_harmonics x
    _      -> print "Skiping harmonics profile." 

  case run_profile_GrainFinder of
    Just x -> profile_GrainFinder x
    _      -> print "Skiping GrainFinder profile." 

  case run_test_GrainFinder of
    Just x -> test_GrainFinder x
    _      -> print "Skiping GrainFinder test." 

  case run_profile_VTKRender of
    Just x -> profile_VTKRender x
    _      -> print "Skiping VTKRender profile."
   
profile_harmonics fout = do 
  rnd <- newStdGen
  let
    mzero i = buildMatrix (2*i+1) (2*i+1) (\_->0)
    rs   = take 100 $ randoms rnd
    cs i = scale (1/100) $ foldl' add (mzero i) $
           map ( \x -> let
                    k= pi/10 * (1+(x-0.5)/100)
                    in getRealCoef (L i) k k k
               ) rs
    c    = map cs [0 .. 15]
    k    = (pi/2)/18
    p    = map (plotPhi2SectionReal c) [k*i | i<-[0..17]]
  --saveToGNUPlot "/home/edgar/Desktop/odf.data" p
  saveToVTKPlot (fout ++ ".vti") p

profile_VTKRender fout = let
  vbox = VoxBox { dimension = mkStdVoxBoxRange (VoxBoxDim 100  100  100)
                , origin    = VoxBoxOrigin  0    0    0
                , spacing   = VoxelDim      0.25 0.25 0.25
                , grainID   = vec }
  vec   = V.replicate (100*100*100) (2::Int)
  vtk   = renderVoxBoxVTK vbox attrs
  attrs = [mkCellAttr "GrainID" (\a _ _ -> 1 :: Int)]
  in writeUniVTKfile (fout ++ ".vtr") vtk

profile_GrainFinder fout = let
  dx = 100
  dy = 100
  dz = 1000
  rawVBox = VoxBox { dimension = mkStdVoxBoxRange (VoxBoxDim dx   dy   dz)
                   , origin    = VoxBoxOrigin  0    0    0
                   , spacing   = VoxelDim      0.25 0.25 0.25
                   , grainID   = vec }
  vec   = V.replicate (dx*dy*dz) (2::Int)
  in case grainFinder rawVBox of  
    Just (vbox, _) -> do
      let
        vec   = V.map unGrainID $ grainID vbox
        vtk   = renderVoxBoxVTK vbox attrs
        attrs = [mkCellAttr "GrainID" (\a _ _ -> vec V.! a)]
      writeUniVTKfile (fout ++ ".vtr") vtk
    _ -> print "Unable to find grains."
 
test_GrainFinder fout = case grainFinder grainTest of 
  Just (vbox, _) -> do
    let
      vec   = V.map unGrainID $ grainID vbox
      vtk   = renderVoxBoxVTK vbox attrs
      attrs = [mkCellAttr "GrainID" (\a _ _ -> vec V.! a)]
    writeUniVTKfile (fout ++ ".vtr") vtk
  _ -> print "Unable to find grains." 

grainTest = VoxBox { dimension = mkStdVoxBoxRange (VoxBoxDim 21   15   5)
                   , origin    = VoxBoxOrigin  0    0    0
                   , spacing   = VoxelDim      0.25 0.25 0.25
                   , grainID   = g }
  where g = V.fromList
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
