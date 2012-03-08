-- | Read and load *.ang files from EBSD measure systems.
module Hammer.IO.ANGReader
( parseANG
, EBSDpoint (..)
, EBSDinfo  (..)
, Gridinfo  (..)
, EBSDdata  (..)
) where

-- Imports
import Data.List
import Data.Char
import Text.ParserCombinators.Parsec
import Hammer.Texture.Orientation

-- Data definition
-- | Information associated to each point. For futher reference consult OIM manual.
data EBSDpoint = EBSDpoint
               { rotation :: Rodrigues
               , colpos   :: (Int, Int)
               , qi       :: Double
               , ci       :: Double
               , phase    :: Int
               , detecInt :: Int
               } deriving Show
                          
-- | Information describing the measuriment.
data EBSDinfo = EBSDinfo
              { xstart       :: Double
              , ystart       :: Double
              , zstart       :: Double
              , workDist     :: Double
              , nphase       :: Int
              , materialName :: String
              , formula      :: String
              , info         :: String
              , symmetry     :: String
              , latticeCons  :: (Double, Double, Double, Double, Double, Double)
              , numFamilies  :: Int
              , hklFamilies  :: [(Int, Int, Int, Int, Double, Int)]
              , categories   :: (Int, Int, Int, Int, Int)
              } deriving Show

-- | Information about the grid of point. Hexagonal or Square
data Gridinfo = Gridinfo
              { colRow  :: (Int, Int, Int)
              , xystep  :: (Double, Double)
              , hexGrid :: Bool
              } deriving Show
-- | Hold the whole ANG data strcuture
data EBSDdata = EBSDdata
              { node     :: [EBSDpoint]
              , grid     :: Gridinfo
              , headInfo :: EBSDinfo
              } deriving Show



-- Parse functions
-- ---------------

-- Read the input ANG file. Rise an error mesage in case of bad format or acess.
parseANG::String -> IO EBSDdata
parseANG fileName = do
  parsed <- parseFromFile parseFilePattern fileName
  case (parsed) of
    Left err  -> error (">> Error reading file " ++ fileName ++ "\n" ++ show err)
    Right xs  -> return xs


parseFilePattern::Parser EBSDdata
parseFilePattern = do
  headInfo <- headParse
  skipComment
  gridInfo <- gridParse
  many (skipComment)
  points   <- many (pointParse gridInfo)
  eof
  return $ EBSDdata points gridInfo headInfo


-- SubParsers
-- ----------
  
headParse::Parser EBSDinfo
headParse = do
  dumb      <- getInfo "# TEM_PIXperUM" nFloat
  xstart    <- getInfo "# x-star" nFloat
  ystart    <- getInfo "# y-star" nFloat
  zstart    <- getInfo "# z-star" nFloat
  wkdis     <- getInfo "# WorkingDistance" nFloat
  skipComment
  phase     <- getInfo "# Phase" nInt
  mat       <- getInfo "# MaterialName" sText
  form      <- getInfo "# Formula"  sText
  info      <- getInfo "# Info" sText
  sym       <- getInfo "# Symmetry" sText
  latt      <- latticeParse
  nFamily   <- getInfo "# NumberFamilies" nInt
  hklFamily <- many hklParse
  cat       <- categoryParse
  return $ EBSDinfo xstart ystart zstart wkdis phase mat form info sym latt nFamily hklFamily cat


latticeParse::Parser (Double, Double, Double, Double, Double, Double)
latticeParse = do
  string "# LatticeConstants"
  a      <- nFloat
  b      <- nFloat
  c      <- nFloat
  alpha1 <- nFloat
  alpha2 <- nFloat
  alpha3 <- nFloat
  eol
  return (a, b, c, alpha1, alpha2, alpha3)


hklParse::Parser (Int, Int, Int, Int, Double, Int)
hklParse = do
  try $ string "# hklFamilies"
  h <- nInt
  k <- nInt
  l <- nInt
  a <- nInt
  b <- nFloat
  c <- nInt <|> return 0
  eol
  return (h, k, l, a, b, c)

categoryParse::Parser (Int,Int,Int,Int,Int)
categoryParse = do
  string "# Categories"
  a      <- nInt
  b      <- nInt
  c      <- nInt
  d      <- nInt
  f      <- nInt
  eol
  return (a,b,c,d,f)

gridParse::Parser Gridinfo
gridParse = do
  isHex <- getInfo "# GRID:" gridType
  xstep <- getInfo "# XSTEP:" nFloat
  ystep <- getInfo "# YSTEP:" nFloat
  codd  <- getInfo "# NCOLS_ODD:" nInt
  ceven <- getInfo "# NCOLS_EVEN:" nInt
  row   <- getInfo "# NROWS:" nInt
  return $ Gridinfo (row, ceven, codd) (xstep, ystep) isHex

pointParse::Gridinfo -> Parser EBSDpoint
pointParse g = do
  phi1    <- nFloat
  phi     <- nFloat
  phi2    <- nFloat
  xpos    <- nFloat
  ypos    <- nFloat
  qi      <- nFloat
  ci      <- nFloat
  phase   <- nInt
  dectInt <- nInt
  xx      <- nFloat
  eol
  let rotation = toRodrigues (Euler (Rad phi1, Rad phi, Rad phi2))
      (row, col) = getGridPoint g (xpos, ypos)
  return $ EBSDpoint rotation (row, col) qi ci phase dectInt



-- Calculate colunm position (row,col) from ID sequence
-- Origin at (1,1)
getGridPoint::Gridinfo -> (Double, Double) -> (Int, Int)
getGridPoint g (xpos, ypos) =
  let
    (xstep, ystep) = xystep g
    row            = 1 + round (ypos/ystep)
    -- divide for 0.5*xstep to avoid error when round value
    -- ex. round 7.4/5.0 \= round 7.6/5.0;
    col            = 1 + div (round ((2*xpos)/xstep)) 2
  in (row, col)


getInfo::String -> Parser a -> Parser a
getInfo ident func = do
  string ident
  x <- func
  eol
  return x


-- Basic parsers
-- -------------

skipComment::Parser ()  
skipComment = string "#" >> skipMany (noneOf "\r\n") >> eol

gridType::Parser Bool
gridType = do
  blanks
  x <-  (string "HexGrid" >> return True)
    <|> (string "SqrGrid" >> return False)
  return x        
  
nFloat::Parser Double
nFloat = do
  blanks
  ds <- many (digit <|> oneOf ".-+")
  return $ read ds

sText::Parser String
sText = do
  blanks
  ds <- many (noneOf "\r\n")
  return $ (unwords.words) ds

nInt::Parser Int
nInt =  do
  blanks
  ds <- many (digit <|> oneOf ".-+")
  return $ read ds

-- Skip blank chars (space and tab)
blanks::Parser ()
blanks = skipMany (oneOf " \t" )

-- Skip blanks chars till and including the eol (End Of Line - CR-LF or LF)
eol::Parser ()
eol = blanks >> skipMany1 (oneOf "\r\n")
