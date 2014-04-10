{-# LANGUAGE RecordWildCards #-}
module TestGrainFinder where

import Control.Applicative
import Data.Maybe
import Hammer.VoxBox
import Hammer.VoxConn
import Test.QuickCheck

import Debug.Trace

-- ================================================================================

dbg x a = trace (show x ++ " " ++ show a) a

runChecker =  do
  let myArgs = Args {replay = Nothing, maxSuccess = 1000, maxDiscardRatio = 50, maxSize = 1000, chatty = True}
  print "Testing GrainFinder.."
  quickCheckWith myArgs test_voxelPosConv
  quickCheckWith myArgs test_checkPosBound
  --quickCheckWith myArgs test_split_merge

instance Arbitrary VoxelPos where
  arbitrary = liftA3 VoxelPos x x x
    where x = choose (-50,100)

instance Arbitrary VoxBoxDim where
  arbitrary = liftA3 VoxBoxDim p p p
    where p = choose (1,100)

instance Arbitrary VoxBoxRange where
  arbitrary = VoxBoxRange <$> arbitrary <*> arbitrary

msgFail text = printTestCase ("\x1b[7m Fail: " ++ show text ++ "! \x1b[0m")

test_checkPosBound :: VoxBoxRange -> VoxelPos -> Property
test_checkPosBound vbr vp@(VoxelPos x y z) =
  checkPosBound vbr vp ==>
  let (VoxBoxDim dx dy dz) = vbrDim vbr
  in abs (dx*dy*dz) >= vbr %@ vp

test_voxelPosConv :: VoxBoxRange -> VoxelPos -> Property
test_voxelPosConv vbr vpos = checkPosBound vbr vpos ==>
  case getVoxelID vbr vpos of
    Just i -> case vbr %#? i of
      Just p -> test (vpos == p)
      _      -> test False
    _ -> test False
  where
    test x = msgFail "Can't convert VID to VoxelPos." x

{--
test_split_merge :: VoxBoxRange -> Property
test_split_merge vbr = let
  (b1, b2, b3, b4, b5, b6, b7, b8) = splitBox vbr
  b12 = mergeMaybe b1 b2
  b34 = mergeMaybe b3 b4
  b56 = mergeMaybe b5 b6
  b78 = mergeMaybe b7 b8
  b1234 = mergeMaybe b12 b34
  b5678 = mergeMaybe b56 b78

  mergeMaybe a b = let
    foo = do
      a' <- a
      b' <- b
      mergeVoxBoxRange a' b'
    in case foo of
      Just (x,_,_) -> Just x
      _            -> a <|> b

  in case mergeMaybe b1234 b5678 of
    Just b
      | (b == vbr) -> property True
      | otherwise  -> property False
    _          -> property False
--}
