{-# OPTIONS_GHC -fno-warn-orphans #-}
module BrainWall.Edge.Slope.Tests
    ( tests
    ) where

import           BrainWall.Edge.Slope
import           BrainWall.V2
import           Data.Ratio           (Ratio, (%))
import           Test.Tasty           (TestTree, testGroup)
import           Test.Tasty.HUnit     (testCase, (@?=))

fromV2' :: V2 Integer -> Slope (Ratio Integer)
fromV2' = fromV2

tests :: TestTree
tests = testGroup "BrainWall.Edge.Slope"
    [ testCase "fromV2 cases" $ do
        fromV2' (V2 3    0   ) @?= Q1 0
        fromV2' (V2 3    1   ) @?= Q1 (1 % 3)
        fromV2' (V2 0    3   ) @?= Q2 0
        fromV2' (V2 (-1) 3   ) @?= Q2 (1 % 3)
        fromV2' (V2 (-3) 0   ) @?= Q3 0
        fromV2' (V2 (-3) (-1)) @?= Q3 (1 % 3)
        fromV2' (V2 0    (-3)) @?= Q4 0
        fromV2' (V2 1    (-3)) @?= Q4 (1 % 3)
    ]
