{-# OPTIONS_GHC -fno-warn-orphans #-}
module BrainWall.Edge.Tests
    ( tests
    ) where

import           BrainWall.Edge
import           BrainWall.V2
import           Data.Ratio       (Ratio, (%))
import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase, (@?=))

edgePointSquaredDistance' :: V2 Integer -> Edge Integer -> Ratio Integer
edgePointSquaredDistance' = edgePointSquaredDistance

tests :: TestTree
tests = testGroup "BrainWall.Edge"
    [ testCase "edgePointSquaredDistance cases" $ do
        edgePointSquaredDistance' (V2 1 1) (Edge (V2 1 1) (V2 2 2)) @?= 0
        edgePointSquaredDistance' (V2 1 1) (Edge (V2 0 1) (V2 2 1)) @?= 0
        edgePointSquaredDistance' (V2 1 1) (Edge (V2 0 0) (V2 2 0)) @?= 1
        edgePointSquaredDistance' (V2 0 1) (Edge (V2 0 0) (V2 3 2)) @?= 9 % 13
    ]
