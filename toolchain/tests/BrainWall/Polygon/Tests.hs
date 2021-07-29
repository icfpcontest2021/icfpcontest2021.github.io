{-# LANGUAGE OverloadedLists #-}
module BrainWall.Polygon.Tests
    ( tests
    ) where

import           BrainWall.Polygon
import           BrainWall.V2
import qualified Data.Vector       as V
import           Test.Tasty        (TestTree, testGroup)
import           Test.Tasty.HUnit  (testCase, (@?=))

assertClockwise :: V.Vector (V2 Integer) -> IO ()
assertClockwise v = do
    clockwise v @?= True
    clockwise (V.reverse v) @?= False

tests :: TestTree
tests = testGroup "BrainWall.Polygon"
    [ testCase "clockwise cases" $ do
        assertClockwise [V2 0 0, V2 1 1, V2 2 1, V2 3 0, V2 3 2, V2 0 2]
        assertClockwise [V2 5 0, V2 6 4, V2 4 5, V2 1 5, V2 1 0]

    , testCase "polygonSplitEdge cases" $ do
        polygonSplitEdge (V2 0 0) polygon @?= Just (V2 0 4, V2 2 2)
        polygonSplitEdge (V2 1 1) polygon @?= Just (V2 0 0, V2 2 2)
        polygonSplitEdge (V2 0 2) polygon @?= Just (V2 0 4, V2 0 0)
    ]
  where
    --  o         o
    --  |\       /|
    --  | o-----o |
    --  |         |
    --  o---------o
    Just polygon = mkPolygon [V2 0 0, V2 2 2, V2 4 2, V2 6 0, V2 6 4, V2 0 4]
        :: Maybe (Polygon Integer)
