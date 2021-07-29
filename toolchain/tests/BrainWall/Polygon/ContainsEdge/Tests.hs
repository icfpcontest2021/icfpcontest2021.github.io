{-# LANGUAGE OverloadedLists #-}
module BrainWall.Polygon.ContainsEdge.Tests
    ( tests
    ) where

import           BrainWall.Edge
import           BrainWall.Polygon
import           BrainWall.Polygon.ContainsEdge
import           BrainWall.Polygon.ContainsPoint
import           BrainWall.V2
import           Control.Monad                   (guard)
import           Data.Maybe                      (isJust, isNothing,
                                                  listToMaybe)
import           Data.Ratio                      (Ratio, (%))
import           Test.Tasty                      (TestTree, testGroup)
import           Test.Tasty.HUnit                (testCase, (@?=))
import           Test.Tasty.QuickCheck           (testProperty)
import qualified Test.Tasty.QuickCheck           as QC

tests :: TestTree
tests = testGroup "BrainWall.Polygon.ContainsEdge"
    [ testCase "containsEdge cases 1" $ do
        --  o         o
        --  |\       /|
        --  | o-----o |
        --  |         |
        --  o---------o
        let (Just polygon) = mkPolygon
                [V2 0 0, V2 2 2, V2 4 2, V2 6 0, V2 6 4, V2 0 4]
                :: Maybe (Polygon Integer)
        containsEdge (Edge (V2 0 0) (V2 6 0)) polygon @?= False
        containsEdge (Edge (V2 1 1) (V2 5 1)) polygon @?= False
        containsEdge (Edge (V2 2 2) (V2 4 2)) polygon @?= True
        containsEdge (Edge (V2 0 3) (V2 6 3)) polygon @?= True
        containsEdge (Edge (V2 0 4) (V2 6 4)) polygon @?= True

    , testCase "containsEdge cases 2" $ do
        --  o---------o
        --  |         |
        --  |   o-o   |
        --  |  /   \  |
        --  o-o     o-o
        let (Just polygon) = mkPolygon
                [ V2 0 0, V2 12 0, V2 12 6, V2 10 6
                , V2 7 4, V2 5 4, V2 2 6, V2 0 6
                ] :: Maybe (Polygon Integer)
        containsEdge (Edge (V2 0 0) (V2 6 0)) polygon @?= True
        containsEdge (Edge (V2 4 0) (V2 14 0)) polygon @?= False
        containsEdge (Edge (V2 0 2) (V2 0 14)) polygon @?= False

        containsEdge (Edge (V2 0 6) (V2 12 6)) polygon @?= False
        containsEdge (Edge (V2 1 6) (V2 11 6)) polygon @?= False
        containsEdge (Edge (V2 2 6) (V2 10 6)) polygon @?= False

        containsEdge (Edge (V2 5 4) (V2 7 4)) polygon @?= True
        containsEdge (Edge (V2 4 4) (V2 7 4)) polygon @?= True
        containsEdge (Edge (V2 4 4) (V2 8 4)) polygon @?= True

        containsEdge (Edge (V2 7 4) (V2 10 6)) polygon @?= True
        containsEdge (Edge (V2 4 2) (V2 10 6)) polygon @?= True
        containsEdge (Edge (V2 7 4) (V2 13 8)) polygon @?= False

    , testProperty "containsEdge property" $ do
        -- TODO: generate arbitrary polygons
        --
        --  o---------o
        --  |         |
        --  |   o-o   |
        --  |  /   \  |
        --  o-o     o-o
        let (Just polygon) = mkPolygon
                [ V2 0 0, V2 12 0, V2 12 6, V2 10 6
                , V2 7 4, V2 5 4, V2 2 6, V2 0 6
                ] :: Maybe (Polygon Integer)

        edge <- arbitraryEdge (20, 20)
        pure $ case containsEdge edge polygon of
            True  -> isNothing $ findCounterExample edge polygon
            False -> isJust $ findCounterExample edge polygon
    ]

arbitraryV2 :: (Integer, Integer) -> QC.Gen (V2 Integer)
arbitraryV2 (w, h) = V2 <$> QC.choose (0, w) <*> QC.choose (0, h)

arbitraryEdge :: (Integer, Integer) -> QC.Gen (Edge Integer)
arbitraryEdge dim = Edge <$> arbitraryV2 dim <*> arbitraryV2 dim

-- | Assuming that an edge is not contained, try to find a point that is
-- outside.
findCounterExample
    :: Edge Integer -> Polygon Integer -> Maybe (V2 (Ratio Integer))
findCounterExample edge polygon = listToMaybe $ do
    i <- [0 .. steps]
    let Edge p q = fromIntegral <$> edge
        d = q .-. p
        t = i % steps
        point = p .+. d .* t
    guard . not $ rationalPointInPolygon point polygon
    pure point
  where
    steps = 100 :: Integer
