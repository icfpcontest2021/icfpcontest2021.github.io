{-# OPTIONS_GHC -fno-warn-orphans #-}
module BrainWall.Polygon.ContainsPoint.Tests
    ( tests
    ) where

import           BrainWall.Edge
import           BrainWall.Polygon.ContainsPoint
import           BrainWall.Svg
import           BrainWall.V2
import           Control.Monad                   (forM_)
import           Data.Ratio                      (Ratio, (%))
import           Test.Tasty                      (TestTree, testGroup)
import           Test.Tasty.HUnit                (testCase, (@?=))
import           Test.Tasty.QuickCheck           (testProperty)
import qualified Test.Tasty.QuickCheck           as QC

-- We probably want to move this instance
instance QC.Arbitrary a => QC.Arbitrary (V2 a) where
    arbitrary = V2 <$> QC.arbitrary <*> QC.arbitrary

rayEdgeIntersection'
    :: Ray (Ratio Integer) -> Edge (Ratio Integer)
    -> Maybe (RayEdgeIntersection (Ratio Integer))
rayEdgeIntersection' = rayEdgeIntersection

tests :: TestTree
tests = testGroup "BrainWall.Polygon.ContainsPoint"
    [ testCase "rayEdgeIntersection cases" $ do
        rayEdgeIntersection' (Ray (V2 1 1)) (Edge (V2 2 1) (V2 5 1)) @?=
            Just (HorizontalOverlapping 2 5)

        rayEdgeIntersection' (Ray (V2 1 1)) (Edge (V2 5 1) (V2 2 1)) @?=
            Just (HorizontalOverlapping 2 5)

        rayEdgeIntersection' (Ray (V2 1 0)) (Edge (V2 0 0) (V2 2 0)) @?=
            Just (HorizontalOverlapping 1 2)

        rayEdgeIntersection' (Ray (V2 1 0)) (Edge (V2 0 0) (V2 1 0)) @?=
            Just (HorizontalOverlapping 1 1)

        rayEdgeIntersection' (Ray (V2 2 0)) (Edge (V2 0 0) (V2 1 0)) @?=
            Nothing

        rayEdgeIntersection' (Ray (V2 0 1)) (Edge (V2 2 (-1)) (V2 1 2)) @?=
            Just (Intersection (4 % 3))

        rayEdgeIntersection' (Ray (V2 2 1)) (Edge (V2 2 (-1)) (V2 1 2)) @?=
            Nothing

        rayEdgeIntersection' (Ray (V2 0 2)) (Edge (V2 2 (-1)) (V2 1 2)) @?=
            Just (VertexIntersection 1)

        rayEdgeIntersection' (Ray (V2 0 3)) (Edge (V2 2 (-1)) (V2 1 2)) @?=
            Nothing

    , testProperty "rayEdgeIntersection direction is irrelevant" $ \ray p q ->
        rayEdgeIntersection' (Ray ray) (Edge p q) ==
        rayEdgeIntersection' (Ray ray) (Edge q p)

    , testCase "pointInPolygon complicated" $ do
        svg <- readSvg "tests/data/complicated-hole.svg"
        [polygon] <- either fail
            (pure . fmap (fmap (round :: Double -> Integer))) $
            traverse polygonFromSvg $ svgElements svg

        forM_ [1 .. 9] $ \y ->
            pointInPolygon (V2 1 y) polygon @?= True
        forM_ [4 .. 8] $ \y ->
            pointInPolygon (V2 3 y) polygon @?= False
        forM_ [1 .. 9] $ \y ->
            pointInPolygon (V2 (-1) y) polygon @?= False
    ]
