module Main where

import qualified BrainWall.Database.Tests
import qualified BrainWall.Edge.Slope.Tests
import qualified BrainWall.Edge.Tests
import qualified BrainWall.Main.Prosecutor.Tests
import qualified BrainWall.Polygon.ContainsEdge.Tests
import qualified BrainWall.Polygon.ContainsPoint.Tests
import qualified BrainWall.Polygon.Tests
import qualified BrainWall.Problem.Tests
import qualified Test.Tasty                            as Tasty

main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "brain-wall"
    [ BrainWall.Database.Tests.tests
    , BrainWall.Edge.Tests.tests
    , BrainWall.Edge.Slope.Tests.tests
    , BrainWall.Main.Prosecutor.Tests.tests
    , BrainWall.Polygon.ContainsEdge.Tests.tests
    , BrainWall.Polygon.ContainsPoint.Tests.tests
    , BrainWall.Polygon.Tests.tests
    , BrainWall.Problem.Tests.tests
    ]
