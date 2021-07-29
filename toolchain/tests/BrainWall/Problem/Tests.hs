{-# LANGUAGE OverloadedLists #-}
module BrainWall.Problem.Tests
    ( tests
    ) where

import           BrainWall.Edge
import           BrainWall.Polygon
import           BrainWall.Problem
import           BrainWall.V2
import           Data.Ratio        ((%))
import           Test.Tasty        (TestTree, testGroup)
import           Test.Tasty.HUnit  (testCase, (@=?))

tests :: TestTree
tests = testGroup "BrainWall.Problem"
    [ testCase "bonus cheater" $ do
        [TooManyBonuses 1] @=? validateSolution triangleProblem Solution
            { solutionBonuses  =
                [ ClaimedBonus 0 Globalist Nothing
                , ClaimedBonus 0 SuperFlex Nothing
                ]
            , solutionVertices = [V2 0 0, V2 11 0, V2 10 10]
            }

    , testCase "global epsilon" $ do
        [] @=? validateSolution triangleProblem Solution
            { solutionBonuses  = []
            , solutionVertices = [V2 0 0, V2 11 0, V2 10 10]
            }

        [EdgeLengthMismatch (0, 1) (100, 144) (1000000 `div` 4)] @=?
            validateSolution triangleProblem Solution
                { solutionBonuses  = []
                , solutionVertices = [V2 0 0, V2 12 0, V2 10 10]
                }

        [] @=? validateSolution triangleProblem Solution
            { solutionBonuses  = [ClaimedBonus 0 Globalist Nothing]
            , solutionVertices = [V2 0 0, V2 12 0, V2 10 10]
            }

        [GlobalistEdgeLengthMismatch (3 % 4) (39 % 50)] @=?
            validateSolution triangleProblem Solution
                { solutionBonuses  = [ClaimedBonus 0 Globalist Nothing]
                , solutionVertices = [V2 0 0, V2 13 0, V2 10 10]
                }

    , testCase "superflex" $ do
        [EdgeLengthMismatch (2, 0) (200, 400) (1000000 `div` 4)] @=?
            validateSolution triangleProblem Solution
                { solutionBonuses  = []
                , solutionVertices = [V2 0 0, V2 10 0, V2 20 0]
                }

        [] @=? validateSolution triangleProblem Solution
            { solutionBonuses  = [ClaimedBonus 0 SuperFlex Nothing]
            , solutionVertices = [V2 0 0, V2 10 0, V2 20 0]
            }

    , testCase "wallhack" $ do
        [] @=? validateSolution triangleProblem Solution
            { solutionBonuses  = []
            , solutionVertices = [V2 0 85, V2 10 85, V2 10 95]
            }

        [ PointNotInHole 2 (V2 10 105)
            , EdgeNotInHole (1, 2) (Edge (V2 10 95) (V2 10 105))
            , EdgeNotInHole (2, 0) (Edge (V2 10 105) (V2 0 95))
            ] @=? validateSolution triangleProblem Solution
            { solutionBonuses  = []
            , solutionVertices = [V2 0 95, V2 10 95, V2 10 105]
            }

        [] @=? validateSolution triangleProblem Solution
            { solutionBonuses  = [ClaimedBonus 0 WallHack Nothing]
            , solutionVertices = [V2 0 95, V2 10 95, V2 10 105]
            }

        -- Three edges and two points outside the hole.
        (5 @=?) . length $ validateSolution triangleProblem Solution
            { solutionBonuses  = [ClaimedBonus 0 WallHack Nothing]
            , solutionVertices = [V2 95 95, V2 105 95, V2 105 105]
            }

    , testCase "break_a_leg" $ do
        [VerticesLengthMismatch 4 3] @=?
            validateSolution triangleProblem Solution
                { solutionBonuses  = [ClaimedBonus 0 BreakALeg (Just (2, 0))]
                , solutionVertices = [V2 0 0, V2 10 0, V2 10 10]
                }

        [BrokenLegError "Length error for broken legs"] @=? validateSolution triangleProblem Solution
            { solutionBonuses  = [ClaimedBonus 0 BreakALeg (Just (2, 0))]
            , solutionVertices = [V2 0 0, V2 10 0, V2 10 10, V2 0 10]
            }

        [BrokenLegError "Length error for broken legs"] @=? validateSolution triangleProblem Solution
            { solutionBonuses  = [ClaimedBonus 0 BreakALeg (Just (2, 0))]
            , solutionVertices = [V2 0 0, V2 10 0, V2 10 10, V2 10 0]
            }

        [BrokenLegError "Length error for broken legs"] @=?
            validateSolution triangleProblem Solution
                { solutionBonuses  = [ClaimedBonus 0 BreakALeg (Just (2, 0))]
                , solutionVertices = [V2 0 0, V2 10 0, V2 10 10, V2 0 20]
                }

        [ BrokenLegError "Length error for broken legs"
          , PointNotInHole 3 (V2 (-1) 10)
          , EdgeNotInHole (2, 3) (Edge (V2 10 10) (V2 (-1) 10))
          , EdgeNotInHole (0, 3) (Edge (V2 0 0) (V2 (-1) 10))] @=?
            validateSolution triangleProblem Solution
                { solutionBonuses  = [ClaimedBonus 0 BreakALeg (Just (2, 0))]
                , solutionVertices = [V2 0 0, V2 10 0, V2 10 10, V2 (-1) 10]
                }

        [ PointNotInHole 3 (V2 (-3) 6)
          , EdgeNotInHole (2, 3) (Edge (V2 4 8) (V2 (-3) 6))
          , EdgeNotInHole (0, 3) (Edge (V2 0 0) (V2 (-3) 6))] @=?
            validateSolution triangleProblem Solution
                { solutionBonuses  = [ClaimedBonus 0 BreakALeg (Just (2, 0))]
                , solutionVertices = [V2 0 0, V2 10 0, V2 4 8, V2 (-3) 6]
                }

        [] @=?
            validateSolution triangleProblem Solution
                { solutionBonuses  = [ClaimedBonus 0 BreakALeg (Just (2, 0))]
                , solutionVertices = [V2 3 0, V2 13 0, V2 7 8, V2 0 6]
                }

        [] @=?
            validateSolution triangleProblem Solution
                { solutionBonuses  = [ClaimedBonus 0 BreakALeg (Just (0, 2))]
                , solutionVertices = [V2 3 0, V2 13 0, V2 7 8, V2 0 6]
                }

        [BrokenLegError "Leg to break must be specified"] @=?
            validateSolution triangleProblem Solution
                { solutionBonuses  = [ClaimedBonus 0 BreakALeg Nothing]
                , solutionVertices = [V2 0 0, V2 10 0, V2 10 10, V2 0 10]
                }
    ]
  where
    width           = 100
    height          = 100
    Just squareHole = fmap Hole $ mkPolygon
        [V2 0 0, V2 width 0, V2 width height, V2 0 height]

    triangleProblem = Problem
        { problemHole    = squareHole
        , problemFigure  = triangleFigure
        , problemEpsilon = 1000000 `div` 4
        , problemBonuses = []
        }

    triangleFigure = Figure
        { figureVertices = [V2 0 0, V2 10 0, V2 10 10]
        , figureEdges    = [(0, 1), (1, 2), (2, 0)]
        }
