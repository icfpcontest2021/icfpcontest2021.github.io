{-# LANGUAGE DeriveFunctor #-}
module BrainWall where

{-
hole01 :: Hole
hole01 = V2 0 0 :| [V2 3 0, V2 3 3]

figure01 :: Figure
figure01 = holeEdges hole01

problem01 :: Problem
problem01 = Problem hole01 figure01 0

figure02 :: Figure
figure02 = tail figure01 ++ [head figure01]

figure03 :: Figure
figure03 =
    [ Edge (V2 0 0) (V2 3 0)
    , Edge (V2 3 0) (V2 6 0)
    , Edge (V2 6 0) (V2 9 3)
    ]

figure04 :: Figure
figure04 = holeEdges $ V2 3 3 :| [V2 3 0, V2 0 0]

moveFigure :: V2 Integer -> Figure -> Figure
moveFigure o figure = [Edge (p .+. o) (q .+. o) | Edge p q <- figure]

figure05 :: Figure
figure05 = moveFigure (V2 1 0) figure04
-}
