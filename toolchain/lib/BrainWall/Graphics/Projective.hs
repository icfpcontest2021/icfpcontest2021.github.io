-- Based on <https://math.stackexchange.com/a/339033>
module BrainWall.Graphics.Projective
    ( FourPoints
    , projectiveTransform
    ) where

import BrainWall.V2
import qualified Linear.Matrix as L
import qualified Linear.V3 as L

-- Four points.  Should not have three colinear points.
type FourPoints a = (V2 a, V2 a, V2 a, V2 a)

solveAndScale :: Fractional a => FourPoints a -> L.M33 a
solveAndScale (V2 x1 y1, V2 x2 y2, V2 x3 y3, V2 x4 y4) = L.V3
    (L.V3 (lambda * x1) (mu * x2) (tau * x3))
    (L.V3 (lambda * y1) (mu * y2) (tau * y3))
    (L.V3  lambda        mu        tau      )
  where
    lhs = L.V3 (L.V3 x1 x2 x3) (L.V3 y1 y2 y3) (L.V3 1  1  1)
    rhs = L.V3 x4 y4 1

    (L.V3 lambda mu tau) = L.inv33 lhs L.!* rhs

projectiveTransform
    :: Fractional a => FourPoints a -> FourPoints a -> (V2 a -> V2 a)
projectiveTransform source destination = transform
  where
    a = solveAndScale source
    b = solveAndScale destination
    c = b L.!*! L.inv33 a

    transform (V2 x y) =
        let (L.V3 x' y' z') = c L.!* (L.V3 x y 1) in
        V2 (x' / z') (y' / z')
