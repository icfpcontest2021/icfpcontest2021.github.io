{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module BrainWall.Circle
    ( Circle (..)
    , CircleCircleIntersection (..)
    , circleCircleIntersection
    ) where

import           BrainWall.V2
import           Data.Hashable (Hashable)
import           GHC.Generics  (Generic)

data Circle a = Circle !(V2 a) !a
    deriving (Eq, Functor, Generic, Show)

instance Hashable a => Hashable (Circle a)

data CircleCircleIntersection a
    = Separate
    | Contained
    | Coincident
    | Intersecting (V2 a) (V2 a)

circleCircleIntersection
    :: (Floating a, Ord a) => Circle a -> Circle a -> CircleCircleIntersection a
circleCircleIntersection (Circle p0@(V2 x0 y0) r0) (Circle p1@(V2 x1 y1) r1)
    | d > r0 + r1 = Separate
    | d < abs (r0 - r1) = Contained
    | d == 0 && r0 == r1 = Coincident
    | otherwise = Intersecting
        (V2 (x2 + h * (y1 - y0) / d) (y2 - h * (x1 - x0) / d))
        (V2 (x2 - h * (y1 - y0) / d) (y2 + h * (x1 - x0) / d))
  where
    d = distance p0 p1
    a = (r0 * r0 - r1 * r1 + d * d) / (2 * d)
    h = sqrt $ r0 * r0 - a * a
    _p2@(V2 x2 y2) = p0 .+. (p1 .-. p0) .* (a / d)
