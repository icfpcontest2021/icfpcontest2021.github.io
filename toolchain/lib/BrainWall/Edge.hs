{-# LANGUAGE DeriveFunctor #-}
module BrainWall.Edge
    ( Edge (..)
    , edgeSquaredLength
    , edgeConnected

    , EdgeIntersection (..)
    , edgeIntersection

    , factorAllEdges

    , edgePointSquaredDistance
    , pointOnEdge
    , rationalPointOnEdge
    ) where

import           BrainWall.V2
import Data.Foldable
import           Data.Ratio   (Ratio, (%))

data Edge a = Edge (V2 a) (V2 a) deriving (Eq, Functor)

instance Show a => Show (Edge a) where show (Edge p q) = show p ++ "-" ++ show q

edgeSquaredLength :: Num a => Edge a -> a
edgeSquaredLength (Edge p q) = squaredDistance p q

-- | Do the edges have a shared vertex?
edgeConnected :: Eq a => Edge a -> Edge a -> Bool
edgeConnected (Edge p0 q0) (Edge p1 q1) =
    p0 == p1 || p0 == q1 || q0 == p1 || q0 == q1

data EdgeIntersection
    = Touching         -- The edges touch.
    | Overlapping      -- The edges overlap.
    | Intersecting     -- The edges intersect at a point.
    | NotIntersecting  -- No intersection.
    deriving (Eq, Show)

-- | Taken from https://github.com/pgkelley4/line-segments-intersect/blob/master/js/line-segments-intersect.js
-- Updated to work with arbitary-precision rational numbers.
edgeIntersection :: (Real a) => Edge a -> Edge a -> EdgeIntersection
edgeIntersection edgeP@(Edge p p2) edgeQ@(Edge q q2)
    | uNumerator == 0 && denominator == 0 && overlapping = Overlapping
    | edgeConnected edgeP edgeQ = Touching
    | denominator == 0 = NotIntersecting
    | t >= 0 && t <= 1 && u >= 0 && u <= 1 =
        if t == 0 || t == 1 || u == 0 || u == 1 then Touching else Intersecting
    | otherwise = NotIntersecting
  where
    r = p2 .-. p
    s = q2 .-. q
    uNumerator = crossProduct (q .-. p) r
    denominator = crossProduct r s

    u = toRational uNumerator / toRational denominator
    t = toRational (crossProduct (q .-. p) s) / toRational denominator

    -- Do they overlap? (Are all the point differences in either direction
    -- the same sign)
    overlapping = not $
        equal
            [ v2X q - v2X p < 0, v2X q - v2X p2 < 0
            , v2X q2 - v2X p < 0, v2X q2 - v2X p2 < 0
            ] &&
        equal
            [ v2Y q - v2Y p < 0, v2Y q - v2Y p2 < 0
            , v2Y q2 - v2Y p < 0, v2Y q2 - v2Y p2 < 0
            ]

-- Factor edges so they don't intersect
factorEdges :: Integral a => Edge (Ratio a) -> Edge (Ratio a) -> Maybe [Edge (Ratio a)]
factorEdges edgeP' edgeQ'
  | uNumerator == 0 && denominator == 0 = colinear
  | denominator == 0 = Nothing -- parallel but not colinear
  | edgeConnected edgeP edgeQ = Nothing
  | t >= 0 && t <= 1 && u >= 0 && u <= 1 =
    Just $ filter goodEdge $ map (Edge intersection) [p, p2, q, q2]
  | otherwise = Nothing
  where
    edgeP@(Edge p p2) = case edgeP' of
      Edge p' p2'
        | p' <= p2' -> Edge p' p2'
        | otherwise -> Edge p2' p'
    edgeQ@(Edge q q2) = case edgeQ' of
      Edge q' q2'
        | q' <= q2' -> Edge q' q2'
        | otherwise -> Edge q2' q'
    r = p2 .-. p
    s = q2 .-. q
    uNumerator = crossProduct (q .-. p) r
    denominator = crossProduct r s

    u = uNumerator / denominator
    t = crossProduct (q .-. p) s / denominator
    intersection = q .+. (s .* u)

    minBy f x y = if f x <= f y then x else y
    maxBy f x y = if f x >= f y then x else y

    goodEdge (Edge x y) = x /= y
    colinear
      | v2X leftX > v2X rightX = Nothing
      | v2X leftX < v2X rightX = Just $ filter goodEdge [Edge farLeftX leftX, Edge leftX rightX, Edge rightX farRightX]
      | v2Y leftY > v2Y rightY = Nothing
      | v2Y leftY < v2Y rightY = Just $ filter goodEdge [Edge farLeftY leftY, Edge leftY rightY, Edge rightY farRightY]
      | otherwise = Nothing
      where
        farLeftX = minBy v2X p q
        leftX = maxBy v2X p q
        rightX = minBy v2X p2 q2
        farRightX = maxBy v2X p2 q2
        farLeftY = minBy v2Y p q
        leftY = maxBy v2Y p q
        rightY = minBy v2Y p2 q2
        farRightY = maxBy v2Y p2 q2

factorAllEdges :: Integral a => [Edge (Ratio a)] -> [Edge (Ratio a)]
factorAllEdges = go []
  where
    go xs [] = xs
    go xs (y : ys) = go (addEdge y xs) ys
    addEdge e [] = [e]
    addEdge e (x : xs) = case factorEdges e x of
      Nothing -> x : addEdge e xs
      Just es -> foldl' (flip addEdge) xs es

-- | Based on <http://paulbourke.net/geometry/pointlineplane/>.
edgePointSquaredDistance :: Integral a => V2 a -> Edge a -> Ratio a
edgePointSquaredDistance p3@(V2 x3 y3) (Edge p1@(V2 x1 y1) (V2 x2 y2))
    | denominator == 0 = fromIntegral $ edgeSquaredLength (Edge p1 p3)
    | otherwise        = edgeSquaredLength (Edge closest (fromIntegral <$> p3))
  where
    dx          = x2 - x1
    dy          = y2 - y1
    denominator = dx * dx + dy * dy
    uNumerator  = (x3 - x1) * dx + (y3 - y1) * dy
    u0          = uNumerator % denominator
    u1          = if u0 > 1 then 1 else if u0 < 0 then 0 else u0
    x           = fromIntegral x1 + u1 * fromIntegral dx
    y           = fromIntegral y1 + u1 * fromIntegral dy
    closest     = V2 x y

pointOnEdge :: Integral a => V2 a -> Edge a -> Bool
pointOnEdge q = rationalPointOnEdge (fmap fromIntegral q)

rationalPointOnEdge :: Integral a => V2 (Ratio a) -> Edge a -> Bool
rationalPointOnEdge q edge
    | crossProduct r s /= 0 = False
    | otherwise             =
        let dot = dotProduct r s in
        dot >= 0 && dot <= edgeSquaredLength (Edge p p2)
  where
    r = p2 .-. p
    s = q .-. p
    Edge p p2 = fmap fromIntegral edge

equal :: Eq a => [a] -> Bool
equal []       = True
equal (x : xs) = all (== x) xs
