module BrainWall.Polygon.ContainsEdge
    ( containsEdge
    , crossesEdge
    ) where

import           BrainWall.Edge
import qualified BrainWall.Edge.Slope            as Slope
import           BrainWall.Polygon
import           BrainWall.Polygon.ContainsPoint
import           BrainWall.V2
import qualified Data.Vector                     as V

containsEdge :: Integral a => Edge a -> Polygon a -> Bool
containsEdge (Edge p q) polygon | p == q =
    pointInPolygon p polygon
containsEdge edge@(Edge p q) polygon =
    pointInPolygon p polygon &&
    pointInPolygon q polygon &&
    edgeLeadsInside (Edge p q) polygon &&
    edgeLeadsInside (Edge q p) polygon &&
    V.all
        (\e -> case edgeIntersection edge e of
            Intersecting    -> False
            Overlapping     -> True
            Touching        -> True
            NotIntersecting -> True)
        (polygonEdges polygon) &&
    V.all (insideCorner edge) (polygonCorners polygon)

crossesEdge :: (Real a) => Edge a -> Polygon a -> Bool
crossesEdge edge polygon = V.any (\e -> edgeIntersection edge e == Intersecting) (polygonEdges polygon)

-- | This deals with edge cases where the edge touches the polygon, e.g.:
--
--     o====o====o
--         / \
--
-- Where `=` is the polygon and `/` represents a possible edge.
edgeLeadsInside :: Integral a => Edge a -> Polygon a -> Bool
edgeLeadsInside (Edge p q) polygon = case polygonSplitEdge p polygon of
    Nothing     -> True
    Just (r, s) ->
        let pToQ = Slope.fromV2 (q .-. p)
            pToR = Slope.fromV2 (r .-. p)
            pToS = Slope.fromV2 (s .-. p) in
        pToQ `Slope.between` (pToS, pToR)


-- | This deals with edge cases where a corner of the polygon touches the
-- edge, e.g.:
--
--     o====o====o
--         / \
--
-- Where `=` is the edge and `/` represents part of the polygon.
insideCorner :: Integral a => Edge a -> (V2 a, V2 a, V2 a) -> Bool
insideCorner edge@(Edge p q) (prev, corner, next)
    | not $ corner `pointOnEdge` edge = True
    | otherwise                       =
        let toQ    = Slope.fromV2 (q    .-. corner)
            toP    = Slope.fromV2 (p    .-. corner)
            toNext = Slope.fromV2 (next .-. corner)
            toPrev = Slope.fromV2 (prev .-. corner) in
        (p == corner || (toP `Slope.between` (toNext, toPrev))) &&
        (q == corner || (toQ `Slope.between` (toNext, toPrev)))
