{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}
module BrainWall.Polygon.ContainsPoint
    ( Ray (..)
    , RayEdgeIntersection (..)
    , rayEdgeIntersection

    , pointInPolygon
    , rationalPointInPolygon
    ) where

import           BrainWall.Edge
import           BrainWall.Polygon
import           BrainWall.V2
import           Control.Monad     (guard)
import qualified Data.List         as L
import           Data.Maybe        (maybeToList)
import           Data.Ratio        (Ratio)
import qualified Data.Vector       as V

-- | A ray travels horizontally to the right infinitely.
newtype Ray a = Ray (V2 a)

data RayEdgeIntersection a
    = HorizontalOverlapping a a  -- Left and right X coordinates of the overlap
    | VertexIntersection a       -- Intersection at vertex
    | Intersection a             -- Intersection at coordinate
    deriving (Eq, Show)

rayEdgeIntersection
    :: (Fractional a, Ord a) => Ray a -> Edge a -> Maybe (RayEdgeIntersection a)
rayEdgeIntersection (Ray (V2 rayX rayY)) (Edge (V2 pX pY) (V2 qX qY))
    -- Edge is entirely to the left of the ray.
    | pX < rayX && qX < rayX = Nothing
    -- Edge is entirely above the ray.
    | pY < rayY && qY < rayY = Nothing
    -- Edge is entirely below the ray.
    | pY > rayY && qY > rayY = Nothing
    -- Edge is on the ray.
    | pY == qY && pY == rayY = Just $ HorizontalOverlapping
        (max rayX (min pX qX)) (max pX qX)
    -- p is on the ray.
    | pY == rayY && pX >= rayX = Just $ VertexIntersection pX
    -- q is on the ray.
    | qY == rayY && qX >= rayX = Just $ VertexIntersection qX
    -- Edge is parallel to the ray (but not on it).
    | pY == qY = Nothing
    -- Most general case.
    | otherwise =
        let t = (rayY - pY) / (qY - pY)
            x = pX + t * (qX - pX) in
        if t >= 0 && t <= 1 && x >= rayX
            then Just $ Intersection x
            else Nothing

rayBelowEdge :: Ord a => Ray a -> Edge a -> Bool
rayBelowEdge (Ray (V2 _ rayY)) (Edge (V2 _ pY) (V2 _ qY)) =
    rayY >= pY && rayY >= qY

pointInPolygon :: Integral a => V2 a -> Polygon a -> Bool
pointInPolygon point =
    rationalPointInPolygon (fmap fromIntegral point)

rationalPointInPolygon :: Integral a => V2 (Ratio a) -> Polygon a -> Bool
rationalPointInPolygon point polygon
    | any (rationalPointOnEdge point) (polygonEdges polygon) = True
    | otherwise                                              =
        odd $ crossings (0 :: Int) intersections
  where
    edges = fmap fromIntegral <$> polygonEdges polygon

    crossings !n = \case
        (HorizontalOverlapping _ _, _) : is -> crossings n is
        (Intersection _, _) : is -> crossings (n + 1) is
        (VertexIntersection _, e0) : (VertexIntersection _, e1) : is
            | rayBelowEdge (Ray point) e0 == rayBelowEdge (Ray point) e1 ->
                crossings n is
            | otherwise -> crossings (n + 1) is
        (VertexIntersection _, _) : is -> crossings (n + 1) is
        [] -> n

    intersections = L.sortOn (intersectionX . fst) $ do
        edge <- V.toList edges
        intersection <- maybeToList $ rayEdgeIntersection (Ray point) edge
        guard $ case intersection of
            HorizontalOverlapping _ _ -> False
            _                         -> True
        pure (intersection, edge)

    intersectionX = \case
        HorizontalOverlapping l _ -> l
        VertexIntersection x      -> x
        Intersection x            -> x
