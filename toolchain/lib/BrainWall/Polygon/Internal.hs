{-# LANGUAGE DeriveFunctor #-}
module BrainWall.Polygon.Internal
    ( Polygon (..)
    , mkPolygon
    , polygonSimplify
    , polygonVertices
    , polygonEdges
    , polygonCorners
    , polygonSplitEdge

    , clockwise
    ) where

import           BrainWall.Edge
import           BrainWall.V2
import qualified Data.Vector        as V

newtype Polygon a = Polygon {unPolygon :: V.Vector (V2 a)}
    deriving (Eq, Functor, Show)

polygonVertices :: Polygon a -> V.Vector (V2 a)
polygonVertices = unPolygon

mkPolygon :: (Num a, Ord a) => V.Vector (V2 a) -> Maybe (Polygon a)
mkPolygon v
    | V.length v < 3 = Nothing
    | otherwise      = Just . Polygon $ mkClockwise v

polygonSimplify :: Integral a => Polygon a -> Maybe (Polygon a)
polygonSimplify (Polygon vec)
    | V.length vec' < 3 = Nothing
    | otherwise         = Just $ Polygon vec'
  where
    vec' = V.ifilter (\j p ->
        let i = if j - 1 < 0 then V.length vec - 1 else j - 1
            k = if j + 1 >= V.length vec then 0 else j + 1 in
        not $ pointOnEdge p (Edge (vec V.! i) (vec V.! k))) vec

polygonEdges :: Polygon a -> V.Vector (Edge a)
polygonEdges (Polygon polygon) =
    V.zipWith Edge polygon (V.drop 1 polygon <> V.take 1 polygon)

-- | Utility for iterating over the corners of a polygon.  Returns (p, q, r)
-- for every corner q, where p and r are the previous and next vertices in
-- clockwise order.
polygonCorners :: Polygon a -> V.Vector (V2 a, V2 a, V2 a)
polygonCorners (Polygon polygon) = V.imap toCorner polygon
  where
    toCorner iq q =
        let ip = if iq - 1 < 0 then V.length polygon - 1 else iq - 1
            ir = if iq + 1 >= V.length polygon then 0 else iq + 1 in
        (polygon V.! ip, q, polygon V.! ir)

-- | If a point p lays on the edges of a polygon, obtain (q, r) so that
-- (q, p) and (p, r) are edges of the polygon in clockwise order.
polygonSplitEdge :: Integral a => V2 a -> Polygon a -> Maybe (V2 a, V2 a)
polygonSplitEdge p (Polygon polygon) = go 0
  where
    go i
        | i >= V.length polygon          = Nothing
        | p == q                         = Just (polygon V.! prev, r)
        | p == r                         = go (i + 1)  -- Find it next.
        | not (pointOnEdge p (Edge q r)) = go (i + 1)
        | otherwise                      = Just (q, r)
      where
        q = polygon V.! i
        r = polygon V.! next
        next = if i + 1 >= V.length polygon then 0 else i + 1
        prev = if i - 1 < 0 then V.length polygon - 1 else i - 1

mkClockwise :: (Num a, Ord a) => V.Vector (V2 a) -> V.Vector (V2 a)
mkClockwise verts = if clockwise verts then verts else V.reverse verts

clockwise :: (Num a, Ord a) => V.Vector (V2 a) -> Bool
clockwise verts = (<= 0) . V.sum . V.map area . polygonEdges . Polygon $
    V.map (.+. V2 offsetX offsetY) verts
  where
    -- https://stackoverflow.com/a/1165943
    area (Edge (V2 x1 y1) (V2 x2 y2)) = (x2 - x1) * (y2 + y1)

    -- Ensure X and Y coordinates are positive.
    offsetX = offset v2X
    offsetY = offset v2Y
    offset f
        | V.length verts <= 0 = 0
        | lo < 0              = -lo
        | otherwise           = 0
      where
        lo = V.minimum $ V.map f verts
