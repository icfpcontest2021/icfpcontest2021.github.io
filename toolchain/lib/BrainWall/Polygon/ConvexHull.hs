{-# LANGUAGE ScopedTypeVariables #-}

-- |
module BrainWall.Polygon.ConvexHull where

import BrainWall.Polygon.Internal
import BrainWall.V2
import Data.Maybe
import Data.Ord
import qualified Data.Vector as V

convexHull :: forall a. (Real a, Ord a) => V.Vector (V2 a) -> Polygon a
convexHull points = fromJust $ mkPolygon hullPoints
  where
    leftmost = V.minimumBy (comparing v2X) points
    hullPoints = V.cons leftmost $ V.unfoldr go (leftmost, V2 0 1)
    dot (V2 x1 y1) (V2 x2 y2) = realToFrac $ x1 * x2 + y1 * y2
    norm :: V2 a -> Double
    norm v = sqrt $ dot v v
    go (prevPoint, prevDir) =
      if nextPoint == leftmost then Nothing else Just (nextPoint, (nextPoint, nextPoint .-. prevPoint))
      where
        nextPoint =
          V.maximumBy
            ( comparing $ \v2 ->
                let newDir = v2 .-. prevPoint
                 in dot newDir prevDir / norm newDir / norm prevDir
            )
            $ V.filter (/= prevPoint) points
