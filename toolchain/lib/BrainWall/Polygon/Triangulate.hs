{-# LANGUAGE DeriveFunctor #-}
module BrainWall.Polygon.Triangulate
    ( earCutting
    ) where

import           BrainWall.Edge
import           BrainWall.Polygon.ContainsEdge
import           BrainWall.Polygon.Internal
import           BrainWall.Triangle
import qualified Data.Vector                    as V

-- | Triangulate a polygon using the ear-cutting algorithm
earCutting :: Integral a => Polygon a -> Maybe [Triangle a]
earCutting (Polygon polygon) = case compare (V.length polygon) 3 of
    LT -> Nothing
    EQ -> pure [Triangle (polygon V.! 0) (polygon V.! 1) (polygon V.! 2)]
    GT -> case V.find isEar (V.indexed polygon) of
        Nothing -> Nothing
        Just (i, p) ->
            (Triangle (polygon V.! prev i) p (polygon V.! next i) :) <$>
            earCutting (Polygon $ V.take i polygon <> V.drop (i + 1) polygon)
  where
    next i = if i + 1 >= V.length polygon then 0 else i + 1
    prev i = if i - 1 < 0 then V.length polygon - 1 else i - 1

    isEar (i, _) =
        let q = polygon V.! prev i
            r = polygon V.! next i in
        containsEdge (Edge q r) (Polygon polygon)
