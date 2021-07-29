-- |

module BrainWall.Polygon.Simple where

import BrainWall.Edge
import BrainWall.Polygon.ContainsEdge
import BrainWall.Polygon.Internal
import BrainWall.V2
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Vector as V

isSimple :: (Integral a) => Polygon a -> Bool
isSimple polygon = and
  [ case edgeIntersection e1 e2 of
      Overlapping -> False
      Intersecting -> False
      _ -> True
  | e1 <- edges
  , e2 <- edges
  , e1 /= e2
  ] && -- no pairs of edges should overlap or intersect
  and
  [ edgeIntersection e1 e2 == NotIntersecting
  | (e1:_:edges') <- tails edges
  , e2 <- edges'
  ] -- non-adjacent edges shouldn't even touch
  where
    edges = V.toList $ polygonEdges polygon

problematicVertices :: (Integral a) => Polygon a -> [[V2 a]]
problematicVertices polygon = catMaybes
  [ case edgeIntersection e1 e2 of
      Overlapping -> let Edge v1 v2 = e1 in let Edge v3 v4 = e2 in Just [v1, v2, v3, v4]
      Intersecting -> let Edge v1 v2 = e1 in let Edge v3 v4 = e2 in Just [v1, v2, v3, v4]
      _ -> Nothing
  | e1 <- edges
  , e2 <- edges
  , e1 /= e2
  ] ++ -- no pairs of edges should overlap or intersect
  catMaybes
  [ if edgeIntersection e1 e2 == NotIntersecting then Nothing else let Edge v1 v2 = e1 in let Edge v3 v4 = e2 in Just [v1, v2, v3, v4]
  | let (e1:_:edges') = edges
  , e2 <- tail $ reverse edges'
  ] ++
  catMaybes
  [ if edgeIntersection e1 e2 == NotIntersecting then Nothing else let Edge v1 v2 = e1 in let Edge v3 v4 = e2 in Just [v1, v2, v3, v4]
  | (e1:_:edges') <- tail $ tails edges
  , e2 <- edges'
  ] -- non-adjacent edges shouldn't even touch
  where
    edges = V.toList $ polygonEdges polygon

simplifyPolygon :: (Integral a, Show a) => Polygon a -> Maybe (Polygon a)
simplifyPolygon polygon =
  case problematicVertices polygon of
    [] -> Just polygon
    (xs:_) -> case candidates of
        [] -> Nothing
        cs -> simplifyPolygon $ minimumBy (comparing $ \poly -> length $ problematicVertices poly) cs
      where
        isValid poly = V.all (`containsEdge` poly) (polygonEdges polygon) -- old polygon must fit inside new polygon
        candidates = filter isValid candidates'
        candidates' = catMaybes
          [ mkPolygon vertices
          | x <- xs
          , let vertices = V.filter (/= x) $ polygonVertices polygon
          ]
