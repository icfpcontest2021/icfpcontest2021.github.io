-- |
module BrainWall.Planar where

import BrainWall.Polygon
import BrainWall.Problem
import BrainWall.V2 as V2
import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Ord (comparing)
import qualified Data.Vector as V

data PlanarGraph a = PlanarGraph (Maybe (a, a)) (HMS.HashMap a [a])
  deriving (Show)

figureGraph :: (Num a, Real a) => Figure' a -> PlanarGraph VertexIndex
figureGraph figure = PlanarGraph outsideEdge orderedGraph
  where
    graph = L.foldl' insertEdge HMS.empty $ figureEdges figure
    insertEdge acc (i, j) =
      HMS.insertWith HS.union i (HS.singleton j) $
        HMS.insertWith HS.union j (HS.singleton i) $
          acc

    orderedGraph = HMS.mapWithKey orderByAngle graph

    orderByAngle i js = map snd $
      L.sortOn fst $ do
        let p = figureVertices figure V.! i
        j <- HS.toList js
        let q = figureVertices figure V.! j
            V2 x y = q .-. p
            angle = atan2 (realToFrac y) (realToFrac x) :: Double
        pure (if angle < 0 then pi * 2 + angle else angle, j)

    firstEdges = [(i, j) | (i, j : _) <- HMS.toList orderedGraph]
    outsideEdge = case firstEdges of
      [] -> Nothing
      _ ->
        Just $
          L.minimumBy
            (comparing (\(i, _) -> v2Y $ figureVertices figure V.! i))
            firstEdges

type Face a = NonEmpty a

faceToPolygon :: (Integral a) => Face (V2 a) -> Maybe (Polygon a)
faceToPolygon face = do
  p <- mkPolygon . V.fromList $ NonEmpty.toList face
  polygonSimplify p

outline :: (Eq a, Hashable a) => PlanarGraph a -> Maybe (Face a)
outline (PlanarGraph mbOutsideEdge graph) = do
  outsideEdge <- mbOutsideEdge
  let faces = planarGraphFaces' graph
  case filter (any (== outsideEdge)) faces of
    [face] -> Just $ NonEmpty.map fst face
    _ -> Nothing

planarGraphFaces :: (Eq a, Hashable a) => PlanarGraph a -> [Face a]
planarGraphFaces (PlanarGraph mbOutsideEdge graph) =
  map (NonEmpty.map fst) $
    ( case mbOutsideEdge of
        Nothing -> id
        Just outsideEdge -> filter (not . any (== outsideEdge))
    )
      $ planarGraphFaces' graph

planarGraphFaces' :: (Eq a, Hashable a) => HMS.HashMap a [a] -> [Face (a, a)]
planarGraphFaces' graph =
  go HS.empty $ do
    (i, js) <- HMS.toList graph
    j <- js
    pure (i, j)
  where
    go _ [] = []
    go visited ((i, j) : queue)
      | (i, j) `HS.member` visited = go visited queue
      | otherwise = case walk (i, j) of
        Nothing -> go visited queue
        Just cycl ->
          cycl : go (visited <> HS.fromList (NonEmpty.toList cycl)) queue

    walk start =
      let walk' edge
            | edge == start = Just $ edge :| []
            | otherwise = NonEmpty.cons edge <$> (next edge >>= walk')
       in next start >>= walk'

    next (i, j) = do
      ks <- HMS.lookup j graph
      case break (== i) (ks ++ ks) of
        (_, _ : k : _) -> Just (j, k)
        _ -> Nothing
