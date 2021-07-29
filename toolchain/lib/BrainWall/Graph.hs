{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Simple directed graph.
module BrainWall.Graph
    ( Graph (..)
    , fromEdges
    , undirectedFromEdges

    , toUniqueEdges

    , deleteEdge
    , undirectedDeleteEdge
    , filterVertices

    , reachable

    , numVertices
    , components
    ) where

import           Data.Foldable       (foldl')
import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet        as HS
import           Data.Maybe          (fromMaybe)
import qualified Data.Vector         as V

newtype Graph a = Graph {unGraph :: HMS.HashMap a (HS.HashSet a)}

fromEdges :: (Foldable f, Eq a, Hashable a) => f (a, a) -> Graph a
fromEdges = Graph . foldl' insertEdge mempty
  where
    insertEdge acc (x, y) = HMS.insertWith HS.union x (HS.singleton y) acc

-- | Creata a graph, adding edges in both directions.
undirectedFromEdges :: (Foldable f, Eq a, Hashable a) => f (a, a) -> Graph a
undirectedFromEdges = Graph . foldl' insertEdge mempty
  where
    insertEdge acc (x, y) =
        HMS.insertWith HS.union x (HS.singleton y) $
        HMS.insertWith HS.union y (HS.singleton x) $
        acc

-- | Traverse each edge (a, b) or (b, a) only once.
toUniqueEdges :: (Eq a, Hashable a) => Graph a -> V.Vector (a, a)
toUniqueEdges = go HS.empty . HMS.toList . unGraph
  where
    go _       []                = V.empty
    go visited ((x, ys) : nodes) =
        V.fromList [(x, y) | y <- HS.toList ys, not $ HS.member y visited] <>
        go (HS.insert x visited) nodes

undirectedDeleteEdge :: (Eq a, Hashable a) => (a, a) -> Graph a -> Graph a
undirectedDeleteEdge (x, y) = deleteEdge (x, y) . deleteEdge (y, x)

filterVertices :: (Eq a, Hashable a) => (a -> Bool) -> Graph a -> Graph a
filterVertices f = Graph .
    HMS.mapMaybeWithKey
        (\k nbs ->
            let nbs' = HS.filter f nbs in
            if not (f k) || HS.null nbs' then Nothing else Just nbs') .
    unGraph

deleteEdge :: (Eq a, Hashable a) => (a, a) -> Graph a -> Graph a
deleteEdge (x, y) = Graph . HMS.adjust (HS.delete x) y . unGraph

reachable :: (Eq a, Hashable a) => HS.HashSet a -> Graph a -> HS.HashSet a
reachable set0 (Graph m) = go mempty set0
  where
    go visited queue = case HS.toList queue of
        [] -> visited
        x : _
            | HS.member x visited -> go visited (HS.delete x queue)
            | otherwise           ->
                let !next = HS.filter (not . (`HS.member` visited)) $
                        fromMaybe HS.empty $ HMS.lookup x m in
                go (HS.insert x visited) (HS.delete x queue <> next)

numVertices :: (Eq a, Hashable a) => Graph a -> Int
numVertices (Graph g) = HS.size . HS.fromList $ do
    (p, nbs) <- HMS.toList g
    p : HS.toList nbs

components :: forall a. (Eq a, Hashable a) => Graph a -> [Graph a]
components graph@(Graph g) = resolve HS.empty $ HMS.keys g
  where
    resolve :: HS.HashSet a -> [a] -> [Graph a]
    resolve _        []          = []
    resolve resolved (x : xs)
        | x `HS.member` resolved = resolve resolved xs
        | otherwise              =
            let reach     = reachable (HS.singleton x) graph
                component = filterVertices (`HS.member` reach) graph in
            component : resolve (resolved <> reach) xs
