{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RecordWildCards #-}
module BrainWall.JasperSolver.Limbs
    ( Limb (..)
    , Limbs
    , findLimbs
    , alternateLimb
    , moveLimb
    ) where

import           BrainWall.Circle  (Circle (..))
import qualified BrainWall.Circle  as Circle
import           BrainWall.Graph
import           BrainWall.Problem
import           BrainWall.V2
import           Control.Monad     (guard)
import qualified Data.HashSet      as HS
import qualified Data.Vector       as V

data Limb = Limb
    (VertexIndex, VertexIndex)  -- (p, q)
    (VertexIndex, VertexIndex)  -- (r, s)
    (HS.HashSet VertexIndex)    -- connected to p and r
    (HS.HashSet VertexIndex)    -- connected to q and s
    deriving (Show)

type Limbs = V.Vector Limb

findLimbs :: Figure -> Limbs
findLimbs Figure {..} = V.fromList $ do
    i <- [0 .. V.length uniqueEdges - 1]
    j <- [i + 1 .. V.length uniqueEdges - 1]
    let (p, q) = uniqueEdges V.! i
        (r, s) = uniqueEdges V.! j
        graph' = undirectedDeleteEdge (p, q) $ undirectedDeleteEdge (r, s) graph

        reachableFromP = reachable (HS.singleton p) graph'
        reachableFromQ = reachable (HS.singleton q) graph'
        reachableFromR = reachable (HS.singleton r) graph'

    -- We must cut the graph
    guard . not $ r == p || r == q || s == p || s == q
    guard . not $ HS.member q reachableFromP
    guard . not $ HS.member s reachableFromR
    if | HS.member s reachableFromP && HS.member q reachableFromR ->
            pure $ Limb (p, q) (s, r) reachableFromP reachableFromR
       | HS.member r reachableFromP && HS.member s reachableFromQ ->
            pure $ Limb (p, q) (r, s) reachableFromP reachableFromQ
       | otherwise ->
            []
  where
    uniqueEdges = toUniqueEdges graph
    graph = undirectedFromEdges figureEdges

-- | We can "pin" each point in the limb, leading to four valid transformations.
alternateLimb :: Limb -> V.Vector Limb
alternateLimb (Limb (p, q) (r, s) pr qs) = V.fromList
    [ Limb (p, q) (r, s) pr qs
    , Limb (r, s) (p, q) pr qs
    , Limb (q, p) (s, r) qs pr
    , Limb (s, r) (q, p) qs pr
    ]

-- | P and R remain stationary, Q moves by a polar coordinate offset and we
-- need to determine the new position for S.
moveLimb
    :: Double -> Limb -> V.Vector (V2 Double) -> Maybe (V.Vector (V2 Double))
moveLimb offset (Limb (ip, iq) (ir, is) inPR _) original =
    let p = original V.! ip
        q = original V.! iq
        r = original V.! ir
        s = original V.! is
        q' = case toPolar (q .-. p) of
            Polar rho phi -> p .+. fromPolar (Polar rho (phi + offset))
        qs = distance q s
        rs = distance r s in
    case Circle.circleCircleIntersection (Circle q' qs) (Circle r rs) of
        Circle.Intersecting s0 s1 ->
            let s' = if distance s0 s < distance s1 s then s0 else s1
                Polar _ phiQToS = toPolar (s .-. q)
                Polar _ phiQToS' = toPolar (s' .-. q')
                phiOffset = phiQToS' - phiQToS in
            Just $ flip V.imap original $ \i o ->
                if | i == iq            -> q'
                   | i == is            -> s'
                   | i `HS.member` inPR -> o
                   | otherwise          ->
                       let Polar rho phi = toPolar (o .-. q) in
                       q' .+. fromPolar (Polar rho (phi + phiOffset))
        _ -> Nothing
