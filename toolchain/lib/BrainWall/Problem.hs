{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module BrainWall.Problem
    ( Hole (..)

    , VertexIndex
    , Figure' (..)
    , Figure
    , lookupFigureEdge
    , foldFigureEdges
    , figureFromEdges

    , BonusType (..)
    , bonusTypeFromText
    , Bonus (..)
    , Problem (..)
    , problemBox
    , ClaimedBonus (..)
    , Solution (..)

    , SolutionError (..)
    , BrokenLeg (..)
    , solutionBrokenLeg
    , solutionFigure
    , validateSolution
    , awardBonusesForSolution

    , Epsilon
    , epsilonDenominator

    , Dislikes
    , judgeSolution

    , positivizeProblem
    ) where

import           BrainWall.Box                   (Box)
import qualified BrainWall.Box                   as Box
import           BrainWall.Edge
import           BrainWall.Polygon
import qualified BrainWall.Polygon.ContainsEdge  as Polygon
import           BrainWall.Polygon.ContainsPoint
import           BrainWall.V2
import           Control.Lens                    (Prism', prism')
import           Control.Monad                   (guard)
import qualified Data.Foldable                   as F
import qualified Data.HashSet                    as HS
import           Data.List.NonEmpty              (NonEmpty (..))
import           Data.Maybe                      (maybeToList)
import           Data.Ratio
import qualified Data.Set                        as Set
import qualified Data.Text                       as T
import           Data.Vector                     (Vector)
import qualified Data.Vector                     as V

--------------------------------------------------------------------------------

-- | A hole is specified by a list of points.
newtype Hole = Hole {unHole :: Polygon Integer} deriving (Show)


--------------------------------------------------------------------------------

type VertexIndex = Int

data Figure' a = Figure
    { -- | List of original location of each vertex.
      figureVertices :: Vector (V2 a)
    , -- | List of edges that exist between the vertices.
      figureEdges    :: Vector (VertexIndex, VertexIndex)
    } deriving (Show)

type Figure = Figure' Integer

lookupFigureEdge :: (VertexIndex, VertexIndex) -> Figure -> Maybe (Edge Integer)
lookupFigureEdge (i, j) Figure {..}
    | i < 0 || i >= V.length figureVertices = Nothing
    | j < 0 || j >= V.length figureVertices = Nothing
    | otherwise                             = Just $
        Edge (figureVertices V.! i) (figureVertices V.! j)

foldFigureEdges :: Monoid m => (Edge Integer -> m) -> Figure -> m
foldFigureEdges f figure = foldMap f $ do
    (i, j) <- V.toList $ figureEdges figure
    maybeToList $ lookupFigureEdge (i, j) figure

figureFromEdges :: (Foldable f, Ord a) => f (Edge a) -> Figure' a
figureFromEdges edges = Figure (V.fromList $ Set.toAscList vertices) (V.fromList $ Set.toAscList figureEdges)
  where
    vertices = Set.fromList $ foldMap (\(Edge v1 v2) -> [v1, v2]) edges
    figureEdges = Set.fromList . map go $ F.toList edges
    go (Edge v1 v2) = if v1Idx < v2Idx then (v1Idx, v2Idx) else (v2Idx, v1Idx)
      where
        v1Idx = Set.findIndex v1 vertices
        v2Idx = Set.findIndex v2 vertices

--------------------------------------------------------------------------------

data BonusType
    = Globalist
    | SuperFlex
    | WallHack
    | BreakALeg
    deriving (Bounded, Enum, Eq, Ord, Show)

bonusTypeFromText :: Prism' T.Text BonusType
bonusTypeFromText = prism'
    (\case
        Globalist -> "GLOBALIST"
        SuperFlex -> "SUPERFLEX"
        WallHack  -> "WALLHACK"
        BreakALeg -> "BREAK_A_LEG")
    (\case
        "GLOBALIST"   -> Just Globalist
        "SUPERFLEX"   -> Just SuperFlex
        "WALLHACK"    -> Just WallHack
        "BREAK_A_LEG" -> Just BreakALeg
        _             -> Nothing)

data Bonus = Bonus
    { bonusProblem  :: Int
    , bonusBonus    :: BonusType
    , bonusPosition :: V2 Integer
    } deriving (Show)

data Problem = Problem
    { problemHole    :: Hole
    , problemFigure  :: Figure
    , problemEpsilon :: Epsilon
    , problemBonuses :: V.Vector Bonus
    } deriving (Show)

problemBox :: Problem -> Box Integer
problemBox Problem {..} = case mbBox of
    Nothing -> error "problemBox: empty!"  -- Impossible because hole is polygon
    Just b  -> b
  where
    mbBox =
        foldMap (Just . Box.fromV2) (polygonVertices (unHole problemHole)) <>
        foldMap (Just . Box.fromV2) (figureVertices problemFigure)

data ClaimedBonus = ClaimedBonus
    { claimedBonusProblem :: Int
    , claimedBonusBonus   :: BonusType
    , -- | Only applicable for some bonuses
      claimedBonusEdge    :: Maybe (VertexIndex, VertexIndex)
    } deriving (Show)

-- | A solution is a re-assignment of the vertex positions in the figure.
data Solution = Solution
    { solutionBonuses  :: Vector ClaimedBonus
    , solutionVertices :: Vector (V2 Integer)
    } deriving (Show)


data SolutionError
    = TooManyBonuses Int
    | VerticesLengthMismatch Int Int
    | EdgeLengthMismatch (Int, Int) (Integer, Integer) Epsilon
    | GlobalistEdgeLengthMismatch (Ratio Integer) (Ratio Integer)
    | PointNotInHole VertexIndex (V2 Integer)
    | EdgeNotInHole (VertexIndex, VertexIndex) (Edge Integer)
    | BrokenLegError String
    deriving (Eq, Show)

prettySolutionError :: SolutionError -> String
prettySolutionError (TooManyBonuses n) =
    "Not allowed to claim more then " ++ show n ++ " bonuses."
prettySolutionError (VerticesLengthMismatch expected actual) =
    "The number of vertices doesn't match: expected: " ++
    show expected ++ ", actual: " ++ show actual
prettySolutionError (EdgeLengthMismatch (i, j) (o, d) eps) =
    "Edge between " ++ show (i, j) ++ " has an invalid length: " ++
    "original: " ++ show o ++ ", pose: " ++ show d ++
    ", max epsilon: " ++ show eps
prettySolutionError (GlobalistEdgeLengthMismatch budget used) =
    "GLOBAL_EPSILON is " ++ show budget ++ " but total delta is " ++ show used
prettySolutionError (PointNotInHole _ point) =
    "The point at " ++ show point ++ " is not inside the hole"
prettySolutionError (EdgeNotInHole _ edge) =
    "The edge at " ++ show edge ++ " is not inside the hole."
prettySolutionError (BrokenLegError str) =
    "BREAK_A_LEG incorrectly used: " ++ str

--------------------------------------------------------------------------------

-- | Change a problem so that it fits entirely in the first quarter plane
positivizeProblem :: Problem -> Problem
positivizeProblem problem = problem
  { problemHole = Hole newHole
  , problemFigure = newFigure
  , problemBonuses = newBonuses
  }
  where
    holeVerts = polygonVertices $ unHole $ problemHole problem
    figureVerts = figureVertices $ problemFigure problem
    minX = min (V.minimum $ V.map v2X holeVerts) (V.minimum $ V.map v2X figureVerts)
    minY = min (V.minimum $ V.map v2Y holeVerts) (V.minimum $ V.map v2Y figureVerts)
    Just newHole = mkPolygon $ V.map (.+. V2 minX minY) holeVerts
    newFigure = (problemFigure problem) {figureVertices = V.map (.+. V2 minX minY) figureVerts}
    newBonuses = V.map (\bonus -> bonus {bonusPosition = bonusPosition bonus .+. V2 minX minY}) $ problemBonuses problem

--------------------------------------------------------------------------------

-- | Helper for dealing with the BreakALeg bonus.
data BrokenLeg
    = NoBrokenLeg
    | ErrorBrokenLeg String
    | OkBrokenLeg
        (VertexIndex, VertexIndex)  -- Original
        (VertexIndex, VertexIndex)  -- Broken p1
        (VertexIndex, VertexIndex)  -- Broken p2
    deriving (Show)

solutionBrokenLeg :: Problem -> Solution -> BrokenLeg
solutionBrokenLeg Problem {..} Solution {..} =
    case [e | ClaimedBonus _ BreakALeg e <- V.toList solutionBonuses] of
        [] -> NoBrokenLeg
        Nothing : _ -> ErrorBrokenLeg "Leg to break must be specified"
        Just (i, j) : _
            | (i, j) `V.elem` figureEdges problemFigure ->
                let k = V.length (figureVertices problemFigure) in
                OkBrokenLeg (i, j) (i, k) (j, k)
            | (j, i) `V.elem` figureEdges problemFigure ->
                let k = V.length (figureVertices problemFigure) in
                OkBrokenLeg (j, i) (i, k) (j, k)
            | otherwise -> ErrorBrokenLeg "Broken leg does not exist"


solutionFigure :: Problem -> Solution -> Figure
solutionFigure problem@Problem {..} solution@Solution {..} = problemFigure
    { figureVertices = solutionVertices
    , figureEdges    = case solutionBrokenLeg problem solution of
            OkBrokenLeg original new1 new2 ->
                V.filter (/= original) (figureEdges problemFigure) <>
                V.fromList [new1, new2]
            _ -> figureEdges problemFigure
    }

validateSolution :: Problem -> Solution -> [SolutionError]
validateSolution problem@Problem {..} solution@Solution {..} =
    withWallHack .
    withSuperFlex $
    -- Check that only one bonus is claimed.
    (do
        guard $ length solutionBonuses > 1
        pure $ TooManyBonuses 1) ++
    -- Check that the number of vertices matches.
    (do
        let original = V.length $ figureVertices problemFigure
            expected = if haveBonus BreakALeg then original + 1 else original
            actual = V.length $ figureVertices defigured
        guard $ expected /= actual
        pure $ VerticesLengthMismatch expected actual) ++
    -- Check that the distances where preserved.
    (let edgeSquaredLengths = do
            (i, j) <- V.toList $ figureEdges defigured
            d'2 <- fmap edgeSquaredLength . maybeToList $
                lookupFigureEdge (i, j) defigured
            -- Note that if this is a broken edge, d2 is nothing which is fine,
            -- we will check that separately.
            d2 <- fmap edgeSquaredLength . maybeToList $
                lookupFigureEdge (i, j) problemFigure
            pure (i, j, d2, d'2) in
    -- Check that legs were broken correctly.
    (case brokenLeg of
        NoBrokenLeg -> []
        ErrorBrokenLeg err -> pure $ BrokenLegError err
        OkBrokenLeg original a b -> do
            d2 <- fmap edgeSquaredLength . maybeToList $
                lookupFigureEdge original problemFigure
            ad'2 <- fmap edgeSquaredLength . maybeToList $
                lookupFigureEdge a defigured
            bd'2 <- fmap edgeSquaredLength . maybeToList $
                lookupFigureEdge b defigured
            -- TODO
            guard . not $ (abs (4 * ad'2 % d2 - 1) <= problemEpsilon % epsilonDenominator) &&
                          (abs (4 * bd'2 % d2 - 1) <= problemEpsilon % epsilonDenominator)
            pure $ BrokenLegError "Length error for broken legs") ++
     case haveBonus Globalist of
        -- Check edge deltas individually.
        False -> do
            (i, j, d2, d'2) <- edgeSquaredLengths
            guard . not $ abs (d'2 % d2 - 1) <= problemEpsilon % epsilonDenominator
            pure $ EdgeLengthMismatch (i, j) (d2, d'2) problemEpsilon
        -- Use a global budget for the edge deltas.
        True -> do
            let rhss = fromIntegral (length edgeSquaredLengths) * problemEpsilon % epsilonDenominator
                lhss = sum $ do
                    (_, _, d2, d'2) <- edgeSquaredLengths
                    pure $ abs (d'2 % d2 - 1)

            guard . not $ lhss <= rhss
            pure $ GlobalistEdgeLengthMismatch rhss lhss) ++
    -- All vertices of the defigured version must be inside the hole.
    (do
        (i, point) <- V.toList . V.indexed $ solutionVertices
        guard . not $ pointInPolygon point polygon
        pure $ PointNotInHole i point) ++
    -- Edges from the defigured version may not intersect with the hole,
     -- but they may touch.
    (do
        (i,j) <- V.toList $ figureEdges defigured
        defiguredEdge <- maybeToList $ lookupFigureEdge (i, j) defigured
        guard . not $ Polygon.containsEdge defiguredEdge polygon
        pure $ EdgeNotInHole (i, j) defiguredEdge)
  where
    defigured = solutionFigure problem solution
    polygon = unHole problemHole

    haveBonus ty = V.any ((== ty) . claimedBonusBonus) solutionBonuses
    brokenLeg = solutionBrokenLeg problem solution

    -- Superflex basically comes down to ignoring a single edge length mismatch.
    withSuperFlex
        | not $ haveBonus SuperFlex = id
        | otherwise                 = \case
             [EdgeLengthMismatch {}] -> []
             errs                    -> errs

    withWallHack errs = case [i | PointNotInHole i _p <- errs] of
        _ | not $ haveBonus WallHack -> errs
        []                           -> errs
        _ : _ : _                    -> errs
        [single]                     -> filter (not . wallHack single) errs
      where
        wallHack i (PointNotInHole j _)     = i == j
        wallHack i (EdgeNotInHole (j, k) _) = i == j || i == k
        wallHack _ _                        = False


awardBonusesForSolution :: Problem -> Solution -> [(Bonus, Bool)]
awardBonusesForSolution Problem {..} Solution {..} = do
    bonus <- V.toList problemBonuses
    pure (bonus, bonusPosition bonus `HS.member` verts)
  where
    verts = HS.fromList $ V.toList solutionVertices


--------------------------------------------------------------------------------

-- | Epsilon allowed for edge distance.
type Epsilon = Integer

epsilonDenominator :: Integer
epsilonDenominator = 1000000

--------------------------------------------------------------------------------

type Dislikes = Integer

unsafeDislikesSolution :: Problem -> Solution -> Dislikes
unsafeDislikesSolution Problem {..} Solution {..} =
    V.sum . fmap scoreVertex . unPolygon $ unHole problemHole
  where
    scoreVertex v =
        let distances = squaredDistance v <$> solutionVertices in
        if V.null distances then 0 else V.minimum distances

judgeSolution :: Problem -> Solution -> Either (NonEmpty String) Integer
judgeSolution problem solution = case validateSolution problem solution of
    []     -> Right $ unsafeDislikesSolution problem solution
    e : es -> Left . fmap prettySolutionError $ e :| es
