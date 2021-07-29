{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
module BrainWall.Random where

import BrainWall.Edge
import qualified BrainWall.Main.GenerateHole as GenHole
import qualified BrainWall.Main.GenerateFigure as GenFig
import BrainWall.Json
import BrainWall.Polygon
import BrainWall.Polygon.ContainsEdge
import BrainWall.Problem
import BrainWall.V2
import Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.List
import Data.Ord
import Data.Ratio
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Debug.Trace
import Numeric.GSL.Minimization
import Numeric.LinearAlgebra
import System.Directory
import System.Random.MWC as MWC
import System.Random.Stateful

chooseFrom :: StatefulGen g m => V.Vector x -> g -> m x
chooseFrom v gen = do
  idx <- uniformRM (0, V.length v - 1) gen
  return $ v V.! idx

genFigureFromPolygon :: StatefulGen g m => Polygon Integer -> g -> m Figure
genFigureFromPolygon polygon gen = do
  edges <- replicateM 50 genEdge
  return $ figureFromEdges $ filter isGood edges --
  where
    vertices = unPolygon polygon
    genEdge = do
      v1 <- chooseFrom vertices gen
      v2 <- chooseFrom vertices gen
      return $ Edge v1 v2
    isGood edge@(Edge v1 v2) = v1 /= v2 && containsEdge edge polygon

data EpsilonType
  = ForwardEpsilon -- The mutated figure will be the target figure
  | ReverseEpsilon -- The mutated figure will be the initial figure
  deriving (Enum, Bounded, Eq, Show)

data InitialPositionType
  = StartFigurePosition
  | RandomPosition
  deriving (Enum, Bounded, Eq, Show)

data ObjectiveType
  = Crumple
  | Stretch
  deriving (Enum, Bounded, Eq, Show)

data FigureSource
  = HandDrawn
  | PolyTree
  | FromHoleGenerator
  deriving (Enum, Bounded, Eq, Show)

randomGenerateFigure :: StatefulGen g IO => FigureSource -> g -> IO Figure
randomGenerateFigure PolyTree gen =
  figureFromEdges . GenFig.bfEdges <$> GenFig.generateFigure GenFig.defaultOptions gen
randomGenerateFigure FromHoleGenerator gen = do
  poly <- GenHole.generateHole GenHole.defaultOptions gen
  let problemHole = case GenHole.polyToHole poly of
        Just p  -> p
        Nothing -> error "error: no polygon"
  GenHole.genFigureInHole (GenHole.optionsFigure GenHole.defaultOptions) problemHole gen
randomGenerateFigure HandDrawn gen = do
  examples <- listDirectory "examples"
  let problems = V.fromList $ map ("examples/" ++) $ filter isProblem examples
  problemPath <- chooseFrom problems gen
  Just value <- Aeson.decodeFileStrict problemPath
  let figure = case Aeson.parse (\(Aeson.Object obj) -> obj Aeson..: T.pack "figure" >>= decodeFigure ) value of
        Aeson.Success p -> p
        Aeson.Error str -> error str
  pure figure
  where
    isProblem "" = False
    isProblem ".problem" = True
    isProblem (_:xs) = isProblem xs

randomMutateFigure ::
  forall m g.
  StatefulGen g m =>
  InitialPositionType ->
  EpsilonType ->
  ObjectiveType ->
  Figure ->
  (Integer, Integer) ->
  g ->
  m (Figure, Epsilon)
randomMutateFigure initialPositionType epsilonType objectiveType startFigure (canvasWidth, canvasHeight) gen = do
  initialVec <- case initialPositionType of
    StartFigurePosition -> pure $ V.map (fmap fromIntegral) $ figureVertices startFigure
    RandomPosition -> V.replicateM numVertices genPosition
  objective <- case objectiveType of
    Crumple -> do
      targetPositions <- V.replicateM numVertices genPosition
      pure $ negateObjective $ targetObjective targetPositions
    Stretch -> do
      vertexPairs <- genVertexPairs (V.length $ figureVertices startFigure) numPairs gen
      pure $ negateObjective $ distanceSumObjective vertexPairs
  return $ randomMutateFigureWithObjective initialVec epsilonType startFigure objective
  where
    numPairs = 50
    numVertices = V.length $ figureVertices startFigure
    genPosition :: m (V2 Double)
    genPosition = do
      x <- uniformRM (0, fromIntegral canvasWidth) gen
      y <- uniformRM (0, fromIntegral canvasHeight) gen
      return $ V2 x y

genVertexPairs :: forall m g. StatefulGen g m => Int -> Int -> g -> m (V.Vector (Int, Int))
genVertexPairs numVertices numPairs gen = do
  pairs <- replicateM numPairs genPair
  return $ V.fromList $ Set.toAscList $ Set.fromList $ filter (\(x, y) -> x /= y) pairs
  where
    genPair :: m (Int, Int)
    genPair = do
      x <- uniformRM (0, numVertices - 1) gen
      y <- uniformRM (0, numVertices - 1) gen
      return $ if x < y then (x, y) else (y, x)

data PositionObjectiveWithGradientHessian = PositionObjectiveWithGradientHessian
  { posObjective :: V.Vector (V2 Double) -> Double,
    posGradient :: V.Vector (V2 Double) -> VS.Vector Double,
    posHessian :: V.Vector (V2 Double) -> Matrix Double
  }

targetObjective :: V.Vector (V2 Double) -> PositionObjectiveWithGradientHessian
targetObjective targetPositions =
  PositionObjectiveWithGradientHessian
    { posObjective,
      posGradient,
      posHessian
    }
  where
    posObjective positions = V.sum $ V.zipWith dist targetPositions positions
    numVertices = V.length targetPositions
    posGradient positions =
      VS.convert $
        V.concat
          [ V.zipWith (\(V2 tx _) (V2 x _) -> 2 * (x - tx)) targetPositions positions,
            V.zipWith (\(V2 _ ty) (V2 _ y) -> 2 * (y - ty)) targetPositions positions
          ]
    posHessian _ = build (2 * numVertices, 2 * numVertices) $ \i j -> if i == j then 2 else 0
    dist v1 v2 = sq (v2X v1 - v2X v2) + sq (v2Y v1 - v2Y v2)
    sq x = x * x

negateObjective :: PositionObjectiveWithGradientHessian -> PositionObjectiveWithGradientHessian
negateObjective = scaleObjective (-1)

scaleObjective :: Double -> PositionObjectiveWithGradientHessian -> PositionObjectiveWithGradientHessian
scaleObjective mult oldObj =
  PositionObjectiveWithGradientHessian
    { posObjective = (*) mult . posObjective oldObj,
      posGradient = VS.map ((*) mult) . posGradient oldObj,
      posHessian = scale mult . posHessian oldObj
    }

distanceSumObjective :: V.Vector (Int, Int) -> PositionObjectiveWithGradientHessian
distanceSumObjective vertexPairs =
  PositionObjectiveWithGradientHessian
    { posObjective,
      posGradient,
      posHessian
    }
  where
    posObjective positions = V.sum $ V.map go vertexPairs
      where
        go (i, j) = dist posI posJ
          where
            posI = positions V.! i
            posJ = positions V.! j
    posGradient positions = VS.fromList $ map (go v2X) [0 .. numVertices - 1] ++ map (go v2Y) [0 .. numVertices - 1]
      where
        numVertices = V.length positions
        go get k = V.sum $ V.map go2 vertexPairs
          where
            go2 (i, j)
              | posI == posJ = 0
              | k == i = (get posI - get posJ) / dist posI posJ
              | k == j = (get posJ - get posI) / dist posI posJ
              | otherwise = 0
              where
                posI = positions V.! i
                posJ = positions V.! j
    posHessian positions = fromBlocks [[makeHessian goXX, makeHessian goXY], [makeHessian goXY, makeHessian goYY]]
      where
        numVertices = V.length positions
        get = (positions V.!)
        makeHessian aux =
          fromLists [[aux rowIdx colIdx | colIdx <- [0 .. numVertices - 1]] | rowIdx <- [0 .. numVertices - 1]]

        goXX rowIdx colIdx
          | rowIdx == colIdx =
            V.sum $
              V.map (\(i, j) -> let d = dist (get i) (get j) in if d == 0 then 0 else 1 / d - sq (v2X $ get i .-. get j) / cube d) $
                V.filter (\(i, j) -> i == rowIdx || j == rowIdx) vertexPairs
          | otherwise =
            V.sum $
              V.map (\(i, j) -> let d = dist (get i) (get j) in if d == 0 then 0 else - sq (v2Y $ get i .-. get j) / cube d) $
                V.filter (\(i, j) -> i == min colIdx rowIdx && j == max colIdx rowIdx) vertexPairs
        goYY rowIdx colIdx
          | rowIdx == colIdx =
            V.sum $
              V.map (\(i, j) -> let d = dist (get i) (get j) in if d == 0 then 0 else 1 / d - sq (v2Y $ get i .-. get j) / cube d) $
                V.filter (\(i, j) -> i == rowIdx || j == rowIdx) vertexPairs
          | otherwise =
            V.sum $
              V.map (\(i, j) -> let d = dist (get i) (get j) in if d == 0 then 0 else - sq (v2X $ get i .-. get j) / cube d) $
                V.filter (\(i, j) -> i == min colIdx rowIdx && j == max colIdx rowIdx) vertexPairs
        goXY rowIdx colIdx
          | rowIdx == colIdx =
            V.sum $
              V.map (\(i, j) -> let d = dist (get i) (get j) in if d == 0 then 0 else - (v2X $ get i .-. get j) * (v2Y $ get i .-. get j) / cube d) $
                V.filter (\(i, j) -> i == rowIdx || j == rowIdx) vertexPairs
          | otherwise =
            V.sum $
              V.map (\(i, j) -> let d = dist (get i) (get j) in if d == 0 then 0 else (v2X $ get i .-. get j) * (v2Y $ get i .-. get j) / cube d) $
                V.filter (\(i, j) -> i == min colIdx rowIdx && j == max colIdx rowIdx) vertexPairs

    dist v1 v2 = sqrt $ sq (v2X v1 - v2X v2) + sq (v2Y v1 - v2Y v2)
    sq x = x * x
    cube x = x * x * x

randomMutateFigureWithObjective :: V.Vector (V2 Double) -> EpsilonType -> Figure -> PositionObjectiveWithGradientHessian -> (Figure, Epsilon)
randomMutateFigureWithObjective initialPos epsilonType startFigure PositionObjectiveWithGradientHessian {posObjective, posGradient, posHessian} =
  traceShow ("rows", rows path) $
    traceShow ("objective", objective solution, epsilon) $
      traceShow ("distances", V.zip4 distances finalDistances roundedDistances epsilons) $
        traceShow (newPositions, newMultipliers) $
          traceShow ("gradients", lagrangian (newPositions, newMultipliers), lagrangianGradient (newPositions, newMultipliers), lagrangianHessian (newPositions, newMultipliers) #> lagrangianGradient (newPositions, newMultipliers)) $
            (Figure roundedPositions (figureEdges startFigure), epsilon)
  where
    initialVec = VS.convert $ V.map v2X initialPos V.++ V.map v2Y initialPos V.++ V.replicate numEdges 0
    lagrangian :: (V.Vector (V2 Double), V.Vector Double) -> Double
    lagrangian (positions, multipliers) =
      posObjective positions
        + V.sum (V.zipWith3 (\mult edge realDist -> mult * (distIdx positions edge - fromIntegral realDist) / sqrt (fromIntegral realDist)) multipliers (figureEdges startFigure) distances)
    lagrangianGradient :: (V.Vector (V2 Double), V.Vector Double) -> VS.Vector Double
    lagrangianGradient (positions, multipliers) =
      ( posGradient positions
          + VS.concat
            [ VS.map (edgeGradient v2X) (VS.enumFromN 0 numVertices),
              VS.map (edgeGradient v2Y) (VS.enumFromN 0 numVertices)
            ]
      )
        VS.++ V.convert (V.zipWith (\edge realDist -> (distIdx positions edge - fromIntegral realDist) / sqrt (fromIntegral realDist)) (figureEdges startFigure) distances)
      where
        edgeGradient get vIdx =
          V.sum $
            V.zipWith3
              ( \(v1Idx, v2Idx) mult realDist ->
                  case () of
                    _
                      | vIdx == v1Idx -> mult * 2 * (get (positions V.! vIdx) - get (positions V.! v2Idx)) / sqrt (fromIntegral realDist)
                      | vIdx == v2Idx -> mult * 2 * (get (positions V.! vIdx) - get (positions V.! v1Idx)) / sqrt (fromIntegral realDist)
                      | otherwise -> 0
              )
              (figureEdges startFigure)
              multipliers
              distances
    lagrangianHessian (positions, multipliers) =
      fromBlocks
        [ [topLeftHessian, tr' bottomLeftHessian],
          [bottomLeftHessian, multHessian]
        ]
      where
        multHessian :: Matrix Double
        multHessian = build (numEdges, numEdges) $ \_ _ -> 0
        xMultHessian get = build (numEdges, numVertices) $ \eIdx vIdx ->
          let realDist = distances V.! round eIdx
              (v1Idx, v2Idx) = figureEdges startFigure V.! round eIdx
           in case () of
                _
                  | round vIdx == v1Idx -> 2 / sqrt (fromIntegral realDist) * (get (positions V.! v1Idx) - get (positions V.! v2Idx))
                  | round vIdx == v2Idx -> 2 / sqrt (fromIntegral realDist) * (get (positions V.! v2Idx) - get (positions V.! v1Idx))
                  | otherwise -> 0
        topLeftHessian =
          fromBlocks
            [ [xxHessian, 0],
              [0, xxHessian]
            ]
            + posHessian positions
        bottomLeftHessian = fromBlocks [[xMultHessian v2X, xMultHessian v2Y]]
        xxHessian :: Matrix Double
        xxHessian = build (numVertices, numVertices) $ \i j ->
          V.sum $
            V.zipWith3
              ( \(v1Idx, v2Idx) mult realDist ->
                  case () of
                    _
                      | round i == v1Idx && round j == v1Idx -> mult * 2 / sqrt (fromIntegral realDist)
                      | round i == v2Idx && round j == v2Idx -> mult * 2 / sqrt (fromIntegral realDist)
                      | round i == v1Idx && round j == v2Idx -> - mult * 2 / sqrt (fromIntegral realDist)
                      | round i == v2Idx && round j == v1Idx -> - mult * 2 / sqrt (fromIntegral realDist)
                      | otherwise -> 0
              )
              (figureEdges startFigure)
              multipliers
              distances

    objective = VS.sum . VS.map sq . lagrangianGradient . splitParams . VS.convert
    -- (solution, path) =
    --   minimizeV
    --     NMSimplex2
    --     1e-9
    --     1000000
    --     (VS.replicate numParams 1)
    --     objective
    --     initialVec
    (solution, path) =
      minimizeVD
        VectorBFGS2
        1e-9
        1000
        0.01
        0.1
        objective
        ((\(positions, multipliers) -> scale 2 $ lagrangianHessian (positions, multipliers) #> lagrangianGradient (positions, multipliers)) . splitParams . VS.convert)
        initialVec
    -- initialVec = posInitial (V.map (fmap fromIntegral) $ figureVertices startFigure) VS.++ VS.replicate numEdges 0
    (newPositions, newMultipliers) = splitParams $ VS.convert solution
    numRotations = 200 :: Int
    numShifts = 20 :: Int
    (roundedPositions, roundedDistances, epsilons, epsilon) =
      minimumBy (comparing $ \(_, _, _, x4) -> x4) $
        [tryShiftRotation (V2 shiftX shiftY) angle | angle <- angles, shiftX <- shifts, shiftY <- shifts]
      where
        angles = [fromIntegral x / fromIntegral numRotations * pi / 2 | x <- [0 .. (numRotations - 1)]]
        shifts = [fromIntegral x / fromIntegral numShifts | x <- [0 .. (numShifts - 1)]]
    tryShiftRotation shift angle = (pos, roundedDists, epss, eps)
      where
        pos = V.map (fmap round) shifted
        shifted = V.map (.+. shift) rotated
        rotated = V.map (\(V2 x y) -> (V2 (cos angle * x - sin angle * y) (sin angle * x + cos angle * y))) newPositions
        roundedDists = V.map (distIdx pos) (figureEdges startFigure)
        eps :: Epsilon
        eps = ceiling $ V.maximum epss * fromIntegral epsilonDenominator
        epss = case epsilonType of
          ForwardEpsilon -> V.zipWith getEpsilon distances roundedDists
          ReverseEpsilon -> V.zipWith (flip getEpsilon) distances roundedDists
        reallyBig = fromIntegral (1000000000000 :: Int)
        getEpsilon refDist finalDistance = if refDist == 0 then reallyBig else
          (case compare finalDistance refDist of
            EQ -> 0
            GT -> finalDistance % refDist - 1
            LT -> negate $ finalDistance % refDist - 1)
    sq x = x * x
    numVertices = V.length $ figureVertices startFigure
    numEdges = V.length $ figureEdges startFigure
    splitParams params = (positions, multipliers)
      where
        multipliers = V.drop (2 * numVertices) params
        xs = V.take numVertices params
        ys = V.take numVertices $ V.drop numVertices params
        positions = V.zipWith V2 xs ys
    distances = V.map (distIdx (figureVertices startFigure)) (figureEdges startFigure)
    finalDistances = V.map (distIdx newPositions) (figureEdges startFigure)
    dist v1 v2 = sq (v2X v1 - v2X v2) + sq (v2Y v1 - v2Y v2)
    distIdx positions (v1Idx, v2Idx) = dist v1 v2
      where
        v1 = positions V.! v1Idx
        v2 = positions V.! v2Idx
