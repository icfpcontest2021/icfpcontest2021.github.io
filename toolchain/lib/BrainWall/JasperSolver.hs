-- | Some ideas:
--
-- * Add neighbours that preserve distances, such as move, flip, rotate
-- * For pinching in an invalid state, we should pick the vertices that are the
--   furthest away with a higher probability than the closer ones
-- * For pinching in a valid state, we should pick the vertices that contribute
--   the most to our score (i.e. the worst ones) with a higher probability
-- * Simulated annealing really needs a contiuous scoring function to work well,
--   and ours is not because there's a big drop in invalid->valid.  Either we
--   should make it contiuous or we should run this in two phases.
{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
module BrainWall.JasperSolver where

import           BrainWall.Box                             (Box)
import qualified BrainWall.Box                             as Box
import           BrainWall.Database                        (allFeatures)
import           BrainWall.Edge
import qualified BrainWall.JasperSolver.HillClimb          as HC
import           BrainWall.JasperSolver.Limbs
import qualified BrainWall.JasperSolver.SimulatedAnnealing as SA
import           BrainWall.Json
import           BrainWall.Polygon
import           BrainWall.Polygon.ContainsPoint
import           BrainWall.Problem
import           BrainWall.Svg
import           BrainWall.V2
import           Control.Lens                              (preview)
import           Control.Monad                             (guard)
import qualified Data.Aeson                                as Aeson
import           Data.Bifunctor                            (first)
import qualified Data.ByteString.Lazy.Char8                as BL8
import qualified Data.IntMap                               as IM
import           Data.List                                 (foldl')
import qualified Data.List                                 as L
import           Data.List.NonEmpty                        (NonEmpty (..))
import qualified Data.List.NonEmpty                        as NonEmpty
import           Data.Maybe                                (fromMaybe,
                                                            maybeToList)
import           Data.Ratio                                (Ratio)
import qualified Data.Text                                 as T
import qualified Data.Vector                               as V
import           Debug.Trace                               (trace)
import qualified Options.Applicative.Extended              as OA
import qualified System.IO                                 as IO
import           System.Random.Extended                    (RandomGen,
                                                            distribution,
                                                            newStdGen, randomR)

-- | A state in the search space.
data State = State
    { -- | Full problem.  TODO: remove this
      stateProblem     :: !Problem
    , -- | Allowed epsilon
      stateEpsilon     :: !Double
    , -- | The hole we're trying to fit in.
      stateHole        :: !(Polygon Integer)
    , -- | Bounding box the hole
      stateHoleBox     :: !(Box Integer)
    , -- | The distance constraints between the vertices.
      stateDistances   :: !(IM.IntMap (IM.IntMap Integer))
    , -- | The limbs found in the figure.
      stateLimbs       :: !Limbs
    , -- | Bonus we are targeting
      stateTargetBonus :: !(Maybe (V2 Integer))
    , -- | The current locations of the vertices.
      stateVertices    :: !(V.Vector (V2 Double))
    } deriving (Show)

makeInitialState :: Maybe (V2 Integer) -> Problem -> State
makeInitialState targetBonus problem@Problem {..} = State {..}
  where
    stateProblem = problem
    stateEpsilon = fromIntegral problemEpsilon / fromIntegral epsilonDenominator
    stateHole = unHole problemHole
    stateTargetBonus = targetBonus

    stateHoleBox = fromMaybe (Box.fromV2 (V2 0 0)) $
        foldMap (Just . Box.fromV2) (unPolygon stateHole)

    stateDistances = foldl' insert IM.empty $ do
        (i, j) <- V.toList $ figureEdges problemFigure
        edge <- maybeToList $ lookupFigureEdge (i, j) problemFigure
        pure (i, j, edgeSquaredLength edge)
      where
        insert m (i, j, d) =
            IM.insertWith IM.union j (IM.singleton i d) .
            IM.insertWith IM.union i (IM.singleton j d) $ m

    stateLimbs = findLimbs problemFigure

    stateVertices = fmap fromIntegral <$> figureVertices problemFigure

-- | A state with more info
data Annotated = Annotated
    { -- | Plain state
      annotatedState          :: !State
    , -- | Indices of vertices which are located outside of the hole
      annotatedOutside        :: !(V.Vector Int)
      -- | Distance to the closest vertex of the hole for each vertex.
    , annotatedDistanceToHole :: !(V.Vector Integer)
    } deriving (Show)

annotateState :: State -> Annotated
annotateState state@State {..} = Annotated {..}
  where
    annotatedState = state
    annotatedOutside = V.map fst $
        V.filter (not . (`pointInPolygon` stateHole) . fmap round . snd) $
        V.indexed stateVertices
    annotatedDistanceToHole = flip V.map stateVertices $ \v ->
        minimum . map (edgeSquaredLength . Edge (round <$> v)) .
        V.toList $ unPolygon stateHole

pinchState :: RandomGen g => Annotated -> g -> (State, g)
pinchState Annotated {..} gen0 =
    let (i, gen1) = distribution weighted gen0
        (phi, gen2) = randomR (0 :: Double, pi * 2) gen1
        (rho, gen3) = randomR (0, maxPinch) gen2
        offset = V2 (rho * cos phi) (rho * sin phi)
        vertices = V.update stateVertices $ V.singleton (i, offset) in
    (annotatedState {stateVertices = vertices}, gen3)
  where
    State {..} = annotatedState
    maxPinch =
        let b = stateHoleBox in
        0.5 * fromInteger (Box.width b + Box.height b)
    candidates
        | V.null annotatedOutside = V.imap const stateVertices
        | otherwise               = annotatedOutside

    weighted = NonEmpty.fromList $ do
        i <- V.toList candidates
        let distanceToHole = annotatedDistanceToHole V.! i
        -- The vertex furthest away is three times as likely to get picked.
        pure (maxDistanceToHole + 2 * distanceToHole, i)

    maxDistanceToHole = V.maximum annotatedDistanceToHole

rotateState :: RandomGen g => Annotated -> g -> (State, g)
rotateState Annotated {..} gen0 =
    let (i, gen1) = randomR (0, V.length stateVertices - 1) gen0
        (d, gen2) = randomR (-pi / 4, pi / 4) gen1
        p = stateVertices V.! i
        rotated = V.imap (\j q ->
            if i == j
                then q
                else
                    let (Polar rho phi) = toPolar (q .-. p) in
                    p .+. fromPolar (Polar rho (phi + d))) stateVertices in
    (annotatedState {stateVertices = rotated}, gen2)
  where
    State {..} = annotatedState

moveState :: RandomGen g => Annotated -> g -> (State, g)
moveState Annotated {..} gen0 =
    let (phi, gen1) = randomR (0 :: Double, pi * 2) gen0
        (rho, gen2) = randomR (0, maxPinch) gen1
        offset = V2 (rho * cos phi) (rho * sin phi)
        vertices = V.map (.+. offset) stateVertices in
    (annotatedState {stateVertices = vertices}, gen2)
  where
    State {..} = annotatedState
    maxPinch =
        let b = stateHoleBox in
        0.1 * fromInteger (Box.width b + Box.height b)

limbState :: RandomGen g => Annotated -> g -> (Maybe State, g)
limbState Annotated {..} gen0 =
    let (i, gen1)   = randomR (0, V.length stateLimbs - 1) gen0
        alts        = alternateLimb (stateLimbs V.! i)
        (j, gen2)   = randomR (0, V.length alts - 1) gen1
        limb        = alts V.! j
        (phi, gen3) = randomR (-pi / 4, pi / 4) gen2 in
    case moveLimb phi limb stateVertices of
        Nothing    -> (Nothing, gen3)
        Just verts -> (Just annotatedState {stateVertices = verts}, gen3)
  where
    State {..} = annotatedState

-- | Move things around very slowly.  This can be used at the end of the run
-- to fine tune the solution and squeeze out some more points.
creepState :: RandomGen g => Annotated -> g -> (State, g)
creepState Annotated {..} gen0 =
    let (i, gen1) = randomR (0, V.length stateVertices - 1) gen0
        (j, gen2) = randomR (0, 3) gen1
        p         = stateVertices V.! i
        p'        =
          [p .+. V2 1 0, p .+. V2 0 1, p .-. V2 1 0, p .-. V2 0 1] !! j
        verts     = V.update stateVertices $ V.singleton (i, p') in
    (annotatedState {stateVertices = verts}, gen2)
  where
    State {..} = annotatedState

data EtherealScore
    = ValidScore Integer
    -- | Number of violations, total squared distance from "outside" points to
    -- nearest edges.
    | InvalidScore (Int, Ratio Integer)
    deriving (Eq, Show)

instance Ord EtherealScore where
    ValidScore   x <= ValidScore   y = y <= x
    ValidScore   _ <= InvalidScore _ = False
    InvalidScore _ <= ValidScore   _ = True
    InvalidScore x <= InvalidScore y = y <= x

stateToSolution :: State -> Solution
stateToSolution state = Solution
    { solutionBonuses  = V.empty
    , solutionVertices = fmap round <$> stateVertices state
    }

scoreState :: Annotated -> EtherealScore
scoreState Annotated {..} = case judgeSolution stateProblem sol of
    Left errs -> InvalidScore (length errs, distances)
    Right s   -> ValidScore $ s + toBonus
  where
    State {..} = annotatedState
    sol = stateToSolution annotatedState
    toBonus = case stateTargetBonus of
        Nothing -> 0
        Just b  -> (^ (2 :: Integer)) $ V.minimum $ do
            v <- solutionVertices sol
            pure $ squaredDistance v b
    distances = L.sum $ do
        i <- annotatedOutside
        let p = solutionVertices sol V.! i
        pure $ V.minimum $ do
            edge <- polygonEdges stateHole
            pure $ edgePointSquaredDistance p edge

rigidifyState :: State -> Maybe State
rigidifyState state0@State {..} = go (0 :: Int) stateVertices
  where
    maxIters  = 2000
    distances = stateDistances
    eps       = stateEpsilon

    go !iter vertices
        | V.all null forces = Just $ state0 {stateVertices = vertices}
        | iter >= maxIters =
            trace ("rigidify: gave up after " ++ show iter ++ " iterations")
            Nothing
        | otherwise = go (iter + 1) $
            V.zipWith (\fs v -> foldl' (.+.) v fs) forces vertices
      where
        forces = flip V.imap vertices $ \i p -> do
            neighbours <- maybeToList $ IM.lookup i distances
            (j, d2) <- IM.toList neighbours

            let q = vertices V.! j
                d'2 = squaredDistance p q :: Double
            guard $ abs (d'2 / fromIntegral d2 - 1) > eps
            let actualDistance = sqrt d'2 :: Double
                expectedDistance = sqrt $ fromIntegral d2
                delta = actualDistance - expectedDistance
            guard $ delta < -1e-9 || delta > 1e-9
            pure $ (q .-. p) .* (0.4 * delta / max 1 actualDistance)

data Command = Pinch | Move | Rotate | MoveLimb deriving (Eq, Show)

stepState :: RandomGen g => Annotated -> g -> (Annotated, g)
stepState ann gen0 =
    let (cmd, gen1) = flip distribution gen0 $
            (((* 3) . V.length . stateLimbs $ annotatedState ann), MoveLimb) :|
            (5 :: Int, Pinch) : (2, Move) : (3, Rotate) : [] in
    case cmd of
        Pinch ->
            let (pinched, gen2) = pinchState ann gen1 in
            case rigidifyState pinched of
                Just rigid -> (annotateState rigid, gen2)
                Nothing    -> (ann, gen1)
        Move ->
            let (moved, gen2) = moveState ann gen1 in
            (annotateState moved, gen2)
        Rotate ->
            let (rotated, gen2) = rotateState ann gen1 in
            case rigidifyState rotated of
                Just rigid -> (annotateState rigid, gen2)
                Nothing    -> (ann, gen1)
        MoveLimb ->
            let (moved, gen2) = limbState ann gen1 in
            case rigidifyState =<< moved of
                Just rigid -> (annotateState rigid, gen2)
                Nothing    -> (ann, gen1)

data Options = Options
    { optionsSaveLimbs   :: Maybe FilePath
    , optionsStartFrom   :: Maybe FilePath
    , optionsTargetBonus :: Maybe (V2 Integer)
    , optionsUseBonus    :: Maybe ClaimedBonus
    , optionsValidOnly   :: Bool
    , optionsProblemPath :: FilePath
    } deriving (Show)

defaultOptions :: Options
defaultOptions = Options
    { optionsSaveLimbs   = Nothing
    , optionsStartFrom   = Nothing
    , optionsTargetBonus = Nothing
    , optionsUseBonus    = Nothing
    , optionsValidOnly   = False
    , optionsProblemPath = ""
    }

solve :: Options -> Problem -> IO Solution
solve options problem = do
    mbStartFrom <- case optionsStartFrom options of
        Nothing -> pure Nothing
        Just fp -> Just <$> decodeFileWith (decodeSolution allFeatures) fp

    let initial = annotateState $
            let s0 = makeInitialState (optionsTargetBonus options) problem in
            case mbStartFrom of
                Nothing -> s0
                Just Solution {..} -> s0
                    {stateVertices = fmap fromIntegral <$> solutionVertices}
        saOptions = SA.defaultOptions
            { SA.oScore     = \x -> case scoreState x of
                ValidScore   p -> negate $ fromIntegral p
                InvalidScore (p, d) ->
                    (-10000000) - fromIntegral p * 100000 - fromRational d
            , SA.oNeighbour = stepState
            , SA.oQuit      = if optionsValidOnly options
                then \x -> case scoreState x of
                    ValidScore   _ -> True
                    InvalidScore _ -> False
                else \_ -> False
            , SA.oGiveUp    = Just 2000
            }


        V2 svgWidth svgHeight =
            fromIntegral <$> Box.bottomRight (problemBox problem)
        limbsSvg =
            let figureElement = edgesToSvgElement . map (fmap fromIntegral) .
                    foldFigureEdges pure $ problemFigure problem
                verts = stateVertices $ annotatedState initial
                limbsElement = edgesToSvgElement $ do
                    Limb (i, j) (k, l) _ _ <- V.toList . stateLimbs $
                        annotatedState initial
                    pure $ Edge
                        ((verts V.! i) .* 0.5 .+. (verts V.! j) .* 0.5)
                        ((verts V.! k) .* 0.5 .+. (verts V.! l) .* 0.5)
                movedElement = edgesToSvgElement $ do
                    limb <- take 1 . V.toList . stateLimbs $
                        annotatedState initial
                    verts' <- maybeToList $ moveLimb (-pi / 16) limb verts
                    (i, j) <- V.toList . figureEdges $ problemFigure problem
                    pure $ Edge (verts' V.! i) (verts' V.! j) in
            (makeSvg (0, 0, svgWidth, svgHeight))
                { svgElements =
                    [ figureElement {elementStyle = Just "stroke:red"}
                    , limbsElement {elementStyle = Just "stroke:green"}
                    , movedElement
                        {elementStyle = Just "stroke:orange;stroke-width:0.3"}
                    ]
                }

    case optionsSaveLimbs options of
        Nothing   -> pure ()
        Just path -> writeFile path $ encodeSvg limbsSvg

    gen0 <- newStdGen
    let (!saResult, gen1) = SA.run saOptions initial gen0

    hcResult <- case scoreState saResult of
        InvalidScore _ -> pure saResult
        ValidScore _ | optionsValidOnly options -> pure saResult
        ValidScore saScore -> do

            IO.hPutStrLn IO.stderr $ "Starting hill climb with score " <>
                show saScore

            let hcScore = \x -> case scoreState x of
                    ValidScore   p -> negate p
                    InvalidScore _ -> negate saScore - 1
                hcOpts = HC.defaultOptions
                    { HC.oScore = hcScore
                    , HC.oNeighbour = \x -> first annotateState . creepState x
                    }

                !(!hcResult, _gen2) = HC.hillClimb hcOpts saResult gen1
            -- pure $ fromEthereal $ hillclimb 0 (score problem initial) initial gen0

            IO.hPutStrLn IO.stderr $ "Finished hill climb with score " <>
                show (hcScore hcResult)
            pure hcResult

    let solution = stateToSolution $ annotatedState hcResult

    pure solution
        { solutionBonuses = maybe mempty V.singleton $ optionsUseBonus options
        }

parseOptions :: OA.Parser Options
parseOptions = Options
    <$> OA.optional (OA.strOption $
            OA.long "limbs" <>
            OA.metavar "LIMBS.svg")
    <*> OA.optional (OA.strOption $
            OA.long "start-from" <>
            OA.metavar "SOLUTION.json")
    <*> OA.optional (OA.option (OA.maybeReader $ preview v2FromString) $
            OA.long "target-bonus" <>
            OA.metavar "X,Y")
    <*> OA.optional (ClaimedBonus
            <$> OA.option OA.auto (
                    OA.long "use-bonus-problem" <>
                    OA.metavar "PROBLEM")
            <*> OA.option bonusReader (
                    OA.long "use-bonus" <>
                    OA.metavar "BONUS")
            <*> pure Nothing)
    <*> OA.switch (
            OA.long "valid" <>
            OA.help "exit as soon as a valid solution is found")
    <*> OA.strArgument (OA.metavar "PROBLEM")
  where
    bonusReader = OA.maybeReader $ preview bonusTypeFromText . T.pack

main :: IO ()
main = do
    options <- OA.simpleRunParser parseOptions
    problem <- decodeFileWith (decodeProblem allFeatures)
        (optionsProblemPath options)
    solution <- solve options problem
    BL8.putStrLn . Aeson.encode $ encodeSolution allFeatures solution
