-- |
module BrainWall.Main.RandomProblem
  ( main,
    allGenerationTypes,
    showGenType,
    generateInDirectory,
  )
where

import qualified BrainWall.Box as Box
import BrainWall.Database (allFeatures)
import BrainWall.Edge
import BrainWall.Json
import BrainWall.Planar
import BrainWall.Polygon
import BrainWall.Polygon.ConvexHull
import BrainWall.Polygon.Simple
import BrainWall.Problem
import BrainWall.Random
import qualified BrainWall.Main.GenerateHole as GenHole
import BrainWall.Svg
import BrainWall.V2
import Control.Monad
import Control.Monad.Except
import qualified Data.Aeson.Text as Aeson
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Ratio
import qualified Data.Text.Lazy.IO as Text
import qualified Data.Vector as V
import Debug.Trace
import qualified Options.Applicative as OA
import qualified System.IO as IO
import System.Directory
import System.FilePath
import System.Random.MWC as MWC

data Options = Options
  { optionsGenerationType :: GenerationType,
    outDirectory :: FilePath
  }

parseOptions :: OA.Parser Options
parseOptions =
  Options
    <$> ((allGenerationTypes V.!) <$> OA.option OA.auto (OA.short 't' <> OA.long "type" <> OA.metavar "INT"))
    <*> OA.strArgument (OA.metavar "DIRNAME" <> OA.help "Name of directory to put files. Will be created if it doesn't exist")

parseOptionsInfo :: OA.ParserInfo Options
parseOptionsInfo = OA.info (OA.helper <*> parseOptions) $ OA.fullDesc

parseOptionsPrefs :: OA.ParserPrefs
parseOptionsPrefs = OA.prefs OA.showHelpOnError

rezeroFigure :: Figure -> Figure
rezeroFigure figure = Figure (V.map (.-. V2 minX minY) $ figureVertices figure) (figureEdges figure)
  where
    minX = V.minimum $ V.map v2X $ figureVertices figure
    minY = V.minimum $ V.map v2Y $ figureVertices figure

data ConvexifyHole
  = ConvexifyHole
  | NoConvexifyHole
  deriving (Enum, Bounded, Eq, Show)

data ConvexifyFigure
  = ConvexifyFigure
  | NoConvexifyFigure
  deriving (Enum, Bounded, Eq, Show)

figureToProblem :: ConvexifyHole -> Figure -> Figure -> Epsilon -> Either GenError (Problem, Problem)
figureToProblem convexify startFigure endFigure epsilon = do
  traceShowM ("factored", factored)
  traceShowM ("factored2", figure4)
  traceShowM ("vertices", filteredVertices)
  hole2 <- case convexify of
      NoConvexifyHole -> do
        case simplifyPolygon polygon of
          Nothing -> Left FailedOutline
          Just hole -> Right $ Hole hole
        where
          polygon = fromMaybe (error $ "no polygon? " ++ show filteredVertices) $ mkPolygon $ V.fromList filteredVertices
      ConvexifyHole -> Right $ Hole $ convexHull $ V.fromList filteredVertices
  traceShowM ("hole2", hole2)
  let problem = Problem hole2 startFigure epsilon V.empty
      solvedProblem = Problem hole2 endFigure epsilon V.empty
  pure (problem, solvedProblem)
  where
    figure3Edges = map (fmap fromIntegral) $ foldFigureEdges (: []) endFigure
    factored :: [Edge Rational]
    factored = factorAllEdges figure3Edges
    figure4 = figureFromEdges factored
    planar = figureGraph figure4
    outlined = fromMaybe (error $ "no outline: " ++ show planar) $ outline planar
    vertices = map (figureVertices figure4 V.!) (NonEmpty.toList outlined)
    filteredVertices = map (fmap round) $ filter (\(V2 x y) -> denominator x == 1 && denominator y == 1) vertices


data HoleAroundHole
  = HoleAroundHole
  | NoHoleAroundHole
  deriving (Enum, Bounded, Eq, Show)

data GenerationType = GenerationType !ConvexifyHole !HoleAroundHole !ObjectiveType !EpsilonType !ConvexifyFigure !FigureSource !InitialPositionType

showGenType :: GenerationType -> String
showGenType (GenerationType a b c d e f g) = intercalate "-" [show a, show b, show c, show d, show e, show f, show g]

allGenerationTypes :: V.Vector GenerationType
allGenerationTypes = V.fromList $ GenerationType
  <$> [minBound..maxBound]
  <*> [minBound..maxBound]
  <*> [minBound..maxBound]
  <*> [minBound..maxBound]
  <*> [minBound..maxBound]
  <*> [minBound..maxBound]
  <*> [minBound..maxBound]

data GenError
  = FailedOutline
  | EpsilonTooBig
  deriving (Show)

acceptableEpsilon :: Epsilon
acceptableEpsilon = 250000

generate ::  GenerationType -> GenIO -> ExceptT GenError IO (Problem, Problem, Solution)
generate (GenerationType convexifyHole holeAroundHole objectiveType epsilonType convexifyFigure figureSource initialPositionType) gen = do
  figure <- lift $ case convexifyFigure of
    NoConvexifyFigure -> randomGenerateFigure figureSource gen
    ConvexifyFigure -> do
      preFigure <- randomGenerateFigure figureSource gen
      let vertices = figureVertices preFigure
      let hull = polygonEdges $ convexHull vertices
      let edges = V.toList hull ++ foldFigureEdges (:[]) preFigure
      pure $ figureFromEdges edges
  (mutated, epsilon) <- randomMutateFigure initialPositionType epsilonType objectiveType figure (100, 100) gen

  when (epsilon > acceptableEpsilon) $ throwError EpsilonTooBig

  let m'problem = case epsilonType of
        ForwardEpsilon -> figureToProblem convexifyHole (rezeroFigure figure) (rezeroFigure mutated) epsilon
        ReverseEpsilon -> figureToProblem convexifyHole (rezeroFigure mutated) (rezeroFigure figure) epsilon

  case m'problem of
    Right (problem, solvedProblem) -> do
      let hole1 = problemHole problem
      hole2 <- case holeAroundHole of
        NoHoleAroundHole -> pure hole1
        HoleAroundHole -> do
          let box = problemBox problem
          poly <- GenHole.generateHole
              GenHole.defaultOptions
                  { GenHole.optionsWidth      = Box.width box
                  , GenHole.optionsHeight     = Box.height box
                  , GenHole.optionsAroundHole = Just hole1
                  }
              gen
          lift $ print $ "poly: " <> show poly
          pure . fromMaybe hole1 $ GenHole.polyToHole poly
      let problem2 = positivizeProblem  $ problem {problemHole = hole2}
          solvedProblem2 = positivizeProblem $ solvedProblem {problemHole = hole2}
          solution = Solution V.empty $ figureVertices $ problemFigure solvedProblem2
      pure (problem2, solvedProblem2, solution)
    Left err -> throwError err

generateInDirectory :: FilePath -> GenerationType -> GenIO -> IO ()
generateInDirectory dirPath generationType gen = do
  m'problem <- runExceptT $ generate generationType gen
  case m'problem of
    Left err ->
      putStrLn $ "FAILED: " ++ show err
    Right (problem, solvedProblem, solution) -> do
      case judgeSolution problem solution of
        Left errors -> error $ show errors
        Right score -> do
          traceShowM ("score", score)
      Text.writeFile (dirPath </> "problem.problem") $ Aeson.encodeToLazyText $ encodeProblem allFeatures problem
      IO.writeFile (dirPath </> "problem.svg") $ encodeSvg $ problemToSvg problem
      IO.writeFile (dirPath </> "solved.svg") $ encodeSvg $ problemToSvg solvedProblem
      Text.writeFile (dirPath </> "problem.solution") $ Aeson.encodeToLazyText $ encodeSolution allFeatures solution
      case judgeSolution problem solution of
        Left errors -> error $ show errors
        Right score -> do
          traceShowM ("score", score)

main :: IO ()
main = do
  options <- OA.customExecParser parseOptionsPrefs parseOptionsInfo
  let dirPath = outDirectory options
  createDirectory dirPath

  -- gen <- MWC.create
  gen <- MWC.createSystemRandom
  generateInDirectory dirPath (optionsGenerationType options) gen
