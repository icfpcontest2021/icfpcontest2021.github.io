-- | Uses JasperSolver to check if puzzles are "solvable".
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module BrainWall.Main.JasperGen
    ( main
    ) where

import qualified BrainWall.Box                  as Box
import           BrainWall.Database             (allFeatures)
import qualified BrainWall.JasperSolver         as Solver
import           BrainWall.Json
import           BrainWall.Main.EstimateEpsilon (estimateByCoverage)
import qualified BrainWall.Main.GenerateFigure  as GenFig
import qualified BrainWall.Main.GenerateHole    as GenHole
import           BrainWall.Problem
import           BrainWall.Svg
import           BrainWall.V2
import qualified Data.Text.IO                   as T
import qualified Options.Applicative            as OA
import           System.Random.MWC              as MWC

data Options = Options
    { optionsMaxEps   :: Integer
    , optionsMargin   :: Integer
    , optionsSvg      :: Maybe FilePath
    , optionsSolution :: Maybe FilePath
    , optionsFigure   :: Maybe FilePath
    }

parseOptions :: OA.Parser Options
parseOptions = Options
    <$> OA.option OA.auto (OA.long "max-epsilon" <> OA.value 250000)
    <*> OA.option OA.auto (OA.long "margin" <> OA.value 10)
    <*> OA.optional (OA.strOption (
            OA.long "svg" <> OA.metavar "PROBLEM.svg"))
    <*> OA.optional (OA.strOption (
            OA.long "solution" <> OA.metavar "SOLUTION.json"))
    <*> OA.optional (OA.strOption (
            OA.long "figure" <> OA.metavar "PROBLEM.json"))

parseOptionsInfo :: OA.ParserInfo Options
parseOptionsInfo = OA.info (OA.helper <*> parseOptions) $ OA.fullDesc

parseOptionsPrefs :: OA.ParserPrefs
parseOptionsPrefs = OA.prefs OA.showHelpOnError

main :: IO ()
main = do
    options <- OA.customExecParser parseOptionsPrefs parseOptionsInfo

    gen <- MWC.createSystemRandom
    blueprint <- case optionsFigure options of
        Nothing -> GenFig.buildFigureToProblem <$> GenFig.generateFigure
            GenFig.defaultOptions
                { GenFig.optionsReconnectChance = 0.2
                , GenFig.optionsMaxBridges      = 5
                }
            gen
        Just problemPath ->
            decodeFileWith (decodeProblem allFeatures) problemPath

    let epsilon = min (optionsMaxEps options) $
            case estimateByCoverage 0.6 (problemFigure blueprint) of
                Just eps -> fromIntegral eps
                Nothing  -> problemEpsilon blueprint

    spacingFactor <- uniformRM (0.5, 1) gen

    hole <- do
        let V2 w h = Box.bottomRight $ problemBox blueprint
        poly <- GenHole.generateHole GenHole.defaultOptions
            { GenHole.optionsWidth   = w + optionsMargin options * 2
            , GenHole.optionsHeight  = h + optionsMargin options * 2
            , GenHole.optionsSpacing = spacingFactor *
                GenHole.optionsSpacing GenHole.defaultOptions
            }
            gen
        maybe (fail "invalid polygon generated") pure $
            GenHole.polyToHole poly

    let offset = V2 (optionsMargin options) (optionsMargin options)
        problem = blueprint
            { problemHole = hole
            , problemEpsilon = epsilon
            , problemFigure =
                let fig = problemFigure blueprint
                    verts = figureVertices fig in
                fig {figureVertices = (.+. offset) <$> verts}
            }

    case optionsSvg options of
        Nothing   -> pure ()
        Just path -> writeFile path $ encodeSvg $ problemToSvg problem

    solution <- Solver.solve
        Solver.defaultOptions {Solver.optionsValidOnly = True}
        problem

    case judgeSolution problem solution of
        Left _  -> fail "no solution found"
        Right _ -> do
            case optionsSolution options of
                Nothing -> pure ()
                Just path -> T.writeFile path . encodeText $
                    encodeSolution allFeatures solution

            T.putStrLn . encodeText $ encodeProblem allFeatures problem
