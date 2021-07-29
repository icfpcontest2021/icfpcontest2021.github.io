module BrainWall.Main.RenderSvg
    ( main
    ) where

import           BrainWall.Database  (allFeatures)
import           BrainWall.Json
import           BrainWall.Svg
import           Control.Monad       (guard)
import qualified Options.Applicative.Extended as OA

data Options = Options
    { optionsMargin       :: Integer
    , optionsFillHole     :: Bool
    , optionsOutPath      :: Maybe FilePath
    , optionsProblemPath  :: FilePath
    , optionsSolutionPath :: Maybe FilePath
    }

parseOptions :: OA.Parser Options
parseOptions = Options
    <$> OA.option OA.auto (
            OA.long "margin" <>
            OA.value (ptssMargin defaultProblemToSvgSettings))
    <*> OA.switch (OA.long "fill-hole")
    <*> OA.optional (OA.strOption (OA.short 'o' <> OA.metavar "OUT.svg"))
    <*> OA.strArgument (OA.metavar "PROBLEM")
    <*> OA.optional (OA.strArgument $ OA.metavar "SOLUTION")

main :: IO ()
main = do
    options <- OA.simpleRunParser parseOptions
    problem <- decodeFileWith
        (decodeProblem allFeatures) (optionsProblemPath options)
    mbSolution <- case optionsSolutionPath options of
        Nothing -> pure Nothing
        Just solutionPath -> Just <$>
            decodeFileWith (decodeSolution allFeatures) solutionPath
    let out = encodeSvg $ problemToSvgWith defaultProblemToSvgSettings
            { ptssFillHole = "#444444" <$ guard (optionsFillHole options)
            , ptssSolution = mbSolution
            , ptssMargin   = optionsMargin options
            }
            problem
    case optionsOutPath options of
        Nothing -> putStr out
        Just path -> writeFile path out
