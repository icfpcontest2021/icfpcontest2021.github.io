{-# LANGUAGE RecordWildCards #-}
module BrainWall.Main.ParseSvg
    ( main
    ) where

import           BrainWall.Database             (allFeatures)
import           BrainWall.Json
import           BrainWall.Main.EstimateEpsilon (estimateByCoverage)
import           BrainWall.Problem
import           BrainWall.Svg
import           Data.Aeson.Encode.Pretty       (encodePretty)
import qualified Data.ByteString.Lazy.Char8     as BL8
import qualified Options.Applicative.Extended   as OA

data Options = Options
    { optsCoverage :: !Double
    , optsSvg      :: !FilePath
    } deriving (Show)

parseOptions :: OA.Parser Options
parseOptions = Options
    <$> (OA.option OA.auto (OA.long "coverage" <> OA.value 0.3))
    <*> OA.strArgument (OA.metavar "SVG")

main :: IO ()
main = do
    Options {..} <- OA.simpleRunParser parseOptions
    svg <- readSvg optsSvg
    problem <- case problemFromSvg svg of
        Left err      -> fail $ "Couldn't construct problem: " ++ err
        Right problem -> pure problem
    let epsilon = estimateByCoverage optsCoverage $ problemFigure problem
        problem' = case epsilon of
            Nothing  -> problem
            Just eps -> problem {problemEpsilon = fromIntegral eps}
    BL8.putStrLn . encodePretty $
        encodeProblem allFeatures problem'
