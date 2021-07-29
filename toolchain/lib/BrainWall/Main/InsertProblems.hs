{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module BrainWall.Main.InsertProblems
    ( iterateProblems
    , problemIdFromFilePath
    , solutionIdFromFilePath
    , main
    ) where

import qualified BrainWall.Database           as Db
import           BrainWall.Json
import qualified Data.Aeson.Text              as Aeson
import qualified Data.Text.Lazy               as TL
import           Data.Traversable             (for)
import qualified Options.Applicative.Extended as OA
import           System.Directory             (doesDirectoryExist,
                                               listDirectory)
import           System.FilePath              (splitExtension, takeFileName,
                                               (</>))
import           Text.Read                    (readMaybe)

problemIdFromFilePath :: FilePath -> Maybe Int
problemIdFromFilePath path = case splitExtension $ takeFileName path of
    (str, ".problem") -> readMaybe str
    _                 -> Nothing

solutionIdFromFilePath :: FilePath -> Maybe Int
solutionIdFromFilePath path = case splitExtension $ takeFileName path of
    (str, ".solution") -> readMaybe str
    _                 -> Nothing

iterateProblems :: FilePath -> IO [FilePath]
iterateProblems path = do
    isDir <- doesDirectoryExist path
    if isDir then do
        entries <- listDirectory path
        fmap concat . for entries $ \entry -> iterateProblems $ path </> entry
    else
        pure . maybe [] (\_ -> [path]) $ problemIdFromFilePath path

data Options = Options
    { optsEnable    :: !Bool
    , optsOverwrite :: !Bool
    , optsPaths     :: ![FilePath]
    } deriving (Show)

parseOptions :: OA.Parser Options
parseOptions = Options
    <$> OA.switch (
            OA.short 'e' <>
            OA.long "enable" <>
            OA.help "Immediately enable problems")
    <*> OA.switch (OA.short 'f' <> OA.help "Overwrite existing problems")
    <*> OA.some (OA.strArgument $ OA.metavar "PATH")

main :: IO ()
main = do
    options <- OA.simpleRunParser parseOptions
    problemFileNames <- concat <$> for (optsPaths options) iterateProblems
    problems <- for problemFileNames $ \path ->
        case problemIdFromFilePath path of
            Just n -> do
                problem <- decodeFileWith (decodeProblem Db.allFeatures) path
                let problemId = Db.ProblemId n
                    problemBody = TL.toStrict . Aeson.encodeToLazyText $
                        encodeProblem Db.allFeatures problem
                    problemEnabled = optsEnable options
                pure Db.Problem {..}

            _ -> fail $
                "Bad problem filename: expected N.problem, got " ++ show path

    dbConfig <- Db.configFromEnv
    Db.withDatabase dbConfig $ \conn -> do
        if optsOverwrite options then
            Db.upsertProblems conn problems
        else
            Db.insertProblems conn problems
