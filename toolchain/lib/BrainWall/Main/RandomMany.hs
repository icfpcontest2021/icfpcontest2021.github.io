-- |
{-# LANGUAGE ScopedTypeVariables #-}

module BrainWall.Main.RandomMany where

import qualified BrainWall.Main.RandomProblem as RP
import BrainWall.Random
import Control.Monad
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Data.Vector as V
import Data.Word
import qualified Options.Applicative as OA
import System.Directory
import System.FilePath
import qualified System.IO as IO
import System.Random.MWC as MWC

data Options = Options
  { outDirectory :: FilePath
  , seedFile :: Maybe FilePath
  }

parseOptions :: OA.Parser Options
parseOptions =
  Options
    <$> OA.strArgument (OA.metavar "DIRNAME" <> OA.help "Name of directory to put files. Will be created if it doesn't exist")
    <*> OA.optional (OA.strOption (OA.long "seed" <> OA.metavar "SEEDFILE"))

parseOptionsInfo :: OA.ParserInfo Options
parseOptionsInfo = OA.info (OA.helper <*> parseOptions) $ OA.fullDesc

parseOptionsPrefs :: OA.ParserPrefs
parseOptionsPrefs = OA.prefs OA.showHelpOnError

main :: IO ()
main = do
  options <- OA.customExecParser parseOptionsPrefs parseOptionsInfo
  let dirPath = outDirectory options
  gen <- case seedFile options of
    Nothing -> MWC.createSystemRandom
    Just file -> do
      (seed :: V.Vector Word32) <- read <$> readFile file
      MWC.initialize seed
  forever $ do
    seed <- MWC.save gen
    putStrLn $ "Seed: " ++ show seed
    uuid <- UUID.nextRandom
    generationType <- chooseFrom RP.allGenerationTypes gen
    let thisDir = dirPath </> RP.showGenType generationType </> UUID.toString uuid
    createDirectoryIfMissing True thisDir
    IO.writeFile (thisDir </> "seed") $ show $ fromSeed seed
    putStrLn $ "Generating " ++ thisDir
    RP.generateInDirectory thisDir generationType gen
