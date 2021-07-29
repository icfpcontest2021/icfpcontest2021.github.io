module BrainWall.Main.DownloadSources
    ( main
    ) where

import qualified BrainWall.Database           as Db
import qualified Data.ByteString              as B
import           Data.Char                    (isAlphaNum)
import           Data.Foldable                (for_)
import qualified Data.Text                    as T
import           Data.Time                    (UTCTime)
import qualified Options.Applicative.Extended as OA
import           System.FilePath              ((</>))
import qualified System.IO                    as IO

data Options = Options
    { optionsBefore :: UTCTime
    , optionsOut    :: Maybe FilePath
    } deriving (Show)

parseOptions :: OA.Parser Options
parseOptions = Options
    <$> OA.timeOption (OA.long "before")
    <*> OA.optional (OA.strOption (OA.short 'o' <> OA.metavar "DIR"))

teamNameToArchiveName :: T.Text -> FilePath
teamNameToArchiveName name =
    clean (T.unpack $ T.map safe name) <> ".tar.gz"
  where
    safe c = if isAlphaNum c then c else '_'
    clean ('_' : '_' : xs) = clean ('_' : xs)
    clean (x : xs)         = x : clean xs
    clean []               = []

main :: IO ()
main = do
    options <- OA.simpleRunParser parseOptions
    dbConfig <- Db.configFromEnv
    Db.withDatabase dbConfig $ \db -> do
        teams <- Db.getTeams db
        for_ teams $ \team -> do
            let archiveName = teamNameToArchiveName $ Db.teamName team
                path = case optionsOut options of
                    Just dir -> dir </> archiveName
                    Nothing  -> archiveName
            mbArchive <- Db.getSourceCodeArchive db (Db.teamId team)
                (optionsBefore options)
            case mbArchive of
                Nothing -> pure ()
                Just bs -> do
                    B.writeFile path bs
                    IO.hPutStrLn IO.stderr $ path <> " OK"
