module BrainWall.Main.ResetPassword
    ( main
    ) where

import qualified BrainWall.Database           as Db
import qualified Data.Text                    as T
import qualified Options.Applicative.Extended as OA

data Options = Options
    { optionsEmail       :: T.Text
    , optionsNewPassword :: T.Text
    } deriving (Show)

parseOptions :: OA.Parser Options
parseOptions = Options
    <$> (T.pack <$> OA.strArgument (OA.metavar "EMAIL"))
    <*> (T.pack <$> OA.strArgument (OA.metavar "PASSWORD"))

main :: IO ()
main = do
    options <- OA.simpleRunParser parseOptions
    dbConfig <- Db.configFromEnv
    Db.withDatabase dbConfig $ \conn -> Db.unsafeResetPassword conn
        (optionsEmail options) (optionsNewPassword options)
