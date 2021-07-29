{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module BrainWall.Database
    ( Config (..)
    , configFromEnv

    , Database (..)
    , openDatabase
    , closeDatabase
    , withDatabase
    , initialize

    , Error (..)

    , CachedFeatures (..)
    , Features (..)
    , allFeatures
    , featuresAnyBonuses
    , getFeatures

    , TeamId (..)
    , Team (..)
    , createTeam
    , getTeam
    , getTeamByLoginId
    , getTeamByApiKey
    , getTeams
    , UpdateTeam (..)
    , updateTeam
    , unsafeResetPassword

    , ProblemId (..)
    , Problem (..)
    , insertProblems
    , upsertProblems
    , getEnabledProblem
    , unsafeGetProblem
    , getEnabledProblems
    , getEnabledProblemIds
    , ProblemSummary (..)
    , getEnabledProblemSummaries
    , getAvailableBonusesForProblem

    , SolutionId (..)
    , Solution (..)
    , SolutionResult (..)
    , solutionResult

    , SubmissionRefused (..)
    , submitSolution
    , getSolutionById
    , getSolutionByProblemId
    , getSolutionsByProblemBefore
    , getSolutionsByTeam
    , unsafeGetBestSolutionsByProblemId
    , SolutionSummary (..)
    , getSolutionSummaries

    , JudgementId (..)
    , JudgementStatus (..)
    , judgementStatusFromText
    , popAndStartJudgement
    , startJudgement
    , completeJudgement
    , updateJudgementsBonusOk

    , setScoreboard
    , getPublicScoreboard
    , getLightningScoreboard

    , submitSourceCode
    , getSourceCodeInfo
    , getSourceCodeArchive
    ) where

import           Control.Applicative                  ((<|>))
import           Control.DeepSeq                      (NFData)
import           Control.Exception                    (Exception, bracket,
                                                       throwIO, try)
import           Control.Lens                         (Prism', prism', review,
                                                       (^?))
import           Control.Monad                        (guard, unless, void)
import qualified Crypto.Hash.SHA256                   as Sha256
import qualified Data.Aeson                           as Aeson
import           Data.Bifunctor                       (bimap)
import qualified Data.ByteString                      as B
import qualified Data.ByteString.Base16               as Base16
import qualified Data.ByteString.Char8                as BC8
import           Data.Hashable                        (Hashable)
import qualified Data.HashMap.Strict                  as HMS
import qualified Data.HashSet                         as HS
import           Data.Int                             (Int64)
import           Data.IORef                           (IORef)
import qualified Data.IORef                           as IORef
import qualified Data.List                            as List
import           Data.Maybe                           (fromMaybe, listToMaybe)
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as T
import qualified Data.Time                            as Time
import           Data.Traversable                     (for)
import           Data.UUID                            (UUID)
import qualified Data.UUID                            as Uuid
import qualified Database.PostgreSQL.Simple           as Pg
import qualified Database.PostgreSQL.Simple
import qualified Database.PostgreSQL.Simple.FromField as Pg
import qualified Database.PostgreSQL.Simple.ToField   as Pg
import           GHC.Generics                         (Generic)
import           System.Environment                   (lookupEnv)
import           System.Log.FastLogger                as Log
import qualified Text.Blaze.Html5                     as H
import           Text.Read                            (readMaybe)
import qualified Web.Internal.HttpApiData             as Http
import           Web.Users.Postgresql                 ()
import qualified Web.Users.Types                      as Users

data Config = Config
    { configConnectionString     :: !BC8.ByteString
    , configCacheFeaturesSeconds :: !Int
    }

configFromEnv :: IO Config
configFromEnv = do
    configConnectionString <- maybe "dbname=icfpc" BC8.pack <$>
        lookupEnv "BRAINWALL_PG_CONNECTION_STRING"
    configCacheFeaturesSeconds <- (fromMaybe 5 . (>>= readMaybe)) <$>
        lookupEnv "BRAINWALL_DB_CACHE_FEATURES_SECONDS"
    pure Config {..}

-- | We request the features A LOT so it is worth caching.
data CachedFeatures
    = NoCachedFeatures
    | CachedFeatures !Time.UTCTime !Features

data Database = Database
    { dbConfig     :: !Config
    , dbConnection :: !Pg.Connection
    , dbFeatures   :: !(IORef CachedFeatures)
    }

openDatabase :: Config -> IO Database
openDatabase dbConfig@Config {..} = do
    dbConnection <- Pg.connectPostgreSQL configConnectionString
    dbFeatures   <- IORef.newIORef NoCachedFeatures
    pure Database {..}

closeDatabase :: Database -> IO ()
closeDatabase = Pg.close . dbConnection

-- | Mostly for GHCi, use a connection pool in the web app.
withDatabase :: Config -> (Database -> IO a) -> IO a
withDatabase cfg = bracket (openDatabase cfg) closeDatabase

initialize :: Database -> IO ()
initialize Database {..} = Users.initUserBackend dbConnection

data Error = Error String

instance Exception Error
instance Show Error where show (Error msg) = msg

data Features = Features
    { featuresEnabledProblems       :: !(ProblemId -> Bool)
    , featuresBonusSuperFlexEnabled :: !Bool
    , featuresBonusGlobalistEnabled :: !Bool
    , featuresBonusBreakALegEnabled :: !Bool
    , featuresBonusWallhackEnabled  :: !Bool
    , featuresSubmissionRateSeconds :: !(Maybe Int)
    , featuresSubmissionLimitKb     :: !Int
    , featuresScoreboardFrozen      :: !Bool
    , featuresSubmissionEnded       :: !Bool
    , featuresLightningEnded        :: !Bool
    , featuresWebPassword           :: !(Maybe T.Text)
    , featuresSourceCodeLimitKb     :: !Int
    }

allFeatures :: Features
allFeatures = Features
    { featuresEnabledProblems       = \_ -> True
    , featuresBonusSuperFlexEnabled = True
    , featuresBonusGlobalistEnabled = True
    , featuresBonusBreakALegEnabled = True
    , featuresBonusWallhackEnabled  = True
    , featuresSubmissionRateSeconds = Nothing
    , featuresSubmissionLimitKb     = 64
    , featuresScoreboardFrozen      = False
    , featuresSubmissionEnded       = False
    , featuresLightningEnded        = False
    , featuresWebPassword           = Nothing
    , featuresSourceCodeLimitKb     = 8192
    }

featuresAnyBonuses :: Features -> Bool
featuresAnyBonuses Features {..} =
    featuresBonusSuperFlexEnabled ||
    featuresBonusGlobalistEnabled ||
    featuresBonusBreakALegEnabled ||
    featuresBonusWallhackEnabled

getFeatures :: Database -> IO Features
getFeatures db@Database {..} = do
    now <- Time.getCurrentTime
    cached <- IORef.readIORef dbFeatures
    case cached of
        CachedFeatures old features | Time.addUTCTime expiry old > now ->
            pure features
        _ -> do
            features <- get now
            unless (expiry <= 0) $
                IORef.writeIORef dbFeatures $ CachedFeatures now features
            pure features
  where
    expiry = fromIntegral $ configCacheFeaturesSeconds dbConfig
    get now = do
        features <- Pg.query_ dbConnection
            "SELECT bonus_superflex_enable_at, bonus_globalist_enable_at, \
            \    bonus_breakaleg_enable_at, bonus_wallhack_enable_at, \
            \    submission_rate_seconds, submission_limit_kb, \
            \    scoreboard_freeze_at, submission_end_at, \
            \    lightning_end_at, web_password, \
            \    source_code_limit_kb \
            \FROM features"
        enabledProblemsSet <- HS.fromList <$> getEnabledProblemIds db
        case features of
            (bonusSuperFlexEnableAt, bonusGlobalistEnableAt,
                    bonusBeakALegEnableAt, bonusWallhackEnableAt,
                    featuresSubmissionRateSeconds, featuresSubmissionLimitKb,
                    scoreboardFreezeAt, submissionEndAt,
                    lightningEndAt, featuresWebPassword,
                    featuresSourceCodeLimitKb) : _ -> do
                let featuresBonusSuperFlexEnabled = maybe False (now >) bonusSuperFlexEnableAt
                    featuresBonusGlobalistEnabled = maybe False (now >) bonusGlobalistEnableAt
                    featuresBonusBreakALegEnabled = maybe False (now >) bonusBeakALegEnableAt
                    featuresBonusWallhackEnabled  = maybe False (now >) bonusWallhackEnableAt
                    featuresScoreboardFrozen      = maybe False (now >) scoreboardFreezeAt
                    featuresSubmissionEnded       = maybe False (now >) submissionEndAt
                    featuresLightningEnded        = maybe False (now >) lightningEndAt
                    featuresEnabledProblems       = (`HS.member` enabledProblemsSet)
                pure Features {..}
            _   -> throwIO . Error $ "features is not a singleton"

newtype TeamId = TeamId UUID
    deriving ( Eq, H.ToMarkup, H.ToValue, Pg.FromField, Pg.ToField
             , Http.FromHttpApiData, Hashable, Ord
             )

instance Show TeamId where show (TeamId tid) = Uuid.toString tid

data Team = Team
    { teamId      :: !TeamId
    , teamName    :: !T.Text
    , teamEmail   :: !T.Text
    , teamMembers :: !(Maybe T.Text)
    , teamRegion  :: !(Maybe T.Text)
    , teamApiKey  :: !UUID
    } deriving (Generic)
    -- NOTE: Don't derive Show we don't accidentally show the API key.

instance Pg.FromRow Team

createTeam :: Database -> Users.User -> IO (Either Users.CreateUserError TeamId)
createTeam Database {..} user = Pg.withTransaction dbConnection $ do
    errOrId <- Users.createUser dbConnection user
    case errOrId of
        Left err -> pure $ Left err
        Right userId -> do
            tids <- Pg.query dbConnection
                "INSERT INTO teams (login_id) VALUES (?) RETURNING id" [userId]
            case tids of
                Pg.Only tid : _ -> pure $ Right tid
                _               -> throwIO . Error $ "Could not create team"

getTeam :: Database -> TeamId -> IO Team
getTeam Database {..} tid = do
    teams <- Pg.query dbConnection
        "SELECT teams.id, username, email, members, region, api_key \
        \FROM login INNER JOIN teams \
        \ON login_id = lid \
        \WHERE teams.id = ?"
         [tid]
    case teams of
        t : _ -> pure t
        _     -> throwIO . Error $ "Team not found: " ++ show tid

getTeamByLoginId :: Database -> Int64 -> IO Team
getTeamByLoginId db@Database {..} loginId = do
    -- NOTE: We can optimize this to do one query rather than two.
    tids <- Pg.query dbConnection
        "SELECT id FROM teams WHERE login_id = ?" [loginId]
    case tids of
        Pg.Only tid : _ -> getTeam db tid
        _               -> throwIO . Error $
            "Team login_id not found: " ++ show loginId

getTeamByApiKey :: Database -> UUID -> IO (Maybe Team)
getTeamByApiKey db@Database {..} key = do
    -- NOTE: We can optimize this to do one query rather than two.
    tids <- Pg.query dbConnection "SELECT id FROM teams WHERE api_key = ?" [key]
    case tids of
        Pg.Only tid : _ -> Just <$> getTeam db tid
        _               -> pure Nothing

getTeams :: Database -> IO [Team]
getTeams Database {..} = Pg.query_ dbConnection
    "SELECT teams.id, username, email, members, region, api_key \
    \FROM login INNER JOIN teams \
    \ON login_id = lid"

data UpdateTeam = UpdateTeam
    { updateTeamMembers :: T.Text
    , updateTeamRegion  :: T.Text
    }

-- | Probably worth adding a 'UpdateTeam' type here.
updateTeam :: Database -> TeamId -> UpdateTeam -> IO ()
updateTeam Database {..} tid UpdateTeam {..} = void $ Pg.execute dbConnection
    "UPDATE teams SET members = ?, region = ? WHERE id = ?"
    (updateTeamMembers, updateTeamRegion, tid)

unsafeResetPassword :: Database -> T.Text -> T.Text -> IO ()
unsafeResetPassword Database {..} email newPassword = do
    Just userId <- Users.getUserIdByName dbConnection email
    token <- Users.requestPasswordReset dbConnection userId 10
    errOrOk <- Users.applyNewPassword dbConnection token . Users.makePassword $
        Users.PasswordPlain newPassword
    print errOrOk

newtype ProblemId = ProblemId Int
    deriving ( Eq, Hashable, H.ToMarkup, H.ToValue, Pg.FromField, Pg.ToField
             , Http.FromHttpApiData, NFData, Log.ToLogStr
             )

instance Show ProblemId where show (ProblemId i) = show i

data Problem = Problem
    { problemId      :: !ProblemId
    , problemEnabled :: !Bool
    , problemBody    :: !T.Text
    } deriving (Eq, Generic, Show)

instance Pg.FromRow Problem
instance Pg.ToRow Problem

insertProblems :: Database -> [Problem] -> IO ()
insertProblems Database {..} = void . Pg.executeMany dbConnection
    "INSERT INTO problems (id, enabled, body) VALUES (?, ?, ?)"

upsertProblems :: Database -> [Problem] -> IO ()
upsertProblems Database {..} = void . Pg.executeMany dbConnection
    "INSERT INTO problems (id, enabled, body) \
    \VALUES (?, ?, ?) \
    \ON CONFLICT (id) DO UPDATE \
    \SET enabled = EXCLUDED.enabled, body = EXCLUDED.body"

-- | Currently only returns IDs, but we could return "names" as well.
getEnabledProblemIds :: Database -> IO [ProblemId]
getEnabledProblemIds Database {..} = map Pg.fromOnly <$> Pg.query_ dbConnection
    "SELECT id FROM problems WHERE enabled ORDER BY id ASC"

getEnabledProblem :: Database -> ProblemId -> IO Problem
getEnabledProblem Database {..} pid = do
    problems <- Pg.query dbConnection
        "SELECT id, enabled, body FROM problems WHERE id = ? AND enabled" [pid]
    case problems of
        p : _ -> pure p
        _     -> throwIO . Error $ "Problem not found: " ++ show pid

getEnabledProblems :: Database -> IO [Problem]
getEnabledProblems Database {..} =
    Pg.query_ dbConnection
        "SELECT id, enabled, body FROM problems WHERE enabled ORDER BY id ASC"

-- | This function is unsafe because it does not check if a problem has been
-- enabled yet; so it may retrieve "future" problems.
unsafeGetProblem :: Database -> ProblemId -> IO Problem
unsafeGetProblem Database {..} pid = do
    problems <- Pg.query dbConnection
        "SELECT id, enabled, body FROM problems WHERE id = ?" [pid]
    case problems of
        p : _ -> pure p
        _     -> throwIO . Error $ "Problem not found: " ++ show pid

data ProblemSummary = ProblemSummary
    { problemSummaryId                 :: !ProblemId
    , problemSummaryResult             :: !(Maybe SolutionResult)
    , problemSummaryMinDislikes        :: !(Maybe Int64)
    , problemSummaryContestMinDislikes :: !(Maybe Int64)
    } deriving (Eq, Show)

getEnabledProblemSummaries :: Database -> Maybe TeamId -> IO [ProblemSummary]
getEnabledProblemSummaries Database {..} tid = do
    rows <- Pg.query dbConnection
        "SELECT problems.id, \
        \    problems_min_dislikes.dislikes, \
        \    contest_min_dislikes.dislikes, \
        \    latest_judgements.status, latest_judgements.bonus_ok, \
        \    latest_judgements.dislikes, latest_judgements.message \
        \FROM problems \
        \LEFT OUTER JOIN problems_min_dislikes \
        \    ON problems.id = problems_min_dislikes.problem_id \
        \LEFT OUTER JOIN contest_min_dislikes \
        \    ON problems.id = contest_min_dislikes.problem_id \
        \LEFT OUTER JOIN latest_solutions \
        \    ON latest_solutions.team_id = ? \
        \    AND latest_solutions.problem_id = problems.id \
        \LEFT OUTER JOIN latest_judgements \
        \    ON latest_judgements.solution_id = latest_solutions.id \
        \WHERE problems.enabled \
        \ORDER BY problems.id ASC"
        [tid]
    pure $ do
        (problemSummaryId, problemSummaryMinDislikes,
                problemSummaryContestMinDislikes,
                status, bonusOk, dislikes, message) <- rows
        let problemSummaryResult =
                let r = makeSolutionResult status bonusOk dislikes message in
                if r == SolutionPending then Nothing else Just r
        pure ProblemSummary {..}

getAvailableBonusesForProblem
    :: Database -> TeamId -> ProblemId -> IO [(ProblemId, T.Text)]
getAvailableBonusesForProblem Database {..} tid pid = Pg.query dbConnection
    "SELECT latest_solutions.problem_id, bonus \
    \FROM latest_solutions \
    \INNER JOIN latest_judgements ON latest_solutions.id = solution_id \
    \INNER JOIN judgements_bonuses ON latest_judgements.id = judgement_id \
    \WHERE status = 'VALID' AND bonus_ok = TRUE AND get = TRUE \
    \AND team_id = ? AND judgements_bonuses.problem_id = ?"
    (tid, pid)

data SubmissionRefused
    = SizeLimited Int
    | RateLimited Int
    | SubmissionEnded

instance Show SubmissionRefused where
    show (SizeLimited n) =
        "Solution is too large, please limit to " <> show n <>
        "kb or contact the organisers"
    show (RateLimited n) =
        "Submission rate limit exceeded, please wait " ++
        show n ++ " seconds before trying again"
    show SubmissionEnded = "Submission has ended"

newtype SolutionId = SolutionId UUID
    deriving ( Eq, H.ToMarkup, H.ToValue, Pg.FromField, Pg.ToField
             , Http.FromHttpApiData, Aeson.ToJSON
             )

instance Show SolutionId where show (SolutionId i) = show i

submitSolution
    :: Database -> ProblemId -> TeamId -> T.Text
    -> IO (Either SubmissionRefused SolutionId)
submitSolution db@Database {..} problem team body = do
    -- Check rate limiting.
    now <- Time.getCurrentTime
    Features {..} <- getFeatures db
    mbRateLimited <- case featuresSubmissionRateSeconds of
        Just rateLimitSeconds -> do
            let rateLimit = fromIntegral (rateLimitSeconds :: Int)
            mbLastSubmission <- listToMaybe <$> Pg.query dbConnection
                "SELECT submitted_at FROM latest_solutions \
                \WHERE problem_id = ? AND team_id = ?"
                (problem, team)
            case mbLastSubmission of
                Nothing -> pure Nothing
                Just (Pg.Only lastSubmission) -> pure $
                    let next = Time.addUTCTime rateLimit lastSubmission in
                    if next > now
                        then Just $ RateLimited $
                            round (next `Time.diffUTCTime` now)
                        else Nothing
        _ -> pure Nothing

    -- Check that contest is still running.
    let mbContestEnded = SubmissionEnded <$ guard featuresSubmissionEnded

    -- Check size limit
    let mbSizeLimited = do
            let allowed = featuresSubmissionLimitKb * 1024
                actual  = B.length $ T.encodeUtf8 body
            guard $ actual > allowed
            pure $ SizeLimited featuresSubmissionLimitKb

    case mbRateLimited <|> mbContestEnded <|> mbSizeLimited of
        Just err -> pure $ Left err
        Nothing -> do
            sids <- Pg.query dbConnection
                "INSERT INTO solutions \
                \    (problem_id, team_id, body, submitted_at) \
                \VALUES (?, ?, ?, ?) RETURNING id"
                (problem, team, body, now)
            case sids of
                Pg.Only sid : _ -> pure $ Right sid
                _               -> throwIO . Error $ "Could not create solution"

data Solution = Solution
    { solutionId          :: !SolutionId
    , solutionProblem     :: !ProblemId
    , solutionTeam        :: !TeamId
    , solutionBody        :: !T.Text
    , solutionSubmittedAt :: !Time.UTCTime
    , solutionJudgement   :: !(Maybe JudgementId)
    , solutionStatus      :: !(Maybe JudgementStatus)
    , solutionBonuses     :: ![(ProblemId, T.Text, Bool)]
    , solutionBonusOk     :: !(Maybe Bool)
    , solutionDislikes    :: Maybe Int64
    , solutionMessage     :: Maybe T.Text
    } deriving (Generic, Show)

-- | Drastically simplified
data SolutionResult
    = SolutionPending
    | SolutionValid Int64
    | SolutionInvalid T.Text
    deriving (Eq, Show)

-- | Throws all of the fields in the database together into something nicer.
makeSolutionResult
    :: Maybe JudgementStatus
    -> Maybe Bool
    -> Maybe Int64
    -> Maybe T.Text
    -> SolutionResult
makeSolutionResult _status Nothing _dislikes _message = SolutionPending
makeSolutionResult status (Just bonusOk) dislikes message = case status of
    Nothing -> SolutionPending
    Just JudgementError ->
        SolutionInvalid $ fromMaybe "internal error" message
    Just JudgementInProgress ->
        SolutionPending
    Just JudgementValid -> case dislikes of
        _ | not bonusOk ->
            SolutionInvalid "invalid bonus claim"
        Nothing -> SolutionInvalid "internal error: missing dislikes"
        Just s  -> SolutionValid s
    Just JudgementInvalid ->
        SolutionInvalid $ fromMaybe "invalid" message

solutionResult :: Solution -> SolutionResult
solutionResult Solution {..} = makeSolutionResult
    solutionStatus solutionBonusOk solutionDislikes solutionMessage

-- | We pass in the team ID for safety; we never want to be accessing another
-- team's solutions accidentally.
getSolutionById :: Database -> TeamId -> SolutionId -> IO Solution
getSolutionById Database {..} tid sid = do
    rows <- Pg.query dbConnection
        "SELECT solutions.id, problem_id, team_id, body, \
        \    submitted_at, latest_judgements.id, status, bonus_ok, \
        \    dislikes, message \
        \FROM solutions \
        \LEFT JOIN latest_judgements ON solutions.id = latest_judgements.solution_id \
        \WHERE solutions.id = ? AND team_id = ?"
        (sid, tid)
    case rows of
        (solutionId, solutionProblem, solutionTeam, solutionBody,
                solutionSubmittedAt, solutionJudgement,
                solutionStatus, solutionBonusOk,
                solutionDislikes, solutionMessage) : _ -> do
            solutionBonuses <- case solutionJudgement of
                Nothing -> pure []
                Just jid -> Pg.query dbConnection
                    "SELECT problem_id, bonus, get \
                    \FROM judgements_bonuses \
                    \WHERE judgement_id = ?" [jid :: JudgementId]
            pure Solution {..}
        _     -> throwIO . Error $ "Solution not found: " ++ show sid

-- | Returns the latest submission for a specific by a team.
getSolutionByProblemId :: Database -> TeamId -> ProblemId -> IO (Maybe Solution)
getSolutionByProblemId db@Database {..} tid pid = do
    rows <- Pg.query dbConnection
        "SELECT DISTINCT ON (problem_id) id \
        \FROM solutions \
        \WHERE problem_id = ? AND team_id = ? \
        \ORDER BY problem_id, submitted_at DESC \
        \LIMIT 1"
        (pid, tid)
    case rows of
        Pg.Only sid : _ -> Just <$> getSolutionById db tid sid
        _               -> pure Nothing

getSolutionsByProblemBefore
    :: Database -> ProblemId -> Time.UTCTime -> IO [(TeamId, Maybe SolutionId)]
getSolutionsByProblemBefore Database {..} pid before = Pg.query dbConnection
    "SELECT DISTINCT ON (team_id) team_id, solutions.id \
    \FROM teams \
    \LEFT JOIN solutions ON teams.id = team_id \
    \WHERE problem_id = ? AND (submitted_at <= ? OR solutions.id IS NULL) \
    \ORDER BY team_id, submitted_at DESC"
    (pid, before)

getSolutionsByTeam
    :: Database -> TeamId -> IO [(ProblemId, Solution)]
getSolutionsByTeam Database {..} tid = do
    bonusRows <- Pg.query dbConnection
        "SELECT latest_solutions.problem_id, judgements_bonuses.problem_id, bonus, get \
        \FROM latest_solutions \
        \INNER JOIN latest_judgements ON latest_solutions.id = latest_judgements.solution_id \
        \INNER JOIN judgements_bonuses ON judgement_id = latest_judgements.id \
        \WHERE team_id = ? "
        (Pg.Only tid)
    let bonusMap = HMS.fromListWith (++) $ do
            (pid, pid', bonus, get) <- bonusRows
            pure (pid, [(pid', bonus, get)])

    solRows <- Pg.query dbConnection
        "SELECT DISTINCT ON (problem_id) solutions.id, problem_id, team_id, body, \
        \    submitted_at, latest_judgements.id, status, bonus_ok, \
        \    dislikes, message, problem_id \
        \FROM solutions \
        \LEFT JOIN latest_judgements ON solutions.id = latest_judgements.solution_id \
        \WHERE team_id = ? \
        \ORDER BY problem_id, submitted_at DESC"
        (Pg.Only tid)

    for solRows $ \(solutionId, solutionProblem, solutionTeam, solutionBody,
                solutionSubmittedAt, solutionJudgement,
                solutionStatus, solutionBonusOk,
                solutionDislikes, solutionMessage,
                pid) -> do
        let solutionBonuses = fromMaybe [] $ HMS.lookup pid bonusMap
        pure (pid, Solution {..})

-- | Returns the latest submission for a specific problem.
-- Unsafe because this allows retrieving solutions from any team.
-- Does not retrieve bonuses.
unsafeGetBestSolutionsByProblemId
    :: Database -> ProblemId -> Int -> IO [(Solution, T.Text, Int)]
unsafeGetBestSolutionsByProblemId Database {..} pid n = do
    rows <- Pg.query dbConnection
        "SELECT latest_solutions.id, problem_id, team_id, body, \
        \    submitted_at, latest_judgements.id, status, bonus_ok, \
        \    dislikes, message, login.username, \
        \    ( \
        \        SELECT COUNT(*) FROM judgements_bonuses \
        \        WHERE judgements_bonuses.judgement_id = latest_judgements.id AND get \
        \    ) \
        \FROM latest_solutions \
        \LEFT JOIN latest_judgements ON latest_solutions.id = latest_judgements.solution_id \
        \LEFT JOIN teams ON latest_solutions.team_id = teams.id \
        \LEFT JOIN login ON teams.login_id = login.lid \
        \WHERE problem_id = ? AND bonus_ok AND status = ? \
        \ORDER BY dislikes ASC \
        \LIMIT ?"
        (pid, JudgementValid, n)
    for rows $ \(solutionId, solutionProblem, solutionTeam, solutionBody,
                solutionSubmittedAt, solutionJudgement,
                solutionStatus, solutionBonusOk,
                solutionDislikes, solutionMessage, username,
                numBonusesAwarded) -> do
        let solutionBonuses  = []
        pure (Solution {..}, username, numBonusesAwarded)

data SolutionSummary = SolutionSummary
    { solutionSummaryId          :: !SolutionId
    , solutionSummarySubmittedAt :: !Time.UTCTime
    , solutionSummaryResult      :: !SolutionResult
    } deriving (Generic, Show)

getSolutionSummaries :: Database -> ProblemId -> TeamId -> IO [SolutionSummary]
getSolutionSummaries Database {..} problem team =
    fmap (map toSummary) $ Pg.query dbConnection
    "SELECT solutions.id, submitted_at, status, bonus_ok, dislikes, message \
    \FROM solutions \
    \LEFT JOIN latest_judgements ON solutions.id = latest_judgements.solution_id \
    \WHERE problem_id = ? AND team_id = ? \
    \ORDER BY submitted_at DESC"
    (problem, team)
  where
    toSummary (solutionSummaryId, solutionSummarySubmittedAt,
                status, bonusOk, dislikes, message) =
        let solutionSummaryResult =
                makeSolutionResult status bonusOk dislikes message in
        SolutionSummary {..}

newtype JudgementId = JudgementId Int64
    deriving ( Eq, H.ToMarkup, H.ToValue, Pg.FromField, Pg.ToField
             , Http.FromHttpApiData, NFData
             )

instance Show JudgementId where show (JudgementId i) = show i

data JudgementStatus
    = JudgementInProgress
    | JudgementValid
    | JudgementInvalid
    | JudgementError
    deriving (Eq, Show)

judgementStatusFromText :: Prism' T.Text JudgementStatus
judgementStatusFromText = prism'
    (\case
        JudgementInProgress -> "IN_PROGRESS"
        JudgementValid      -> "VALID"
        JudgementInvalid    -> "INVALID"
        JudgementError      -> "ERROR")
    (\case
        "IN_PROGRESS" -> Just JudgementInProgress
        "VALID"       -> Just JudgementValid
        "INVALID"     -> Just JudgementInvalid
        "ERROR"       -> Just JudgementError
        _             -> Nothing)

instance Pg.FromField JudgementStatus where
    fromField f mbBs = do
        n <- Pg.typename f
        if n /= "judgement_status"
        then Pg.returnError Pg.Incompatible f "judgement_status"
        else case mbBs of
            Nothing -> Pg.returnError Pg.UnexpectedNull f "judgement_status"
            Just bs -> case T.decodeUtf8 bs ^? judgementStatusFromText of
                Nothing -> Pg.returnError Pg.ConversionFailed f (show bs)
                Just x  -> return x

instance Pg.ToField JudgementStatus where
    toField = Pg.toField . review judgementStatusFromText

-- | Pick an unsolved solution and mark it as IN_PROGRESS
popAndStartJudgement :: Database -> IO (Maybe (JudgementId, Solution))
popAndStartJudgement db@Database {..} = do
    now <- Time.getCurrentTime
    unsolved <- try . Pg.withTransaction dbConnection $ Pg.query dbConnection
        "INSERT INTO judgements (id, solution_id, status, started_at) \
        \SELECT (SELECT COUNT(*) FROM judgements), solutions.id, ?, ? \
        \FROM solutions \
        \WHERE NOT EXISTS \
        \    (SELECT * FROM judgements WHERE solution_id = solutions.id) \
        \LIMIT 1 \
        \RETURNING id, solution_id, \
        \    (SELECT team_id FROM solutions WHERE id = solution_id)"
        (JudgementInProgress, now)
    case unsolved :: Either Pg.SqlError [(JudgementId, SolutionId, TeamId)] of
        Left _                      -> pure Nothing
        Right []                    -> pure Nothing
        Right ((jid, sid, tid) : _) -> do
            solution <- getSolutionById db tid sid
            pure $ Just (jid, solution)

-- | Pick a specific solution and mark it as IN_PROGRESS
startJudgement :: Database -> SolutionId -> IO (JudgementId, Solution)
startJudgement db@Database {..} sid = do
    teamIds <- Pg.query dbConnection
        "SELECT team_id FROM solutions WHERE id = ?" [sid]
    case teamIds of
        [] -> throwIO . Error $ "solution " <> show sid <> " has no team ID"
        Pg.Only tid : _ -> do
            now <- Time.getCurrentTime
            solution <- getSolutionById db tid sid
            jids <- Pg.query dbConnection
                "INSERT INTO judgements \
                \    (solution_id, status, started_at) \
                \VALUES (?, ?, ?) RETURNING id"
                (sid, JudgementInProgress, now)
            case jids of
                []              -> throwIO $ Error "Could not start judgement"
                Pg.Only jid : _ -> pure (jid, solution)

completeJudgement
    :: Database
    -> JudgementId
    -> JudgementStatus
    -> Maybe Int64
    -> Maybe String
    -> [(ProblemId, T.Text, Bool)]
    -> IO ()
completeJudgement Database {..} jid status dislikes message bonuses = do
    now <- Time.getCurrentTime
    void $ Pg.execute dbConnection
        "UPDATE judgements \
        \SET status = ?, dislikes = ?, message = ?, completed_at = ? \
        \WHERE id = ?"
        (status, dislikes, message, now, jid)
    void $ Pg.executeMany dbConnection
        "INSERT INTO judgements_bonuses (judgement_id, problem_id, bonus, get) \
        \VALUES (?, ?, ?, ?)"
        [ (jid, pid, bonus, get)
        | (pid, bonus, get) <- bonuses
        ]

updateJudgementsBonusOk
    :: Database -> [(JudgementId, Bool)] -> IO ()
updateJudgementsBonusOk Database {..} bonusOks = do
    let (oks, notOks) = bimap (map fst) (map fst) $ List.partition snd bonusOks
    unless (null oks) . void $ Pg.execute dbConnection
        "UPDATE judgements SET bonus_ok = TRUE  WHERE id IN ?" [Pg.In oks]
    unless (null notOks) . void $ Pg.execute dbConnection
        "UPDATE judgements SET bonus_ok = FALSE WHERE id IN ?" [Pg.In notOks]

setScoreboard
    :: Database
    -> [(TeamId, Int64)]     -- ^ Score fore every team
    -> [(ProblemId, Int64)]  -- ^ Min dislikes per problem
    -> IO ()
setScoreboard db@Database {..} scores minDislikes =
    Pg.withTransaction dbConnection $ do
    Features {..} <- getFeatures db
    update "scoreboard"
    unless featuresLightningEnded $ update "lightning_scoreboard"
    unless featuresScoreboardFrozen $ update "public_scoreboard"
    void . Pg.execute_ dbConnection $ "DELETE FROM problems_min_dislikes"
    void $ Pg.executeMany dbConnection
        "INSERT INTO problems_min_dislikes (problem_id, dislikes) VALUES (?, ?)"
        minDislikes
  where
    update table = do
        void . Pg.execute_ dbConnection $ "DELETE FROM " <> table
        void $ Pg.executeMany dbConnection
            ("INSERT INTO " <> table <> " (team_id, score) VALUES (?, ?)")
            scores

getScoreboardWith :: Database -> Pg.Query -> IO [(TeamId, T.Text, Int64)]
getScoreboardWith Database {..} table = Pg.query_ dbConnection $
    "SELECT team_id, username, score \
    \FROM " <> table <> " \
    \JOIN teams ON teams.id = team_id \
    \JOIN login ON login_id = lid \
    \WHERE score > 0 \
    \ORDER BY score DESC"

getPublicScoreboard :: Database -> IO [(TeamId, T.Text, Int64)]
getPublicScoreboard conn = getScoreboardWith conn "public_scoreboard"

getLightningScoreboard :: Database -> IO [(TeamId, T.Text, Int64)]
getLightningScoreboard conn = getScoreboardWith conn "lightning_scoreboard"

submitSourceCode
    :: Database -> TeamId -> B.ByteString -> IO (Either String T.Text)
submitSourceCode db@Database {..} team archive = do
    now <- Time.getCurrentTime
    Features {..} <- getFeatures db
    if B.length archive > featuresSourceCodeLimitKb * 1024 then
        pure . Left $ "Source code limit exceeded, limit to " <>
            show featuresSubmissionLimitKb <> "kb or contact organisers"
    else do
        let sha256 = T.decodeUtf8 . Base16.encode $ Sha256.hash archive
        print sha256
        void $ Pg.execute dbConnection
            "INSERT INTO teams_source_code VALUES (?, ?, ?, ?)"
            (team, now, sha256, Database.PostgreSQL.Simple.Binary archive)
        pure $ Right sha256

getSourceCodeInfo
    :: Database -> TeamId -> IO (Maybe (T.Text, Time.UTCTime))
getSourceCodeInfo Database {..} team = fmap listToMaybe $ Pg.query
    dbConnection
    "SELECT sha256, submitted_at \
    \FROM teams_source_code \
    \WHERE team_id = ? \
    \ORDER BY submitted_at DESC \
    \LIMIT 1"
    [team]

getSourceCodeArchive
    :: Database -> TeamId -> Time.UTCTime -> IO (Maybe B.ByteString)
getSourceCodeArchive Database {..} team before = do
    rows <- Pg.query dbConnection
        "SELECT archive FROM teams_source_code \
        \WHERE team_id = ? AND submitted_at <= ? \
        \ORDER BY submitted_at DESC LIMIT 1"
        (team, before)
    pure $ case rows of
        Pg.Only bs : _ -> Just bs
        _              -> Nothing
