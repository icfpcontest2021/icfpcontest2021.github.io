{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module BrainWall.Web
    ( main
    ) where

import qualified BrainWall.Database            as Db
import           BrainWall.Json
import           BrainWall.Main.Animate        (makeAnimation)
import           BrainWall.Problem
import qualified BrainWall.Svg                 as Svg
import qualified BrainWall.Version             as Version
import           BrainWall.Web.Assets
import           Control.Lens                  (preview, review)
import           Control.Monad                 (forM_, guard, unless, when)
import           Control.Monad.Identity        (runIdentity)
import           Control.Monad.Trans           (MonadIO, liftIO)
import qualified Data.Aeson                    as Aeson
import qualified Data.ByteString               as B
import           Data.Foldable                 (for_)
import           Data.List.NonEmpty            (NonEmpty (..))
import           Data.Maybe                    (isJust, isNothing, maybeToList)
import           Data.String                   (fromString)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Lazy                as TL
import           Data.Time.Format.ISO8601      (iso8601Show)
import qualified Data.UUID.Types               as Uuid
import qualified Data.Vector                   as V
import qualified Network.HTTP.Types.Status     as Http
import qualified Network.Wai.Handler.Warp      as Warp
import qualified Network.Wai.Logger            as WaiLogger
import           System.Environment            (lookupEnv)
import qualified System.IO                     as IO
import qualified Text.Blaze.Html.Renderer.Text as H
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as HA
import qualified Text.Digestive                as D
import qualified Text.Digestive.Blaze.Html5    as DH
import           Web.Spock
import           Web.Spock.Config
import           Web.Spock.Digestive
import           Web.Users.Postgresql          ()
import qualified Web.Users.Types               as Users

data Session = EmptySession
data AppState = AppState  -- Add logger?

smallText :: Monad m => Maybe T.Text -> D.Form H.Html m T.Text
smallText =
    D.check "limit to 1024 characters" ((<= 1024) . T.length) . D.text

data CreateTeam = CreateTeam
    { ctName     :: !T.Text
    , ctEmail    :: !T.Text
    , ctPassword :: !T.Text
    } deriving (Show)

createTeamForm :: Monad m => D.Form H.Html m CreateTeam
createTeamForm = CreateTeam
    <$> "name" D..: (D.check "Name is required"
            (not . T.null . T.strip)
            (smallText Nothing))
    <*> (D.validate confirmEmailCheck $ (,)
            <$> "email" D..: simpleEmailCheck (T.strip <$> smallText Nothing)
            <*> "confirmEmail" D..: (T.strip <$> smallText Nothing))
    <*> "password" D..: smallText Nothing
  where
    simpleEmailCheck = D.check "Invalid email address" $ \email ->
        case T.split (== '@') email of
            [user, domain] -> T.count "." domain >= 1 && T.length user >= 1
            _              -> False

    confirmEmailCheck (x, y)
        | x == y    = D.Success x
        | otherwise = D.Error "Email confirmation doesn't match"

createTeamView :: D.View H.Html -> H.Html
createTeamView view = DH.form view "?" $ do
    H.div H.! HA.class_ "error" $ DH.childErrorList "" view

    DH.label "name" view "team name"
    DH.inputText "name" view

    DH.label "email" view "email address"
    DH.inputText "email" view

    DH.label "confirmEmail" view "confirm email address"
    DH.inputText "confirmEmail" view

    DH.label "password" view "password"
    DH.inputPassword "password" view

    DH.inputSubmit "register team"

updateTeamForm :: Monad m => Db.Team -> D.Form H.Html m Db.UpdateTeam
updateTeamForm Db.Team {..} = Db.UpdateTeam
    <$> "members" D..: smallText teamMembers
    <*> "region"  D..: smallText teamRegion

updateTeamView :: D.View H.Html -> H.Html
updateTeamView view = DH.form view "?" $ do
    H.div H.! HA.class_ "error" $ DH.childErrorList "" view

    DH.label "members" view "members (one per line)"
    DH.inputTextArea (Just 5) Nothing "members" view

    DH.label "region" view
        "region(s) from which your team is participating (optional)"
    DH.inputTextArea (Just 5) Nothing "region" view

    DH.inputSubmit "update team"

submitSourceCodeForm :: Monad m => D.Form H.Html m FilePath
submitSourceCodeForm =
    D.validate (maybe (D.Error "missing file") pure) ("archive" D..: D.file)

submitSourceCodeView :: Db.Features -> Db.Team -> D.View H.Html -> H.Html
submitSourceCodeView Db.Features {..} Db.Team {..} view = DH.form view url $ do
    H.p $ do
        "Source code must be uploaded within 12 hours after the end of the "
        "contest to qualify "
        "for prizes.  Please submit a .tar.gz file, including a README "
        "containing a high level explanation of how your solution works. "
        "Archive size is limited to "
        H.toHtml featuresSourceCodeLimitKb <> "kb."
    DH.inputFile "archive" view
    DH.inputSubmit "upload source code"
  where
    url = T.pack $ "/teams/" <> show teamId <> "/source"

data Login = Login
    { lEmail    :: !T.Text
    , lPassword :: !T.Text
    } deriving (Show)

loginForm :: Monad m => D.Form H.Html m Login
loginForm = Login
    <$> "email" D..: smallText Nothing
    <*> "password" D..: smallText Nothing

loginView :: D.View H.Html -> H.Html
loginView view = DH.form view "?" $ do
    H.div H.! HA.class_ "error" $ DH.childErrorList "" view

    DH.label "email" view "email address"
    DH.inputText "email" view

    DH.label "password" view "password"
    DH.inputPassword "password" view

    DH.inputSubmit "log in"

    H.p $ H.a H.! HA.href "/register" $ "register a new team"

data SubmitSolution = SubmitSolution
    { ssBody :: !T.Text
    } deriving (Show)

submitSolutionForm :: MonadIO m => D.Form H.Html m SubmitSolution
submitSolutionForm = SubmitSolution
    <$> D.validateM check ("body" D..: D.file)
  where
    check Nothing = pure $ D.Error "missing file"
    check (Just path) = do
        contents <- liftIO $ T.readFile path
        if T.null (T.strip contents) then
            pure $ D.Error "empty file"
        else
            pure $ D.Success contents

submitSolutionView :: Db.ProblemId -> D.View H.Html -> H.Html
submitSolutionView pid view = DH.form view url $ do
    DH.inputFile "body" view
    DH.inputSubmit "submit pose"
  where
    url = T.pack $ "/problems/" <> show pid <> "/solutions"

blaze :: MonadIO m => H.Html -> ActionCtxT ctx m a
blaze = html . TL.toStrict . H.renderHtml

lookupSessionTeam
    :: SpockAction Db.Database sess st (Maybe (Users.SessionId, Db.Team))
lookupSessionTeam = do
    mbSession <- cookie "session"
    case mbSession of
        Nothing -> pure Nothing
        Just session -> runQuery $ \conn -> do
            let sid = Users.SessionId session
            mbLoginId <- Users.verifySession (Db.dbConnection conn) sid 0
            case mbLoginId of
                Nothing -> pure Nothing
                Just loginId -> do
                    team <- Db.getTeamByLoginId conn loginId
                    pure $ Just (sid, team)

getSessionTeam :: SpockAction Db.Database sess st (Users.SessionId, Db.Team)
getSessionTeam = do
    mbTeam <- lookupSessionTeam
    maybe (htmlError Http.forbidden403 "not logged in") pure mbTeam

htmlError :: Http.Status -> String -> SpockAction db sess st a
htmlError status message = do
    setStatus status
    blaze . template Nothing "Error" . H.section . H.p $ H.toHtml message

apiError :: Http.Status -> String -> SpockAction db sess st a
apiError status message = do
    setStatus status
    json $ Aeson.object ["error" Aeson..= message]

bearerTeam :: SpockAction Db.Database sess st Db.Team
bearerTeam = do
    mbApiKey <- (>>= takeBearer) <$> header "Authorization"
    case mbApiKey >>= Uuid.fromText of
        Nothing -> apiError Http.forbidden403
            "missing 'Authorization: Bearer API_KEY' header"
        Just apiKey -> do
            mbTeam <- runQuery $ \conn -> Db.getTeamByApiKey conn apiKey
            case mbTeam of
                Just t  -> pure t
                Nothing -> apiError Http.forbidden403 "Invalid API key"
  where
    takeBearer = fmap T.strip . T.stripPrefix "Bearer"

template :: Maybe Db.Team -> T.Text -> H.Html -> H.Html
template mbTeam title content = H.docTypeHtml $ do
    H.head $ do
        H.title $ H.toHtml title
        H.link H.! HA.rel "stylesheet" H.! HA.href "/assets/style.css"
    H.body $ do
        H.header $ do
            H.a H.! HA.href "https://icfpcontest2021.github.io/"
                H.! HA.target "_blank" $ "ICFPC2021"

            H.a H.! HA.href "/problems" $ "problems"

            H.a H.! HA.href "/teams" $ "scores"

            H.div H.! HA.class_ "session" $ case mbTeam of
                Nothing           -> H.a H.! HA.href "/login" $ "login"
                Just Db.Team {..} -> H.a
                    H.! HA.href ("/teams/" <> H.toValue teamId)
                    $ H.toHtml teamName

        H.h1 $ H.toHtml title
        content

        H.footer $ do
            "cloud hosting provided by "
            H.a H.! HA.href "https://fugue.co/" $ "fugue"
            " | version "
            H.toHtml Version.version

problemUrl :: Db.ProblemId -> String
problemUrl pid = "/problems/" <> show pid

bonusList :: H.Html -> [(Db.ProblemId, BonusType)] -> H.Html
bonusList _ [] = H.p "none"
bonusList sep bonuses = H.ul $ for_ bonuses $ \(pid, ty) -> H.li $ do
    let style = "color: " <> Svg.bonusColor ty
        url = problemUrl pid
    H.span H.! HA.style (H.toValue style) $ H.toHtml $
        review bonusTypeFromText ty
    " " <> sep <> " "
    H.a H.! HA.href (H.toValue url) $
        "problem " <> H.toHtml pid

app :: SpockM Db.Database Session AppState ()
app = do
    runQuery Db.initialize
    get "healthcheck" $ text "ok"
    prehook checkPassword webApp
    apiApp
  where
    checkPassword = do
        Db.Features {..} <- runQuery Db.getFeatures
        for_ featuresWebPassword $ \password ->
            let checker = \user pass ->
                    unless (user == "admin" && pass == password) $ do
                        setStatus Http.status401
                        text "Locked" in
            requireBasicAuth "Password Required" checker $ \() -> pure ()

webApp :: SpockM Db.Database Session AppState ()
webApp = do
    get "assets/style.css" $ do
        setHeader "Content-Type" "text/css; charset=utf-8"
        bytes style_css

    get root $ redirect "/problems"

    get "teams" $ do
        myTeam <- fmap snd <$> lookupSessionTeam
        sb <- runQuery Db.getPublicScoreboard
        features <- runQuery Db.getFeatures
        blaze . template myTeam "Scoreboard" . H.section $ do
            when (Db.featuresScoreboardFrozen features) $
                H.p "The public scoreboard is currently frozen."

            H.table $ do
                H.tr $ do
                    H.th "place"
                    H.th "team"
                    H.th "score"

                forM_ (zip [1 :: Int ..] sb) $
                    \(pos, (_tid, name, score)) -> H.tr $ do
                        H.td $ H.toHtml pos
                        H.td $ H.toHtml name
                        H.td $ H.toHtml score

    getpost ("teams" <//> var) $ \tid -> do
        features <- runQuery Db.getFeatures
        myTeam <- fmap snd <$> lookupSessionTeam
        team <- runQuery $ \conn -> Db.getTeam conn tid
        (view, mbUpdateTeam) <- runForm "team" $ updateTeamForm team
        mbSourceCodeInfo <- runQuery $ \conn -> Db.getSourceCodeInfo conn tid
        let isMyTeam = Just (Db.teamId team) == fmap Db.teamId myTeam
        case mbUpdateTeam of
            Just _ | not isMyTeam -> htmlError Http.forbidden403 "forbidden"
            Just updateTeam -> do
                runQuery $ \conn -> Db.updateTeam conn tid updateTeam
                redirect ("/teams/" <> T.pack (show $ Db.teamId team))
            Nothing -> blaze . template myTeam (Db.teamName team) $ H.section $ do
                when isMyTeam $ do
                    H.h2 "Team info"
                    H.p $ do
                        "API key: "
                        H.a H.! HA.href "#api-key" $ "show"
                        H.pre H.! HA.class_ "hidden" H.! HA.id "api-key" $
                            H.code . H.toHtml . Uuid.toString $
                            Db.teamApiKey team

                    updateTeamView view

                    H.h2 "Source code"
                    H.p H.! HA.style "word-break: break-word" $
                        case mbSourceCodeInfo of
                            Nothing ->
                                "Source code has not yet been submitted."
                            Just (sha256, submitted) ->
                                "Source code has been submitted at " <>
                                H.toHtml (iso8601Show submitted) <>
                                " with SHA256 checksum: " <>
                                H.toHtml sha256
                    submitSourceCodeView features team . runIdentity $
                        D.getForm "source" submitSourceCodeForm

                    H.form H.! HA.method "POST" H.! HA.action "/logout" $
                        H.input H.! HA.type_ "submit" H.! HA.value "log out"

    post ("teams" <//> var <//> "source") $ \tid -> do
        myTeam <- fmap snd <$> lookupSessionTeam
        team <- runQuery $ \conn -> Db.getTeam conn tid
        (_, mbPath) <- runForm "source" submitSourceCodeForm
        unless (Just (Db.teamId team) == fmap Db.teamId myTeam) $
            htmlError Http.forbidden403 "forbidden"
        case mbPath of
            Nothing -> redirect ("/teams/" <> T.pack (show tid))
            Just path -> do
                archive <- liftIO $ B.readFile path
                result <- runQuery $ \conn ->
                    Db.submitSourceCode conn tid archive
                case result of
                    Left err -> htmlError Http.status400 err
                    Right _  -> redirect ("/teams/" <> T.pack (show tid))

    getpost "register" $ do
        let makeTeam CreateTeam {..} = do
                runQuery $ \conn -> do
                    errOrUnit <- Db.createTeam conn $ Users.User
                        { Users.u_name     = ctName
                        , Users.u_email    = ctEmail
                        , Users.u_password = Users.makePassword $
                            Users.PasswordPlain ctPassword
                        , Users.u_active   = True
                        }
                    case errOrUnit of
                        Left err -> pure . D.Error . H.toHtml $ show err
                        Right _  -> pure $ D.Success ()

        (view, mbTeam) <- runForm "register" $
            D.validateM makeTeam createTeamForm
        myTeam <- fmap snd <$> lookupSessionTeam
        case mbTeam of
            Nothing -> do
                blaze . template myTeam "Register team" . H.section $
                    createTeamView view
            Just () ->
                blaze . template myTeam "Registration successful" . H.section $
                    H.p $ do
                        "Team created.  You can "
                        H.a H.! HA.href "/login" $ "log in"
                        " now."

    getpost "login" $ do
        let doLogin Login {..} = do
                mbSessionId <- runQuery $ \conn -> Users.authUser
                    (Db.dbConnection conn)
                    lEmail (Users.PasswordPlain lPassword) week
                pure $ case mbSessionId of
                    Nothing  -> D.Error "Bad login"
                    Just sid -> D.Success sid

        (view, mbSession) <- runForm "login" $ D.validateM doLogin loginForm
        case mbSession of
            Nothing -> do
                myTeam <- fmap snd <$> lookupSessionTeam
                blaze . template myTeam "Log in" . H.section $ loginView view
            Just (Users.SessionId sessionId) -> do
                setCookie "session" sessionId cookieSettings
                redirect "/"

    post "logout" $ do
        sid <- fst <$> getSessionTeam
        runQuery $ \conn -> Users.destroySession (Db.dbConnection conn) sid
        redirect "/"

    get "problems" $ do
        myTeam <- fmap snd <$> lookupSessionTeam
        problems <- runQuery $ \conn -> Db.getEnabledProblemSummaries conn $
            Db.teamId <$> myTeam
        blaze . template myTeam "Problems" $ H.section $ do
            H.table $ do
                H.tr $ do
                    H.th "problem"
                    H.th "your dislikes"
                    H.th "minimal (contest)"
                    H.th "minimal"
                forM_ problems $ \Db.ProblemSummary {..} -> H.tr $ do
                    H.td $
                         H.a H.! HA.href (H.toValue $ problemUrl problemSummaryId) $
                         H.toHtml problemSummaryId
                    H.td $ case problemSummaryResult of
                        Nothing                       -> mempty
                        Just Db.SolutionPending       -> mempty
                        Just (Db.SolutionInvalid _)   -> "❌"
                        Just (Db.SolutionValid   num) -> H.toHtml num
                    H.td $ maybe mempty (H.toHtml)
                        problemSummaryContestMinDislikes
                    H.td $ maybe mempty (H.toHtml) problemSummaryMinDislikes

    get ("problems" <//> var) $ \pid -> do
        myTeam <- fmap snd <$> lookupSessionTeam
        Db.Problem {..} <- runQuery $ \conn -> Db.getEnabledProblem conn pid
        features <- runQuery Db.getFeatures
        solutions <- case myTeam of
            Nothing           -> pure []
            Just Db.Team {..} -> runQuery $ \c ->
                Db.getSolutionSummaries c pid teamId
        availableBonuses <- case myTeam of
            Nothing           -> pure []
            Just Db.Team {..} -> runQuery $ \conn ->
                Db.getAvailableBonusesForProblem conn teamId pid

        problem <- either (htmlError Http.status500) pure $
            decodeWith (decodeProblem features) (T.encodeUtf8 problemBody)
        let svg = Svg.encodeSvg . Svg.svgSetLongEdge 800 . fmap fromIntegral $
                Svg.problemToSvgWith Svg.defaultProblemToSvgSettings
                    {Svg.ptssFillHole = Just "#00000066"}
                    problem

        submittedView <- liftIO $ D.getForm "solution" submitSolutionForm

        blaze . template myTeam (T.pack $ "Problem " <> show problemId) $ H.section $ do
            H.unsafeByteString . T.encodeUtf8 $ T.pack svg

            when (Db.featuresAnyBonuses features) $ do
                H.p $ "Bonuses that can be obtained in this problem:"
                bonusList "for"
                    [ (Db.ProblemId bonusProblem, bonusBonus)
                    | Bonus {..} <- V.toList $ problemBonuses problem
                    ]

            H.form H.! HA.action (H.toValue $ problemUrl pid <> "/download") $
                H.input H.! HA.type_ "submit" H.! HA.value "download problem"

            when (isJust myTeam) $ do
                H.h2 "Your poses"
                H.table $ do
                    H.tr $ do
                        H.th "submitted at"
                        H.th "dislikes"
                    for_ solutions $ \Db.SolutionSummary {..} -> H.tr $ do
                        let url = "/solutions/" <> H.toValue solutionSummaryId
                        H.td $ H.a H.! HA.href url $ H.toHtml $
                            iso8601Show solutionSummarySubmittedAt
                        H.td $ case solutionSummaryResult of
                            Db.SolutionPending     -> "⏳"
                            Db.SolutionInvalid _   -> "❌"
                            Db.SolutionValid   num -> H.toHtml num

                when (null solutions) $ H.p $
                    "No poses submitted"

                H.h2 "Submit pose"
                when (Db.featuresAnyBonuses features) $
                    if null availableBonuses then
                        H.p "No available bonuses"
                    else do
                        H.p "Available bonuses:"
                        bonusList "from" $ do
                            (p, txt) <- availableBonuses
                            ty <- maybeToList $ preview bonusTypeFromText txt
                            pure (p, ty)

                submitSolutionView pid submittedView

            when (isNothing myTeam) $ do
                H.h2 "Submit pose"
                H.p $ do
                    "You must "
                    H.a H.! HA.href "/login" $ "log in"
                    " to submit poses."

    get ("problems" <//> var <//> "download") $ \pid -> do
        Db.Problem {..} <- runQuery $ \conn -> Db.getEnabledProblem conn pid
        features <- runQuery Db.getFeatures
        problem <- either (htmlError Http.status500) pure $
            decodeWith (decodeProblem features) $ T.encodeUtf8 problemBody
        setHeader "Content-Disposition" $
            "attachment; filename=\"" <> T.pack (show pid) <> ".problem\""
        json $ encodeProblem features problem

    getpost ("problems" <//> var <//> "solutions") $ \problemId -> do
        team <- snd <$> getSessionTeam
        (_, mbSubmit) <- runForm "solution" submitSolutionForm
        -- TODO: Validate size under x kb?
        for_ mbSubmit $ \submit -> do
            errOrId <- runQuery $ \conn ->
                Db.submitSolution conn problemId (Db.teamId team) $
                ssBody submit
            either (htmlError Http.status429 . show) pure errOrId
        redirect . T.pack $ problemUrl problemId

    get ("solutions" <//> var) $ \sid -> do
        -- TODO: Helper like `mustSessionTeam`?
        team <- snd <$> getSessionTeam
        solution@Db.Solution {..} <- runQuery $ \conn ->
            Db.getSolutionById conn (Db.teamId team) sid
        Db.Problem {..} <- runQuery $ \conn ->
            Db.getEnabledProblem conn solutionProblem
        when (Db.teamId team /= solutionTeam) $
            htmlError Http.forbidden403 "Forbidden"
        features <- runQuery Db.getFeatures
        let result = Db.solutionResult solution
            enabledBonuses = featuresEnabledBonuses features

        let solutionOrError = decodeWith (decodeSolution features) $
                T.encodeUtf8 solutionBody
            animationOrError
                | Db.SolutionPending <- result = pure "pending..."
                | otherwise = do
                    problem <- decodeWith (decodeProblem features) $
                        T.encodeUtf8 problemBody
                    sol <- solutionOrError
                    pure . Svg.encodeSvg . Svg.svgSetLongEdge 800 .
                        makeAnimation problem sol . Just $ case result of
                            Db.SolutionPending       -> Left $ "pending" :| []
                            Db.SolutionValid   score -> Right $ fromIntegral score
                            Db.SolutionInvalid err   -> Left $ T.unpack err :| []

        blaze . template (Just team) "Pose" . H.section $ do
            H.p $ do
                "Submitted for "
                H.a H.! HA.href (H.toValue $ problemUrl problemId) $
                    "problem " <> H.toHtml problemId
                " at "
                H.toHtml $ iso8601Show solutionSubmittedAt
                "."
            H.p $ case result of
                Db.SolutionPending          -> "Pending evaluation..."
                Db.SolutionValid   dislikes -> "Dislikes: " <> H.toHtml dislikes
                Db.SolutionInvalid err      -> "Error: " <> H.toHtml err

            H.form H.! HA.action ("/solutions/" <> H.toValue sid <> "/download") $
                H.input H.! HA.type_ "submit" H.! HA.value "download solution"

            when (Db.featuresAnyBonuses features) $ do
                let used = case solutionOrError of
                        Left _ -> []
                        Right sol ->
                            [ (Db.ProblemId p, b)
                            | ClaimedBonus p b _ <- V.toList $
                                BrainWall.Problem.solutionBonuses sol
                            ]
                unless (null used) $ do
                    H.p "Used bonuses:"
                    bonusList "from" used

            case animationOrError of
                Left _    -> mempty
                Right svg -> H.unsafeByteString . T.encodeUtf8 $ T.pack svg

            when (Db.featuresAnyBonuses features) $ do
                let unlocked = case result of
                        Db.SolutionValid _ -> do
                            (p, txt, ok) <- solutionBonuses
                            guard ok
                            ty <- maybeToList $ preview bonusTypeFromText txt
                            guard $ ty `elem` enabledBonuses
                            pure (p, ty)
                        _ -> []

                unless (null unlocked) $ do
                    H.p $ "Bonuses unlocked:"
                    bonusList "for" unlocked

    get ("solutions" <//> var <//> "download") $ \sid -> do
        team <- snd <$> getSessionTeam
        Db.Solution {..} <- runQuery $ \conn ->
            Db.getSolutionById conn (Db.teamId team) sid
        setHeader "Content-Disposition" $
            "attachment; filename=\"" <> T.pack (show sid) <> ".solution\""
        text solutionBody
  where
    days n = n * 60 * 60 * 24
    week   = days 7

    cookieSettings = defaultCookieSettings
        { cs_EOL = CookieValidFor week
        }

apiApp :: SpockM Db.Database Session AppState ()
apiApp = do
    get "api/hello" $ do
        team <- bearerTeam
        json $ Aeson.object ["hello" Aeson..= Db.teamName team]

    get ("api/problems" <//> var) $ \pid -> do
        _team <- bearerTeam
        Db.Problem {..} <- runQuery $ \conn -> Db.getEnabledProblem conn pid
        features <- runQuery Db.getFeatures
        problem <- either (apiError Http.status500) pure $
            decodeWith (decodeProblem features) $ T.encodeUtf8 problemBody
        json $ encodeProblem features problem

    post ("api/problems" <//> var <//> "solutions") $ \pid -> do
        team <- bearerTeam
        features <- runQuery Db.getFeatures
        content <- body
        solution <- either (apiError Http.status400) pure $
            decodeWith (decodeSolution features) content
        errOrSid <- runQuery $ \conn ->
            Db.submitSolution conn pid (Db.teamId team) $
            encodeText $ encodeSolution features solution
        case errOrSid of
            Right sid -> json $ Aeson.object ["id" Aeson..= sid]
            Left err  -> apiError Http.status429 $ show err

    get ("api/problems/" <//> var <//> "solutions" <//> var) $ \pid sid -> do
        features <- runQuery Db.getFeatures
        let enabledBonuses = featuresEnabledBonuses features
        team <- bearerTeam
        solution@Db.Solution {..} <- runQuery $ \db ->
            Db.getSolutionById db (Db.teamId team) sid
        unless (solutionProblem == pid) $ apiError Http.status404 "Not found"
        json . Aeson.object $ case Db.solutionResult solution of
            Db.SolutionPending ->
                [ "state" Aeson..= ("PENDING" :: T.Text)
                ]
            Db.SolutionValid dislikes ->
                [ "state" Aeson..= ("VALID" :: T.Text)
                , "dislikes" Aeson..= dislikes
                ] ++
                [ "awarded_bonuses" Aeson..=
                    [ Aeson.object
                        [ "bonus"   Aeson..= txt
                        , "problem" Aeson..= pid'
                        ]
                    | (Db.ProblemId pid', txt, awarded) <- solutionBonuses
                    , ty <- maybeToList $ preview bonusTypeFromText txt
                    , awarded
                    , ty `elem` enabledBonuses
                    ]
                | Db.featuresAnyBonuses features
                ]
            Db.SolutionInvalid err ->
                [ "state" Aeson..= ("INVALID" :: T.Text)
                , "error" Aeson..= err
                ]

main :: IO ()
main = WaiLogger.withStdoutLogger $ \logger -> do
    dbConfig <- Db.configFromEnv
    bindPort <- fmap read <$> lookupEnv "BRAINWALL_BIND_PORT"
    bindAddress <- fmap fromString <$> lookupEnv "BRAINWALL_BIND_ADDRESS"
    IO.hPutStrLn IO.stderr $
        "Listening on " <> show bindAddress <> " port " <> show bindPort

    let settings =
            maybe id Warp.setPort bindPort $
            maybe id Warp.setHost bindAddress $
            Warp.setLogger logger $
            Warp.defaultSettings

    let dbPool = PCConn $ ConnBuilder
            { cb_createConn        = Db.openDatabase dbConfig
            , cb_destroyConn       = Db.closeDatabase
            , cb_poolConfiguration = PoolCfg
                { pc_stripes      = 2
                , pc_resPerStripe = 5
                , pc_keepOpenTime = 300
                }
            }

    spockCfg0 <- defaultSpockCfg EmptySession dbPool AppState
    let spockCfg = spockCfg0 {spc_maxRequestSize = Just $ 16 * 1024 * 1024}

    Warp.runSettings settings =<< spockAsApp (spock spockCfg app)
