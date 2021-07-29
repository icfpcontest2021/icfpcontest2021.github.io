{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module BrainWall.Main.Gallery
    ( main
    ) where

import qualified BrainWall.Database            as Db
import           BrainWall.Json
import           BrainWall.Problem
import qualified BrainWall.Svg                 as Svg
import           Control.Lens                  (review)
import           Control.Monad                 (when)
import           Data.Foldable                 (for_)
import           Data.Int                      (Int64)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Lazy.IO             as TL
import           Data.Traversable              (for)
import qualified Data.Vector                   as V
import qualified Text.Blaze.Html.Renderer.Text as H
import qualified Text.Blaze.Html5              as H

data SolutionInfo = SolutionInfo
    { siSolution       :: !Solution
    , siDislikes       :: !Int64
    , siTeam           :: !T.Text
    , siBonusesAwarded :: !Int
    } deriving (Show)

data ProblemInfo = ProblemInfo
    { piId        :: !Db.ProblemId
    , piProblem   :: !Problem
    , piSolutions :: ![SolutionInfo]
    } deriving (Show)

getBestSolutions :: Db.Database -> Db.ProblemId -> Int -> IO [SolutionInfo]
getBestSolutions db pid n = do
    solutions <- Db.unsafeGetBestSolutionsByProblemId db pid n
    pure $ do
        (solution, siTeam, siBonusesAwarded) <- solutions
        siDislikes <- case Db.solutionResult solution of
            Db.SolutionValid ds -> pure ds
            _                   -> []
        siSolution <- either fail pure .
            decodeWith (decodeSolution Db.allFeatures) .
            T.encodeUtf8 $ Db.solutionBody solution
        pure SolutionInfo {..}

getProblemInfos :: Db.Database -> IO [ProblemInfo]
getProblemInfos db = do
    problems <- Db.getEnabledProblems db
    for problems $ \Db.Problem{..} -> do
        piProblem <- either fail pure .
            decodeWith (decodeProblem Db.allFeatures) $
            T.encodeUtf8 problemBody

        piSolutions <- getBestSolutions db problemId 3
        let piId = problemId
        pure ProblemInfo {..}

rendepiProblemInfo :: ProblemInfo -> H.Html
rendepiProblemInfo ProblemInfo {..} = H.tr $ do
    H.td $ H.toHtml piId

    H.td . H.unsafeByteString . T.encodeUtf8 . T.pack . Svg.encodeSvg $
        Svg.problemToSvg piProblem

    for_ piSolutions $ \SolutionInfo {..} ->
        H.td $ do
            H.unsafeByteString . T.encodeUtf8 . T.pack . Svg.encodeSvg $
                Svg.problemToSvgWith
                    Svg.defaultProblemToSvgSettings
                        {Svg.ptssSolution = Just siSolution}
                    piProblem
            H.div $ do
                H.strong $ H.toHtml siDislikes
                " dislikes by "
                H.strong $ H.toHtml siTeam
                case V.toList (solutionBonuses siSolution) of
                     [] -> mempty
                     ClaimedBonus {..} : _ -> do
                         " using "
                         H.toHtml $ review bonusTypeFromText claimedBonusBonus
                when (siBonusesAwarded > 0) $ do
                    ", "
                    H.toHtml siBonusesAwarded
                    " bonuses awarded"

rendepiProblemInfos :: [ProblemInfo] -> H.Html
rendepiProblemInfos infos = H.docTypeHtml $ do
    H.head $ H.style "table {border-spacing: 40px 20px}"
    H.body . H.table $ foldMap rendepiProblemInfo infos

main :: IO ()
main = do
    dbConfig <- Db.configFromEnv
    Db.withDatabase dbConfig $ \conn -> do
        rows <- getProblemInfos conn
        TL.putStrLn . H.renderHtml $ rendepiProblemInfos rows
