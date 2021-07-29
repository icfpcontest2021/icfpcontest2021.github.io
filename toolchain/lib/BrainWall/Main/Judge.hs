{-# LANGUAGE RecordWildCards #-}
module BrainWall.Main.Judge
    ( main
    ) where

import           BrainWall.Database (allFeatures)
import           BrainWall.Json
import           BrainWall.Problem
import           BrainWall.V2
import           Control.Lens       (review)
import           Data.Foldable      (for_)
import qualified Data.Text          as T
import           System.Environment (getArgs, getProgName)
import           System.Exit        (exitFailure)
import qualified System.IO          as IO

main :: IO ()
main = do
    progName <- getProgName
    args <- getArgs
    case args of
        [problemPath, solutionPath] -> do
            problem <- decodeFileWith (decodeProblem allFeatures) problemPath
            solution <- decodeFileWith (decodeSolution allFeatures) solutionPath
            case judgeSolution problem solution of
                Left errs      -> mapM_ putStrLn errs >> exitFailure
                Right dislikes -> do
                    let bonuses = awardBonusesForSolution problem solution
                    putStrLn $ "OK, dislikes: " ++ show dislikes
                    for_ bonuses $ \(Bonus{..}, ok) -> putStrLn $ "Bonus " <>
                        T.unpack (review bonusTypeFromText bonusBonus) <>
                        " at " <> review v2FromString bonusPosition <>
                        (if ok then " unlocked" else " not unlocked")
        _ -> do
            IO.hPutStrLn IO.stderr $
                "Usage: " ++ progName ++ " file.problem file.solution"
            exitFailure
