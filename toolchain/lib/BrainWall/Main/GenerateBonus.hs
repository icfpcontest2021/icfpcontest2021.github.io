{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module BrainWall.Main.GenerateBonus
    ( main
    ) where

import qualified BrainWall.Box                   as Box
import           BrainWall.Database              (allFeatures)
import           BrainWall.Json
import           BrainWall.Main.InsertProblems   (iterateProblems,
                                                  problemIdFromFilePath,
                                                  solutionIdFromFilePath)
import           BrainWall.Polygon
import           BrainWall.Polygon.ContainsPoint (pointInPolygon)
import           BrainWall.Problem
import           BrainWall.V2
import           Control.Monad                   (unless)
import           Data.Aeson.Encode.Pretty        (encodePretty)
import qualified Data.ByteString.Lazy            as BL
import           Data.Foldable                   (for_)
import qualified Data.HashMap.Strict             as HMS
import           Data.List
import           Data.Maybe                      (fromJust)
import           Data.Ord
import           Data.Traversable                (for)
import qualified Data.Vector                     as V
import qualified Options.Applicative.Extended    as OA
import           System.FilePath                 (replaceExtension)
import qualified Test.QuickCheck                 as QC

bonusPositions :: Hole -> Solution -> [V2 Integer] -> [V2 Integer]
bonusPositions (Hole poly) solution = go
  where
    go bonusesPlaced = case allPoints of
        [] -> []
        _  -> thisBonus : go (thisBonus:bonusesPlaced)
      where
        thisBonus = maximumBy (comparing solutionDistance) allPoints
        Just box = foldMap (Just . Box.fromV2) $ unPolygon poly
        allPoints = filter (`notElem` bonusesPlaced) $
          filter (`pointInPolygon` poly) $
          [V2 x y | x <- [v2X (Box.topLeft box), v2X (Box.topLeft box) + 3 .. v2X (Box.bottomRight box)] , y <- [v2Y (Box.topLeft box), v2Y (Box.topLeft box) + 3 .. v2Y (Box.bottomRight box)] ]
        solutionDistance v2 = V.minimum $ V.map (dist v2) (solutionVertices solution V.++ V.fromList bonusesPlaced V.++ unPolygon poly)
        dist :: V2 Integer -> V2 Integer -> Double
        dist v1 v2 = sqrt $ fromIntegral $ squaredDistance v1 v2

-- | Shuffle such that no element ends up at its original position.
properShuffle :: Eq a => [a] -> QC.Gen [a]
properShuffle things =
    fmap (map $ snd . snd) $
    indexedShuffle `QC.suchThat` \l -> and [i /= j | (i, (j, _)) <- l]
  where
    indexedShuffle = zip [0 :: Int ..] <$> QC.shuffle (zip [0 ..] things)

genBonusAssignment :: [Int] -> QC.Gen [(Int, (Int, BonusType))]
genBonusAssignment problemIds = fmap (zip problemIds) $ do
    ordered <- for problemIds $ \problemId -> do
        types <- QC.shuffle [SuperFlex, WallHack, Globalist, BreakALeg, SuperFlex, WallHack]
        pure (problemId, head types)
    properShuffle ordered

type Problems = HMS.HashMap Int (FilePath, Problem, FilePath, Solution)

type BonusLayer = [(Int, Bonus)]

genBonusLayer :: Problems -> QC.Gen BonusLayer
genBonusLayer problems = do
    assignment <- genBonusAssignment $ HMS.keys problems
    for (HMS.toList problems) $ \(pid, (_, problem, _, solution)) -> do
        let hole    = problemHole problem
            (pid', ty) = fromJust $ lookup pid assignment
            takenPositions = bonusPosition <$> V.toList (problemBonuses problem)
            position = head $ bonusPositions hole solution takenPositions
        pure (pid, Bonus pid' ty position)

parseProblems :: [FilePath] -> IO Problems
parseProblems problemPaths = do
    problems <- fmap HMS.fromList $ for problemPaths $ \path -> do
        pid <- maybe (fail $ "Bad file name: " <> show path) pure $
            problemIdFromFilePath path
        problem <- decodeFileWith (decodeProblem decodeFeatures) path
        pure (pid, (path, problem))
    solutions <- fmap HMS.fromList $ for solutionPaths $ \path -> do
        pid <- maybe (fail $ "Bad file name: " <> show path) pure $
            solutionIdFromFilePath path
        solution <- decodeFileWith (decodeSolution decodeFeatures) path
        pure (pid, (path, solution))
    unless (HMS.null $ HMS.difference problems solutions) $
      fail $ "These problems don't have solutions: " <> show (HMS.difference problems solutions)
    unless (HMS.null $ HMS.difference solutions problems) $
      fail $ "These solutions don't have problems: " <> show (HMS.difference problems solutions)
    return $ HMS.intersectionWith (\(k1, v1) (k2, v2) -> (k1, v1, k2, v2)) problems solutions
  where
    solutionPaths = map (\path -> replaceExtension path ".solution") problemPaths
    decodeFeatures = allFeatures

updateProblems :: Problems -> (Int -> Problem -> Problem) -> IO ()
updateProblems problems f = for_ (HMS.toList problems) $
    \(pid, (path, problem, _, _)) ->
        BL.writeFile path . encodePretty . encodeProblem allFeatures $
        f pid problem

data Options = Options
    { optionsClear :: Bool
    , optionsPaths :: [FilePath]
    } deriving (Show)

parseOptions :: OA.Parser Options
parseOptions = Options
    <$> OA.switch (OA.long "clear")
    <*> OA.some (OA.strArgument $ OA.metavar "N.problem")

main :: IO ()
main = do
    options <- OA.simpleRunParser parseOptions
    problemPaths <- concat <$> for (optionsPaths options) iterateProblems
    problems <- parseProblems problemPaths
    if optionsClear options then
        updateProblems problems $ \_ problem ->
            problem {problemBonuses = mempty}
    else do
        bonuses <- fmap head . QC.sample' $ genBonusLayer problems
        updateProblems problems $ \pid problem -> case lookup pid bonuses of
            Nothing -> problem
            Just bonus ->
                let newBonuses = problemBonuses problem <> V.singleton bonus in
                problem {problemBonuses = newBonuses}
