{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module BrainWall.Main.EstimateEpsilon
    ( main
    , estimateByCoverage
    ) where

import           BrainWall.Database           (allFeatures)
import           BrainWall.Json
import           BrainWall.Polygon
import           BrainWall.Problem
import           BrainWall.V2
import           Data.Maybe                   (isNothing)
import qualified Data.Vector                  as V
import qualified Options.Applicative.Extended as OA

allowedRotations :: Epsilon -> Figure -> Double
allowedRotations epsilon figure = sum $ do
    let steps = 360
        step = pi * 2 / fromIntegral steps
    i <- [0 :: Int .. steps - 1]
    let rot = fromIntegral i * step
        solution = rotation rot
    pure $ if allowed solution then 1.0 / fromIntegral steps else 0
  where
    -- Make up a problem.
    Just holePoly = mkPolygon $ V.fromList [V2 0 0, V2 1 0, V2 1 1, V2 0 1]
    problem = Problem
        { problemFigure  = figure
        , problemHole    = Hole holePoly
        , problemEpsilon = epsilon
        , problemBonuses = mempty
        }

    -- Rotate around center of mass.
    com :: V2 Double
    com =
        let verts = fmap fromIntegral <$> figureVertices figure
            total = V.foldl' (.+.) (V2 0 0) verts in
        if V.null verts
            then total
            else total ./ fromIntegral (V.length verts)

    rotation rot = Solution
        { solutionBonuses  = mempty
        , solutionVertices = do
            p <- fmap fromIntegral <$> figureVertices figure
            let d = p .-. com
                Polar rho phi = toPolar d :: Polar Double
                d' = fromPolar $ Polar rho (phi + rot)
            pure $ round <$> (com .+. d')
        }

    allowed sol = and $ do
        err <- validateSolution problem sol
        case err of
            EdgeLengthMismatch {} -> pure False
            _                     -> pure True

-- | Find the first value for which the supplied value returns a 'Just'.
lowerBound :: (Int -> Maybe a) -> Maybe a
lowerBound f = case f 0 of
    Just x  -> Just x
    Nothing -> findUpper 1
  where
    findUpper hi = case f hi of
        Nothing -> findUpper (hi * 2)
        Just _  -> bisect 0 hi

    bisect lo hi
        | lo + 1 >= hi      = f hi
        | isNothing (f mid) = bisect mid hi
        | otherwise         = bisect lo mid
      where
        mid = (lo + hi) `div` 2

estimateByCoverage :: Double -> Figure -> Maybe Int
estimateByCoverage coverage figure = lowerBound $ \eps ->
    if allowedRotations (fromIntegral eps) figure >= coverage then
        Just eps
    else
        Nothing

data Options = Options
    { optsEpsilon  :: !(Maybe Integer)
    , optsCoverage :: !(Maybe Double)
    , optsProblem  :: !FilePath
    } deriving (Show)

parseOptions :: OA.Parser Options
parseOptions = Options
    <$> OA.optional (OA.option OA.auto (OA.long "eps"))
    <*> OA.optional (OA.option OA.auto (OA.long "coverage"))
    <*> OA.strArgument (OA.metavar "PROBLEM")

main :: IO ()
main = do
    Options {..} <- OA.simpleRunParser parseOptions
    problem <- decodeFileWith (decodeProblem allFeatures) optsProblem
    let fig = problemFigure problem

    case optsEpsilon of
        Nothing -> pure ()
        Just eps -> putStrLn $
            "Coverage for eps=" ++ show eps ++ ": " ++
            show (allowedRotations eps fig)

    case optsCoverage of
        Nothing -> pure ()
        Just cov -> do
            putStrLn $
                "Lower bound eps for " ++ show cov ++ " rotation coverage: " ++
                maybe "?" show (estimateByCoverage cov fig)
