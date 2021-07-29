{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE RecordWildCards  #-}
module BrainWall.Main.GenerateFigure
  ( main,
    Options (..),
    defaultOptions,
    generateFigure,
    buildFigureToProblem,
    BuildFigure (..),
  )
where

import qualified BrainWall.Box          as Box
import           BrainWall.Edge
import qualified BrainWall.Edge.Slope   as Slope
import           BrainWall.Polygon
import           BrainWall.Problem
import           BrainWall.Svg
import           BrainWall.V2
import           Control.Monad          (foldM, guard)
import qualified Data.List              as L
import           Data.Maybe             (fromMaybe, listToMaybe)
import qualified Data.Vector            as V
import qualified Options.Applicative    as OA
import           System.Random.MWC      as MWC
import           System.Random.Stateful

data Options = Options
    { optionsOutPath             :: Maybe FilePath
    , optionsRadius              :: !Double
    , optionsDrag                :: !Double
    , optionsMaxN                :: !Int
    , optionsMaxDepth            :: !Int
    , optionsExtendChance        :: !Double
    , optionsIntersectMultiplier :: !Double
    , optionsReconnectChance     :: !Double
    , optionsMaxBridges          :: !Int
    } deriving (Show)

defaultOptions :: Options
defaultOptions = Options
    { optionsOutPath             = Nothing
    , optionsRadius              = 20
    , optionsDrag                = 5
    , optionsMaxN                = 5
    , optionsMaxDepth            = 6
    , optionsExtendChance        = 0.5
    , optionsIntersectMultiplier = 0.5
    , optionsReconnectChance     = 0.3
    , optionsMaxBridges          = 2
    }

data BuildFigure = BuildFigure
    { bfEdges    :: [Edge Integer]
    , bfFrontier :: [(Int, Edge Integer)]
    } deriving (Show)

newPolygon :: Double -> Int -> [Edge Double]
newPolygon radius edges = L.zipWith Edge verts $ drop 1 verts <> verts
  where
    angle = pi * 2 / fromIntegral edges
    verts = do
        i <- fromIntegral <$> [0 .. edges - 1]
        pure . fromPolar $ Polar radius (i * angle)

data Extension = Extension (V2 Double) [V2 Double] (V2 Double)

extendPolygon :: Edge Double -> Int -> Extension
extendPolygon (Edge p q) edges = Extension p verts q
  where
    side   = distance p q
    radius = side / (2 * sin (pi / fromIntegral edges))
    angle  = pi * 2 / fromIntegral edges
    pqToC  = Polar
        { polarRho = sqrt $ radius * radius - (side / 2) * (side / 2)
        , polarPhi = case toPolar (q .-. p) of
            Polar _ phi -> phi - pi / 2
        }
    c      = (p .+. q) .* 0.5 .+. fromPolar pqToC
    phi0   = case toPolar (p .-. c) of Polar _ phi -> phi
    verts  = do
        i <- fromIntegral <$> [1 .. edges - 2]
        pure $ c .+. fromPolar (Polar radius (phi0 + i * angle))

dragExtension :: StatefulGen g m => Options -> Extension -> g -> m Extension
dragExtension Options {..} (Extension p verts q) gen = do
    verts' <- go verts
    pure $ Extension p verts' q
  where
    go [] = pure []
    go (v : vs) = do
        rho <- uniformRM (0.0, optionsDrag) gen
        phi <- uniformRM (0.0, pi * 2) gen
        let v' = v .+. fromPolar (Polar rho phi)
        (v' :) <$> go vs

extensionToEdges :: Extension -> [Edge Double]
extensionToEdges (Extension p vs q) =
    let verts = p : vs ++ [q] in L.zipWith Edge verts $ drop 1 verts

-- You can think of the generated figure as a tree of polygons, branching in
-- different directions.  This function tries to reconnect branches of this
-- tree (if they are close enough, etc.) to make things a bit harder.
reconnect :: Options -> Edge Integer -> BuildFigure -> Maybe BuildFigure
reconnect Options {..} (Edge p q) bf@BuildFigure {..} = listToMaybe $ do
    -- Always connect to the frontier since that forms the border of the
    -- other branches.  Additionally this ensures we don't reconnect the
    -- same thing from two different places because we will drop `i`.
    (i, Edge r s) <- bfFrontier
    let (p', q') | squaredDistance p r < squaredDistance p s = (r, s)
                 | otherwise                                 = (s, r)

    -- These are the edges that we'll use to reconnect.
    let newEdges = [Edge p p', Edge q q']

    -- Make sure they have "about the right" length.
    guard $ all (\e ->
        let d = sqrt . fromIntegral $ edgeSquaredLength e in
        d >= optionsRadius / 2 && d <= optionsRadius * 2)
        newEdges

    -- Make sure they are on the right side of the line.
    let pToQ = Slope.fromV2 $ q .-. p
        qToP = Slope.fromV2 $ p .-. q
        pToR = Slope.fromV2 $ r .-. p
        pToS = Slope.fromV2 $ s .-. p
    guard $
        pToR `Slope.between` (qToP, pToQ) &&
        pToS `Slope.between` (qToP, pToQ)

    -- Maks sure they don't intersect with any existing edges.
    guard $ all (\e -> and $ do
        existing <- bfEdges
        pure $ case edgeIntersection e existing of
            Touching        -> True
            NotIntersecting -> True
            _               -> False)
        newEdges

    pure bf
        { bfFrontier = take i bfFrontier <> drop (i + 1) bfFrontier
        , bfEdges    = newEdges <> bfEdges
        }


step :: StatefulGen g m => Options -> BuildFigure -> g -> m BuildFigure
step _ bf@BuildFigure {..} _ | null bfFrontier = pure bf
step opts@Options {..} bf@BuildFigure {..} gen = do
    pReconnect <- uniformRM (0.0, 1.0) gen

    i <- uniformRM (0, length bfFrontier - 1) gen
    let (depth, edge) = bfFrontier !! i
        frontier = take i bfFrontier <> drop (i + 1) bfFrontier
    n <- uniformRM (3, optionsMaxN) gen

    let extension = extendPolygon (fromIntegral <$> edge) n
    extension' <- dragExtension opts extension gen
    let polygon = fmap round <$> extensionToEdges extension'
        front = map ((,) (depth + 1)) polygon <> frontier

    pExtend <- uniformRM (0.0, 1.0) gen

    let noIntersections = null $ do
            existing <- bfEdges
            new <- polygon
            guard $ case edgeIntersection existing new of
                Touching        -> False
                NotIntersecting -> False
                _               -> True
            pure existing

        accept =
            (if noIntersections
                then id else (optionsIntersectMultiplier *)) $
            optionsExtendChance

    if  | depth > optionsMaxDepth -> step opts bf {bfFrontier = frontier} gen
        | Just bf' <- reconnect opts edge bf {bfFrontier = frontier}
            , pReconnect < optionsReconnectChance -> step opts bf' gen
        -- Extend an edge with a polygon.
        | pExtend < accept ->
            step opts bf {bfEdges = polygon <> bfEdges, bfFrontier = front} gen
        | otherwise -> step opts bf {bfFrontier = frontier} gen

uniformPick :: StatefulGen g m => [a] -> g -> m a
uniformPick options gen = do
    i <- uniformRM (0, length options - 1) gen
    pure $ options !! i

addBridge :: StatefulGen g m => BuildFigure -> g -> m BuildFigure
addBridge bf@BuildFigure {..} gen = do
    p <- uniformPick [p | Edge a b <- bfEdges, p <- [a, b]] gen
    q <- uniformPick [q | Edge a b <- bfEdges, q <- [a, b]] gen
    if p == q then
        pure bf
    else
        pure bf {bfEdges = Edge p q : bfEdges}

generateFigure :: StatefulGen g m => Options -> g -> m BuildFigure
generateFigure opts@Options {..} gen = do
    n <- uniformRM (3, optionsMaxN) gen
    let polygon0 = newPolygon optionsRadius n
    bf <- step opts BuildFigure
        { bfEdges   = fmap round <$> polygon0
        , bfFrontier = ((,) 0) . fmap round <$> polygon0
        }
        gen
    numBridges <- uniformRM (0, optionsMaxBridges) gen
    foldM (\acc _ -> addBridge acc gen) bf [1 .. numBridges]

buildFigureToProblem :: BuildFigure -> Problem
buildFigureToProblem BuildFigure {..} = Problem
    { problemFigure = figure
        { figureVertices = (.-. Box.topLeft box) <$> figureVertices figure
        }
    , problemBonuses = V.empty
    , problemEpsilon = 150000
    , problemHole    = Hole .
        fromMaybe (error "buildFigureToProblem: invalid hole") $
        mkPolygon $ V.fromList
            [ V2 0 0
            , V2 w 0
            , V2 w h
            , V2 0 h
            ]
    }
  where
    figure = figureFromEdges bfEdges
    box    = fromMaybe (error "buildFigureToProblem: empty figure") $
        foldMap (Just . Box.fromV2) $ figureVertices figure
    w      = Box.width box
    h      = Box.height box

parseOptions :: OA.Parser Options
parseOptions = Options
    <$> OA.optional (OA.strOption (OA.short 'o' <> OA.metavar "OUT.svg"))
    <*> OA.option OA.auto (
            OA.long "radius" <>
            OA.value optionsRadius)
    <*> OA.option OA.auto (
            OA.long "drag" <>
            OA.value optionsDrag)
    <*> OA.option OA.auto (
            OA.long "max-n" <>
            OA.value optionsMaxN)
    <*> OA.option OA.auto (
            OA.long "max-depth" <>
            OA.value optionsMaxDepth)
    <*> OA.option OA.auto (
            OA.long "extend-chance" <>
            OA.value optionsExtendChance)
    <*> OA.option OA.auto (
            OA.long "intersect-multiplier" <>
            OA.value optionsIntersectMultiplier)
    <*> OA.option OA.auto (
            OA.long "reconnect-chance" <>
            OA.value optionsReconnectChance)
    <*> OA.option OA.auto (
            OA.long "max-bridges" <>
            OA.value optionsMaxBridges)
  where
    Options {..} = defaultOptions

parseOptionsInfo :: OA.ParserInfo Options
parseOptionsInfo = OA.info (OA.helper <*> parseOptions) $ OA.fullDesc

parseOptionsPrefs :: OA.ParserPrefs
parseOptionsPrefs = OA.prefs OA.showHelpOnError

main :: IO ()
main = do
    options <- OA.customExecParser parseOptionsPrefs parseOptionsInfo
    gen <- MWC.createSystemRandom
    fig <- generateFigure options gen
    let out = encodeSvg . problemToSvg $ buildFigureToProblem fig
    case optionsOutPath options of
        Nothing   -> putStr out
        Just path -> writeFile path out
