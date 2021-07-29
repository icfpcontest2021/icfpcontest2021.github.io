{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
module BrainWall.Main.GenerateHole
    ( Options (..)
    , defaultOptions
    , generateHole
    , Poly
    , polyToHole
    , main
    , genFigureInHole
    ) where

import qualified BrainWall.Box                   as Box
import           BrainWall.Database              (allFeatures)
import           BrainWall.Edge
import           BrainWall.Graph                 (Graph)
import qualified BrainWall.Graph                 as Graph
import           BrainWall.Graphics.Delaunay
import           BrainWall.Json
import           BrainWall.Polygon
import           BrainWall.Polygon.ContainsEdge
import           BrainWall.Polygon.ContainsPoint
import           BrainWall.Polygon.Triangulate
import           BrainWall.Problem
import           BrainWall.Svg
import           BrainWall.Triangle
import           BrainWall.V2
import           Control.Monad                   (guard, when)
import           Data.Aeson.Encode.Pretty        (encodePretty)
import qualified Data.ByteString.Lazy            as BL
import qualified Data.ByteString.Lazy.Char8      as BL8
import qualified Data.List                       as L
import           Data.Maybe                      (fromMaybe, isNothing)
import           Data.Ord                        (comparing)
import           Data.Traversable                (for)
import qualified Data.Vector                     as V
import qualified Options.Applicative             as OA
import           System.Exit                     (exitFailure)
import qualified System.IO                       as IO
import           System.Random.MWC               as MWC
import           System.Random.Stateful

data Options hole = Options
    { -- Width and height of the hole.  Ignored if `optionsAroundHole` is set.
      optionsWidth       :: !Integer
    , optionsHeight      :: !Integer
    , -- | Minimum distance in between points is:
      --
      -- (width + height) * spacing
      --
      -- You should set this lower when using `optionsAroundHole`.
      optionsSpacing     :: !Double
    , optionsIterations  :: !Int
    , optionsAroundHole  :: !(Maybe hole)
    , optionsFigure      :: !FigureOptions
    , optionsOutProblem  :: !(Maybe FilePath)
    , optionsOutSvg      :: !(Maybe FilePath)
    , optionsTriangulate :: !Bool
    } deriving (Show)

data FigureOptions = FigureOptions
    { figureOptionsEnable            :: !Bool
    , figureOptionsEdgeP             :: !Double
    , figureOptionsIntersectingEdgeP :: !Double
    } deriving (Show)

defaultOptions :: Options a
defaultOptions = Options
    { optionsWidth       = 100
    , optionsHeight      = 100
    , optionsSpacing     = 0.5 * (1 / 7)
    , optionsIterations  = 1000
    , optionsAroundHole  = Nothing
    , optionsFigure      = FigureOptions
        { figureOptionsEnable            = False
        , figureOptionsEdgeP             = 0.5
        , figureOptionsIntersectingEdgeP = 0.005
        }
    , optionsOutProblem  = Nothing
    , optionsOutSvg      = Nothing
    , optionsTriangulate = False
    }

data Step = Split | Move deriving (Show)

type Poly = V.Vector (V2 Integer)

frequency :: StatefulGen g m => g -> [(Int, m a)] -> m a
frequency gen choices = do
  idx <- uniformRM (0, n - 1) gen
  go idx choices
  where
    n = sum $ map fst choices
    go _ []                 = error "impossible"
    go idx ((weight, x):xs) = if idx < weight then x else go (idx - weight) xs

step :: StatefulGen g m => Options Hole -> Poly -> g -> m (Maybe Poly)
step Options {..} verts gen = frequency gen $
    -- Split an existing line.
    [ (3, do
        i <- uniformRM (0, V.length verts - 1) gen
        let Edge p q = edge verts i
            r = (`div` 2) <$> (p .+. q)
            verts' =
                V.take (i + 1) verts <> V.singleton r <>
                V.drop (i + 1) verts
            j = (i + 1) `mod` V.length verts'
        pure $ do
            guard $ validPoint verts' (i + 1)
            guard $ validEdge verts' i && validEdge verts' j
            pure verts')

    -- Pick a point and move it around.
    , (5, do
        i <- uniformRM (0, V.length verts - 1) gen
        let (V2 x y) = verts V.! i
        dx <- uniformRM (-optionsWidth  `div` 10, optionsWidth  `div` 10) gen
        dy <- uniformRM (-optionsHeight `div` 10, optionsHeight `div` 10) gen
        let x' = max 0 . min optionsWidth  $ x + dx
            y' = max 0 . min optionsHeight $ y + dy
        let verts' = V.update verts $ V.singleton (i, V2 x' y')
            j = if i == 0 then V.length verts' - 1 else i - 1
        pure $ do
            guard $ validPoint verts' i
            guard $ validEdge verts' j && validEdge verts' i
            guard $ case optionsAroundHole of
                Nothing -> True
                Just (Hole poly) ->
                    -- We don't want to do this check when splitting.
                    (not $ containsEdge (edge verts' i) poly) &&
                    (not $ containsEdge (edge verts' j) poly)
            pure verts')
    ]
  where
    edge v i = Edge (v V.! i) (v V.! ((i + 1) `mod` V.length v))

    validPoint :: V.Vector (V2 Integer) -> Int -> Bool
    validPoint vs i =
        let p = vs V.! i in
        V.and (V.imap (\j q -> if j == i then True else p /= q) vs) &&
        (case optionsAroundHole of
            Nothing          -> True
            Just (Hole poly) ->
                V.any (pointOnEdge p) (polygonEdges poly) ||
                not (pointInPolygon p poly))

    validEdge :: V.Vector (V2 Integer) -> Int -> Bool
    validEdge vs i = noIntersect && goodSpacing && noIntersectAroundHole
      where
        noIntersect = V.and $ do
            j <- V.enumFromN 0 (V.length vs)
            guard $ i /= j
            pure $ case edgeIntersection (edge vs i) (edge vs j) of
                Intersecting    -> False
                Overlapping     -> False
                Touching        -> True
                NotIntersecting -> True
        goodSpacing = V.and $ do
            let ie@(Edge ip iq) = edge vs i
            j <- V.enumFromN 0 (V.length vs)
            let je@(Edge jp jq) = edge vs j
            guard . not $ edgeConnected ie je
            pure $
                edgePointSquaredDistance jp ie >= spacingTreshold &&
                edgePointSquaredDistance jq ie >= spacingTreshold &&
                edgePointSquaredDistance ip je >= spacingTreshold &&
                edgePointSquaredDistance iq je >= spacingTreshold
        noIntersectAroundHole = case optionsAroundHole of
            Nothing -> True
            Just (Hole poly) -> V.and $ do
                holeEdge <- polygonEdges poly
                pure $ case edgeIntersection (edge vs i) holeEdge of
                    Intersecting    -> False
                    Overlapping     -> True
                    Touching        -> True
                    NotIntersecting -> True

    spacing = fromIntegral (optionsWidth + optionsHeight) * optionsSpacing
    spacingTreshold = fromIntegral (round $ spacing * spacing :: Integer)

generateHole :: StatefulGen g m => Options Hole -> g -> m Poly
generateHole opts gen = do
    let polygon0 = case optionsAroundHole opts of
            Nothing          -> V.fromList [V2 0 0, V2 w 0, V2 w h, V2 0 h]
            Just (Hole poly) -> polygonVertices poly
    go 0 polygon0
  where
    w = optionsWidth opts
    h = optionsHeight opts
    go i !polygon
        | i >= optionsIterations opts = pure polygon
        | otherwise                   = do
            polygon' <- step opts polygon gen
            go (i + 1) (fromMaybe polygon polygon')

polyToHole :: Poly -> Maybe Hole
polyToHole poly = mkPolygon poly >>= fmap Hole . polygonSimplify

suchThat :: Monad m => m a -> (a -> Bool) -> m a
suchThat go good = do
  a <- go
  if good a then pure a else suchThat go good

genV2InTriangle :: StatefulGen g m => Triangle Integer -> g -> m (V2 Integer)
genV2InTriangle (Triangle p q r) gen = (`suchThat` inTriangle) $ do
    let p'    = fromIntegral <$> p :: V2 Double
        pToQ' = (fromIntegral <$> q) .-. p'
        pToR' = (fromIntegral <$> r) .-. p'
    u <- uniformRM (0, 1) gen
    t <- uniformRM (0, 1) gen
    pure . fmap round $ p' .+. pToQ' .* u .+. pToR' .* t
  where
    inTriangle point = case mkPolygon (V.fromList [p, q, r]) of
        Nothing   -> error "genV2InTriangle: empty triangle?"
        Just poly -> pointInPolygon point poly

genPointsInHole :: StatefulGen g m => Hole -> g -> m [V2 Integer]
genPointsInHole (Hole polygon) gen = (++)
    <$> for triangles (\_ -> frequency gen byArea)
    <*> for triangles (flip genV2InTriangle gen)
  where
    triangles = delaunay .
        fromMaybe (error "genPointsInHole: no triangulation?") $
        earCutting polygon

    byArea = do
        triangle <- triangles
        let area = triangleArea $ fromIntegral <$> triangle :: Double
        pure (round area, genV2InTriangle triangle gen)

connectPointsInHole
    :: StatefulGen g m => FigureOptions -> Hole -> [V2 Integer] -> g -> m (Graph (V2 Integer))
connectPointsInHole FigureOptions {..} (Hole polygon) points gen =
    fmap Graph.undirectedFromEdges .
    fmap (map (\case Edge p q -> (p, q))) . pick [] $
    L.sortOn edgeSquaredLength [Edge p q | p <- points, q <- points, p < q]
  where
    pick edges [] = pure edges
    pick edges (edge : es)
        | not $ containsEdge edge polygon = pick edges es
        | otherwise                       = do
            let accept
                    | intersects = figureOptionsIntersectingEdgeP
                    | otherwise  = figureOptionsEdgeP
            p <- uniformRM (0, 1 :: Double) gen
            if p < accept then pick (edge : edges) es else pick edges es
      where
        intersects = or $ do
            other <- edges
            pure $ case edgeIntersection other edge of
                Intersecting    -> True
                Overlapping     -> True
                Touching        -> False
                NotIntersecting -> False

genFigureInHole :: StatefulGen g m => FigureOptions -> Hole -> g -> m Figure
genFigureInHole opts hole gen = do
    points <- genPointsInHole hole gen
    graph <- connectPointsInHole opts hole points gen
    let graph' = case Graph.components graph of
         [] -> graph
         cs -> L.maximumBy (comparing Graph.numVertices) cs
    pure . figureFromEdges . fmap (uncurry Edge) $ Graph.toUniqueEdges graph'

parseOptions :: OA.Parser (Options FilePath)
parseOptions = Options
    <$> OA.option OA.auto (
            OA.long "width" <>
            OA.value optionsWidth)
    <*> OA.option OA.auto (
            OA.long "height" <>
            OA.value optionsHeight)
    <*> OA.option OA.auto (
            OA.long "spacing" <>
            OA.value optionsSpacing)
    <*> OA.option OA.auto (
            OA.long "iterations" <>
            OA.value optionsIterations)
    <*> OA.optional (OA.strOption $
            OA.long "around-hole" <>
            OA.metavar "PROBLEM.problem")
    <*> (FigureOptions
            <$> OA.switch (
                    OA.long "figure")
            <*> OA.option OA.auto (
                    OA.long "figure-edge-p" <>
                    OA.value figureOptionsEdgeP)
            <*> OA.option OA.auto (
                    OA.long "figure-intersecting-edge-p" <>
                    OA.value figureOptionsIntersectingEdgeP))
    <*> OA.optional (OA.strOption $
            OA.short 'o' <>
            OA.metavar "PROBLEM.problem")
    <*> OA.optional (OA.strOption $
            OA.long "svg" <>
            OA.metavar "PROBLEM.svg")
    <*> OA.switch (
            OA.long "triangulate")
  where
    Options {..} = defaultOptions
    FigureOptions {..} = optionsFigure

parseOptionsInfo :: OA.ParserInfo (Options FilePath)
parseOptionsInfo = OA.info (OA.helper <*> parseOptions) $ OA.fullDesc

parseOptionsPrefs :: OA.ParserPrefs
parseOptionsPrefs = OA.prefs OA.showHelpOnError

main :: IO ()
main = do
    options0 <- OA.customExecParser parseOptionsPrefs parseOptionsInfo

    aroundHoleProblem <- case optionsAroundHole options0 of
        Nothing   -> pure Nothing
        Just path -> Just <$> decodeFileWith (decodeProblem allFeatures) path
    let aroundHole = problemHole <$> aroundHoleProblem
        aroundHoleBox = problemBox <$> aroundHoleProblem
        options = options0
            { optionsAroundHole = aroundHole
            , optionsWidth      =
                maybe (optionsWidth options0) Box.width $ aroundHoleBox
            , optionsHeight     =
                maybe (optionsHeight options0) Box.height aroundHoleBox
            }

    gen <- MWC.createSystemRandom
    poly <- generateHole options gen
    Hole holePolygon <- case polyToHole poly of
        Just p  -> pure p
        Nothing -> do
            IO.hPutStrLn IO.stderr $ "error: no polygon"
            exitFailure

    let problemHole = Hole holePolygon

    problemFigure <- case figureOptionsEnable (optionsFigure options) of
        False -> pure $ Figure mempty mempty
        True  ->
            genFigureInHole (optionsFigure options) problemHole gen

    let problemEpsilon = 150000
        problemBonuses = mempty
        problem = Problem {..}

        problemSvg = problemToSvgWith
            defaultProblemToSvgSettings {ptssFillHole = Just "#444"}
            problem

        holeTriangulation = delaunay <$> earCutting holePolygon

        holeTriangleElement = case holeTriangulation of
            Nothing -> Nothing
            Just triangles -> Just . makeElement . GroupElement $ do
                t <- triangles
                pure . edgesToSvgElement $ triangleEdges t

    when (isNothing holeTriangleElement) $
        IO.hPutStrLn IO.stderr "warning: no triangulation"

    case holeTriangulation of
        Nothing -> pure ()
        Just triangles ->
            let holeArea, canvasArea :: Double
                holeArea = sum $ triangleArea . fmap fromIntegral <$> triangles
                canvasArea = fromIntegral $
                    optionsWidth options * optionsHeight options in
            IO.hPutStrLn IO.stderr $
                "Area covered by hole: " ++ show (holeArea / canvasArea)

    let svg = encodeSvg $ problemSvg
            { svgElements =
                (case holeTriangleElement of
                    Just el | optionsTriangulate options ->
                        [el {elementStyle = Just "stroke:#aaa"}]
                    _ -> []) ++
                (case optionsAroundHole options of
                    Nothing -> []
                    Just h -> pure . setElementStyle "stroke:#44b;fill:none" .
                        polygonToSvgElement $ unHole h) ++
                svgElements problemSvg
            }

        out = encodePretty $ encodeProblem allFeatures problem

    case optionsOutSvg options of
        Nothing   -> pure ()
        Just path -> writeFile path svg

    case optionsOutProblem options of
        Nothing -> BL8.putStrLn out
        Just p  -> BL.writeFile p out
