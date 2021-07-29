{-# LANGUAGE RecordWildCards #-}
module BrainWall.Main.Animate
    ( makeAnimation
    , main
    ) where

import qualified BrainWall.Box           as Box
import           BrainWall.Database      (allFeatures)
import           BrainWall.Edge
import           BrainWall.Json
import           BrainWall.Polygon
import           BrainWall.Problem
import           BrainWall.Svg
import           BrainWall.V2
import qualified BrainWall.Web.Assets    as Assets
import           Control.Monad           (guard)
import qualified Data.List               as List
import           Data.List.NonEmpty      (NonEmpty)
import           Data.Maybe              (fromMaybe)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Vector             as V
import qualified Options.Applicative     as OA
import qualified Text.XML.Light.Extended as Xml

-- | This is the box that holds the problem.  A lot of sizing is based around
-- this, so we pass it to a lot of functions.
type PBox = Box.Box Integer

judgingElement
    :: PBox -> Either (NonEmpty String) Dislikes -> Element Double
judgingElement pbox dislikes =
    (makeElement $ GroupElement
        [ dislikesElement (judgesW, judgesH) dislikes
        , judgesElement
        ])
        { elementTransform =
            [ Translate $ V2 (v2X pos) (v2Y pos - judgesH)
            , SkewY (90 - approachSkewX)
            ]
        }
  where
    (judgesElement, judgesW, judgesH) = judges pbox

    pos = applyTransform
        (transformFromCommand (SkewY problemSkewY))
        (V2 (problemWidth pbox) problemHeight) .+.
        V2 margin margin .+.
        V2 (problemHeight * 0.3)
            (problemSkewMarginTop pbox + problemHeight * 0.1)

judges :: PBox -> (Element Double, Double, Double)
judges _cvs = case Xml.parseXMLDoc . T.unpack $ T.decodeUtf8 Assets.judges_svg of
    Nothing -> error "judges: invalid xml"
    Just xml -> case parseSvg xml of
        Left err -> error $ "judges: invalid svg: " ++ err
        Right svg ->
            let actualH = svgHeight svg :: Double
                desiredW = svgWidth svg * (desiredH / actualH)
                embed = makeElement . RawXml .
                    Xml.set_attr (Xml.Attr (Xml.unqual "width") (show desiredW)) $
                    Xml.set_attr (Xml.Attr (Xml.unqual "height") (show desiredH))
                    xml in
            ( makeElement $ GroupElement [embed]
            , desiredW
            , desiredH
            )
  where
    desiredH = problemHeight * 0.6

dislikesElement
    :: (Double, Double)
    -> Either (NonEmpty String) Dislikes -> Element Double
dislikesElement (judgesW, judgesH) dislikes =
    let textPosition = V2 (judgesW * 0.4) (judgesH * 0.7)
        textContent = dislikesText
        textCenter = True

        dislikesText = case dislikes of
            Left _  -> "boo"
            Right x -> show x

        boardRise = -60 :: Double
        boardWidth = max 40 $ 10 * fromIntegral (length dislikesText)
        boardHeight = 14
        boardPos = textPosition .-. V2 boardWidth boardHeight ./ 2

        board = (makeElement $ GroupElement
            [ (makeElement $ RectElement boardPos boardWidth boardHeight)
                { elementStyle = Just "stroke: black; fill: lightsalmon"
                }
            , (makeElement $ TextElement Text {..})
                { elementStyle = Just "font-size: 12px"
                }
            ])
            { elementAnimate = pure $ AnimateTransform
                { animationType        = "translate"
                , animationDur         = animationDuration
                , animationKeyTimes    = Just $ "0;" <> wallHitKeyTime <> ";1"
                , animationValues      = Just $
                    "0,0;0,0;0," <> show boardRise <> ";"
                , animationRepeatCount = Just "indefinite"
                }
            }

        stickWidth = 5
        stickPos@(V2 _ stickY) = textPosition .-. V2 (stickWidth / 2) 0
        stick = (makeElement $ RectElement stickPos stickWidth 0)
            { elementStyle = Just "stroke: black; fill: lightsalmon"
            , elementAnimate =
                [ Animate
                    { animationAttributeName = "y"
                    , animationBegin         = Nothing
                    , animationDur           = animationDuration
                    , animationKeyTimes      = Just $ "0;" <> wallHitKeyTime <> ";1"
                    , animationTo            = Nothing
                    , animationValues        = Just $ show stickY <> ";" <>
                        show stickY <> ";" <> show (stickY + boardRise)
                    , animationRepeatCount   = Just "indefinite"
                    }
                , Animate
                    { animationAttributeName = "height"
                    , animationBegin         = Nothing
                    , animationDur           = animationDuration
                    , animationKeyTimes      = Just $ "0;" <> wallHitKeyTime <> ";1"
                    , animationTo            = Nothing
                    , animationValues        = Just $ "0;0;" <> show (-boardRise)
                    , animationRepeatCount   = Just "indefinite"
                    }
                ]
            } in

    makeElement $ GroupElement [stick, board]

edgesToAnimatedSvgElement
    :: Show a => String -> String -> [(Edge a, Edge a)] -> Element a
edgesToAnimatedSvgElement begin end edgePairs =
    makeElement . GroupElement $ do
    (Edge p q, Edge p' q') <- edgePairs
    -- <animate attributeName="points" dur="500ms" to=" ... shape 2 points ... " />
    let lineEl = makeElement $ LineElement p q
    pure $ lineEl
        { elementAnimate =
            [ animateTo "x1" (v2X p) (v2X p')
            , animateTo "y1" (v2Y p) (v2Y p')
            , animateTo "x2" (v2X q) (v2X q')
            , animateTo "y2" (v2Y q) (v2Y q')
            ]
        }
  where
    animateTo attr from to = Animate
        { animationAttributeName = attr
        , animationBegin         = Nothing
        , animationDur           = animationDuration
        , animationKeyTimes      = Just $ "0;" <> begin <> ";" <> end <> ";1"
        , animationTo            = Nothing
        , animationValues        = Just $ List.intercalate ";" $ map show
            [from, from, to, to]
        , animationRepeatCount   = Just "indefinite"
        }

animationDuration :: String
animationDuration = "10s"

wallHitKeyTime :: String
wallHitKeyTime = "0.4"

wallToFigure :: V2 Double
wallToFigure = V2 60 40

wallToEnd :: V2 Double
wallToEnd = V2 150 100

figureToEnd :: V2 Double
figureToEnd = wallToEnd .-. wallToFigure

problemMargin :: Double
problemMargin = 5

problemHeight :: Double
problemHeight = 100 + problemMargin * 2

problemScale :: PBox -> Double
problemScale pbox =
    (problemHeight - problemMargin * 2) / fromIntegral (Box.height pbox)

problemWidth :: PBox -> Double
problemWidth pbox =
    fromIntegral (Box.width pbox) * problemScale pbox + problemMargin * 2

-- Map a problem vertex to the scaled space
problemMap :: Box.Box Integer -> V2 Integer -> V2 Double
problemMap pbox p =
    fmap fromIntegral (p .-. Box.topLeft pbox) .* problemScale pbox .+.
    V2 problemMargin problemMargin

problemSkewY :: Double
problemSkewY = -10

problemSkewMarginTop :: PBox -> Double
problemSkewMarginTop pbox = abs $ v2Y $ applyTransform
    (transformFromCommand $ SkewY problemSkewY)
    (V2 (problemWidth pbox) 0)

planetFill :: String
planetFill = "#c3dbc3"

approachSkewX :: Double
approachSkewX = 90 - polarDegrees (toPolar wallToEnd)

margin :: Double
margin = 10

totalWidth :: PBox -> Double
totalWidth pbox = problemWidth pbox + v2X wallToEnd + margin * 2

totalHeight :: PBox -> Double
totalHeight pbox =
    problemHeight + v2Y wallToEnd + problemSkewMarginTop pbox + margin * 2

wallElement
    :: PBox -> Hole -> [(Bonus, Bool)] -> Element Double
wallElement pbox hole bonuses = Element
    { elementId        = Just "hole"
    , elementTransform =
        [Translate (V2 margin (margin + problemSkewMarginTop pbox))]
    , elementClipPath  = Nothing
    , elementClipRule  = Nothing
    , elementStyle     = Nothing
    , elementAnimate   =
        [ AnimateTransform
            { animationType        = "translate"
            , animationDur         = animationDuration
            , animationValues      = Just $
                "0,0;" <> show (v2X wallToEnd) <> "," <> show (v2Y wallToEnd)
            , animationKeyTimes    = Nothing
            , animationRepeatCount = Just "indefinite"
            }
        , AnimateTransform
            { animationType        = "skewY"
            , animationDur         = animationDuration
            , animationValues      = Just $
                show problemSkewY <> ";" <> show problemSkewY
            , animationKeyTimes    = Nothing
            , animationRepeatCount = Just "indefinite"
            }
        ]
    , elementGuts      = GroupElement $
        [ setElementStyle "fill:#fc6192;fill-rule:evenodd;" .
            makeElement . PathElement $
            [MoveTo True (verts V.! 0)] ++
            map (LineTo True) (V.toList $ V.drop 1 verts) ++ [Close] ++
            [ MoveTo True (V2 0 0)
            , LineTo True (V2 problemW 0)
            , LineTo True (V2 problemW problemHeight)
            , LineTo True (V2 0 problemHeight)
            , Close
            ]
        , setElementStyle "stroke:black;fill:none" . makeElement $
            RectElement (V2 0 0) problemW problemHeight
        ] ++
        [ addElementAnimate Animate
            { animationAttributeName = "r"
            , animationBegin         = Nothing
            , animationDur           = animationDuration
            , animationKeyTimes      = Just $ "0;" <> wallHitKeyTime <> ";1"
            , animationTo            = Nothing
            , animationValues        = Just $ List.intercalate ";" $ map show
                [bonusSize, bonusSize, if bonusOk then bonusSize * 2 else 0]
            , animationRepeatCount   = Just "indefinite"
            } $
            setElementStyle ("fill:" <> bonusColor bonusBonus) . makeElement $
            CircleElement (problemMap pbox bonusPosition) bonusSize
        | (Bonus {..}, bonusOk) <- bonuses
        ]
    }
  where
    verts = problemMap pbox <$> unPolygon (unHole hole)
    problemW = problemWidth pbox
    bonusSize = 0.03 * (problemW + problemHeight) / 2

sweepElement :: PBox -> (Element Double, Element Double)
sweepElement canvas =
    ( (makeElement $ GroupElement [approach, pool, rline])
        { elementTransform = transform
        }
    , (makeElement $ GroupElement [leftFloor, downFloor, waterOverlay, lline])
        { elementTransform = transform
        }
    )
  where
    transform =
        [ Translate (V2
            margin
            (margin + problemSkewMarginTop canvas + problemHeight))
        , skewXY approachSkewX problemSkewY
        ]

    lline = setElementStyle "stroke:black" . makeElement $ LineElement
        (V2 0 0) (V2 0 (v2Y wallToEnd))
    rline = setElementStyle "stroke:black" . makeElement $ LineElement
        (V2 (problemWidth canvas) 0) (V2 (problemWidth canvas) (v2Y wallToEnd))

    approach = setElementStyle "fill:#cba136" . makeElement $ RectElement
        (V2 0 0) (problemWidth canvas) (v2Y wallToFigure)

    pool = setElementStyle "fill:#00ffff" . makeElement $ RectElement
        (V2 0 (v2Y wallToFigure)) (problemWidth canvas) (v2Y figureToEnd)

    leftFloor = setElementStyle ("fill:" <> planetFill) . makeElement $
        RectElement (V2 (-problemWidth canvas) 0)
            (problemWidth canvas) (v2Y wallToEnd)

    downFloor = setElementStyle ("fill:" <> planetFill) . makeElement $
        RectElement (V2 (-problemWidth canvas) (v2Y wallToEnd))
            (problemWidth canvas * 2) (v2Y wallToEnd)

    waterOverlayH = v2Y figureToEnd
    waterOverlay = setElementStyle "fill:#00ffffbb" $
        addElementAnimate AnimateTransform
            { animationType        = "translate"
            , animationDur         = animationDuration
            , animationKeyTimes    = Just $
                "0;" <> wallHitKeyTime <> ";1"
            , animationValues      = Just $
                "0,0;0,0;0," <> show (v2Y figureToEnd)
            , animationRepeatCount = Just "indefinite"
            } $
        addElementAnimate Animate
            { animationAttributeName = "height"
            , animationBegin         = Nothing
            , animationDur           = animationDuration
            , animationKeyTimes      = Just $ "0;" <> wallHitKeyTime <> ";1"
            , animationTo            = Nothing
            , animationValues        = Just . List.intercalate ";" $
                map show [waterOverlayH, waterOverlayH, 0]
            , animationRepeatCount   = Just "indefinite"
            } $
        makeElement $ RectElement
            (V2 0 (v2Y wallToFigure)) (problemWidth canvas) waterOverlayH

makeAnimation
    :: Problem -> Solution -> Maybe (Either (NonEmpty String) Integer)
    -> Svg Double
makeAnimation problem@(Problem hole figure0 _ _) solution mbJudgement =
    (makeSvg viewBox)
        { svgElements =
            [ (makeElement $ ClipPathElement
                    [makeElement $ RectElement (V2 0 0) viewW viewH])
                { elementId = Just "view-clip"
                }
            , (makeElement $ GroupElement content)
                { elementClipPath = Just "url(#view-clip)"
                }
            ]
        }
  where
    figure = case solutionBrokenLeg problem solution of
        OkBrokenLeg leg _ _ | Just (Edge p q) <- lookupFigureEdge leg figure0 ->
            solutionFigure problem $ solution
                { solutionVertices = figureVertices figure0 <>
                    V.fromList [(`div` 2) <$> (p .+. q)]
                }
        _ -> figure0

    figure' = solutionFigure problem solution

    content =
        [ sky, planet ] ++
        [ sweep ] ++
        [ judgingElement pbox judgement ] ++
        [ figureReform | not drowning ] ++
        [ wallElement pbox hole
            (awardBonusesForSolution problem solution) ] ++
        [ figureReform | drowning ] ++
        [ figureDeform ] ++
        [ waterOverlay ]
    (sweep, waterOverlay) = sweepElement pbox

    judgement = fromMaybe (judgeSolution problem solution) mbJudgement

    drowning = case judgement of
        Left  _ -> True
        Right _ -> False

    problemMapEdge (Edge p q) = Edge (problemMap pbox p) (problemMap pbox q)

    scaledFigureEdges, scaledFigureEdges' :: [Edge Double]
    scaledFigureEdges = foldFigureEdges (pure . problemMapEdge) figure
    scaledFigureEdges' = foldFigureEdges (pure . problemMapEdge) figure'

    sky = setElementStyle "fill:skyblue" $ makeElement $ RectElement
        (V2 0 0) (totalWidth pbox) (totalHeight pbox)

    planet = setElementStyle ("fill:" <> planetFill) $
        makeElement $ CircleElement
        (V2 (totalWidth pbox * 0.5) (2 * totalHeight pbox))
        (1.9 * totalHeight pbox)

    figureDeform :: Element Double
    figureDeform = el
        { elementStyle     = Just
            "fill:none;stroke:#ff0000"
        , elementTransform = [Translate (V2 x y), SkewY problemSkewY]
        , elementAnimate   = Animate
            { animationAttributeName = "display"
            , animationBegin         = Nothing
            , animationDur           = animationDuration
            , animationKeyTimes      = Just $ "0;" <> wallHitKeyTime <> "; 1"
            , animationTo            = Nothing
            , animationValues        = Just "inline; none; none"
            , animationRepeatCount   = Just "indefinite"
            } : elementAnimate el
        }
      where
        V2 x y = wallToFigure .+. V2 0 (problemSkewMarginTop pbox) .+.
            V2 margin margin
        el = edgesToAnimatedSvgElement "0" wallHitKeyTime $ zip
            scaledFigureEdges
            scaledFigureEdges'

    figureReform :: Element Double
    figureReform = el
        { elementStyle     = Just
            "fill:none;stroke:#ff0000"
        , elementTransform = [Translate (V2 x y), SkewY problemSkewY]
        , elementAnimate   =
            [ Animate
                { animationAttributeName = "display"
                , animationBegin         = Nothing
                , animationDur           = animationDuration
                , animationKeyTimes      = Just $ "0;" <> wallHitKeyTime <> ";1"
                , animationTo            = Nothing
                , animationValues        = Just "none; inline; inline"
                , animationRepeatCount   = Just "indefinite"
                }
            ] ++
            (guard drowning >>
                let drownSkew = 30 :: Double in
                [ AnimateTransform
                    { animationType        = "translate"
                    , animationDur         = animationDuration
                    , animationKeyTimes    = Just $
                        "0;" <> wallHitKeyTime <> ";1"
                    , animationValues      = Just $
                        let finalY = v2Y figureToEnd + problemHeight +
                                problemSkewMarginTop pbox in
                        "0,0;0,0;" <> show (v2X figureToEnd) <> "," <>
                        show finalY
                    , animationRepeatCount = Just "indefinite"
                    }
                    -- Purely to compensate for the skew
                    , AnimateTransform
                    { animationType        = "translate"
                    , animationDur         = animationDuration
                    , animationKeyTimes    = Just $
                        "0;" <> wallHitKeyTime <> ";1"
                    , animationValues      = Just $
                        "0,0;0,0;" ++ show drownSkew ++ ",0"
                    , animationRepeatCount = Just "indefinite"
                    }
                    , AnimateTransform
                    { animationType        = "skewX"
                    , animationDur         = animationDuration
                    , animationKeyTimes    = Just $
                        "0;" <> wallHitKeyTime <> ";1"
                    , animationValues      = Just . List.intercalate ";" $
                        map show [0, 0, -drownSkew]
                    , animationRepeatCount = Just "indefinite"
                    }
                ]) ++
            elementAnimate el
        }
      where
        V2 x y = wallToFigure .+. V2 0 (problemSkewMarginTop pbox) .+.
            V2 margin margin
        el = edgesToAnimatedSvgElement wallHitKeyTime "1" $ zip
            scaledFigureEdges'
            scaledFigureEdges
    {-
    judgesElement :: Element Integer
    judgesElement = el
        { elementTransform = [Translate x y]
        }
      where
        V2 x y = wallToFigure canvas
        el = makeElement $ GroupElement [judges canvas]
    -}
    pbox = problemBox problem
    viewW = totalWidth pbox
    viewH = totalHeight pbox
    viewBox = (0, 0, viewW, viewH)

data Options = Options
    { optionsOutPath      :: Maybe FilePath
    , optionsProblemPath  :: FilePath
    , optionsSolutionPath :: FilePath
    }

parseOptions :: OA.Parser Options
parseOptions = Options
    <$> OA.optional (OA.strOption (OA.short 'o' <> OA.metavar "OUT.svg"))
    <*> OA.strArgument (OA.metavar "PROBLEM")
    <*> OA.strArgument (OA.metavar "SOLUTION")

parseOptionsInfo :: OA.ParserInfo Options
parseOptionsInfo = OA.info (OA.helper <*> parseOptions) OA.fullDesc

parseOptionsPrefs :: OA.ParserPrefs
parseOptionsPrefs = OA.prefs OA.showHelpOnError

main :: IO ()
main = do
    options <- OA.customExecParser parseOptionsPrefs parseOptionsInfo
    problem <- decodeFileWith
        (decodeProblem allFeatures) (optionsProblemPath options)
    solution <- decodeFileWith
        (decodeSolution allFeatures) (optionsSolutionPath options)
    let out = encodeSvg $ makeAnimation problem solution Nothing
    case optionsOutPath options of
        Nothing   -> putStr out
        Just path -> writeFile path out
