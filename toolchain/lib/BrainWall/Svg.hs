{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module BrainWall.Svg
    ( ViewBox
    , makeViewBox

    , Element (..)
    , ElementGuts (..)
    , makeElement
    , setElementStyle
    , addElementAnimate
    , addElementTransform

    , TransformCommand (..)
    , skewXY

    , PathCommand (..)

    , Animation (..)

    , Image (..)
    , Text (..)

    , Svg (..)
    , makeSvg
    , parseSvg
    , readSvg
    , renderSvg
    , encodeSvg
    , svgSetLongEdge

    , ProblemToSvgSettings (..)
    , defaultProblemToSvgSettings
    , problemToSvgWith
    , problemToSvg

    , edgesToSvgElement
    , polygonToSvgElement
    , problemFromSvg
    , polygonFromSvg

    , Transform
    , transformFromCommand
    , applyTransform

    , bonusColor
    ) where

import qualified BrainWall.Box              as Box
import           BrainWall.Edge
import           BrainWall.Polygon
import           BrainWall.Polygon.Internal
import           BrainWall.Problem
import           BrainWall.V2
import           Control.Applicative        ((<|>))
import           Control.Monad              (unless, when)
import           Control.Monad.State        (State, execState, gets, modify,
                                             state)
import qualified Data.Attoparsec.Text       as AP
import           Data.Char                  (isAlpha, isSpace, isUpper, toLower)
import           Data.Foldable              (for_)
import qualified Data.List                  as List
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.List.NonEmpty         as NonEmpty
import qualified Data.Map                   as Map
import           Data.Maybe                 (catMaybes, fromMaybe, maybeToList)
import qualified Data.Set                   as Set
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import qualified Linear.Matrix              as L
import qualified Linear.V3                  as L
import           Text.Read                  (readMaybe)
import qualified Text.XML.Light             as Xml

type ViewBox a = (a, a, a, a)

makeViewBox :: Integer -> [V2 Integer] -> ViewBox Integer
makeViewBox _      []       = (0, 0, 1, 1)
makeViewBox margin vertices =
    (minX - margin, minY - margin, width + margin * 2, height + margin * 2)
  where
    minX     = minimum $ map v2X vertices
    maxX     = maximum $ map v2X vertices
    minY     = minimum $ map v2Y vertices
    maxY     = maximum $ map v2Y vertices
    width    = maxX - minX
    height   = maxY - minY

data Svg a = Svg
    { svgViewBox  :: ViewBox a
    , svgWidth    :: a
    , svgHeight   :: a
    , svgElements :: [Element a]
    } deriving (Functor, Show)

data Element a = Element
    { elementId        :: Maybe String
    , elementStyle     :: Maybe String
    , elementClipPath  :: Maybe String
    , elementClipRule  :: Maybe String
    , elementTransform :: [TransformCommand a]
    , elementAnimate   :: [Animation]
    , elementGuts      :: ElementGuts a
    } deriving (Functor, Show)

data ElementGuts a
    = GroupElement    [Element a]
    | ClipPathElement [Element a]
    | PathElement     [PathCommand a]
    | LineElement     !(V2 a) !(V2 a)
    | RectElement     !(V2 a) !a !a
    | CircleElement   !(V2 a) !a
    | ImageElement    !(Image a)
    | TextElement     !(Text a)
    | RawXml          !Xml.Element
    deriving (Functor, Show)

data Image a = Image
    { imagePosition :: !(Maybe (V2 a))
    , imageWidth    :: !(Maybe a)
    , imageHeight   :: !(Maybe a)
    , imageHref     :: !String
    } deriving (Functor, Show)

data Text a = Text
    { textPosition :: !(V2 a)
    , textContent  :: !String
    , textCenter   :: !Bool
    } deriving (Functor, Show)

data Animation
    = Animate
        { animationAttributeName :: !String
        , animationBegin         :: !(Maybe String)
        , animationDur           :: !String
        , animationKeyTimes      :: !(Maybe String)
        , animationTo            :: !(Maybe String)
        , animationValues        :: !(Maybe String)
        , animationRepeatCount   :: !(Maybe String)
        }
    | AnimateTransform
        { animationType        :: !String
        , animationDur         :: !String
        , animationKeyTimes    :: !(Maybe String)
        , animationValues      :: !(Maybe String)
        , animationRepeatCount :: !(Maybe String)
        }
    deriving (Show)

data TransformCommand a
    = Translate !(V2 a)
    | Scale !a !a
    | SkewX !a
    | SkewY !a
    | Matrix !a !a !a !a !a !a
    deriving (Functor, Show)

skewXY :: Floating a => a -> a -> TransformCommand a
skewXY ax ay =
    let tx = tan (pi * ax / 180)
        ty = tan (pi * ay / 180) in
    Matrix 1 ty tx 1 0 0

data PathCommand a
    = MoveTo Bool (V2 a)
    | LineTo Bool (V2 a)
    | HorizontalTo Bool a
    | VerticalTo Bool a
    | QuadraticTo Bool (V2 a) (V2 a)
    | Close
    deriving (Functor, Show)

makeElement :: ElementGuts a -> Element a
makeElement guts = Element
    { elementId        = Nothing
    , elementStyle     = Nothing
    , elementClipPath  = Nothing
    , elementClipRule  = Nothing
    , elementTransform = []
    , elementAnimate   = []
    , elementGuts      = guts
    }

setElementStyle :: String -> Element a -> Element a
setElementStyle style el = el {elementStyle = Just style}

addElementAnimate :: Animation -> Element a -> Element a
addElementAnimate a el = el {elementAnimate = elementAnimate el ++ [a]}

addElementTransform :: TransformCommand a -> Element a -> Element a
addElementTransform t el = el {elementTransform = elementTransform el ++ [t]}

elementById :: String -> Svg a -> Either String (Element a)
elementById key svg = case foldMap (go mempty) (svgElements svg) of
    [el]  -> Right el
    []    -> Left $ "Element with ID " ++ show key ++ " not found"
    _ : _ -> Left $ "Multiple elements with ID " ++ show key ++ " found"
  where
    go transform el =
        (if Just key == elementId el
            then [el {elementTransform = transform <> elementTransform el}]
            else []) ++
        (case elementGuts el of
            GroupElement children ->
                foldMap (go (transform <> elementTransform el)) children
            _ -> [])

parseViewBox :: AP.Parser (ViewBox Double)
parseViewBox = (,,,)
    <$> AP.double <* AP.skipSpace
    <*> AP.double <* AP.skipSpace
    <*> AP.double <* AP.skipSpace
    <*> AP.double <* AP.skipSpace <* AP.endOfInput

parsePathCommands :: String -> Either String [PathCommand Double]
parsePathCommands str = case dropWhile isSpace str of
    [] -> pure []
    -- [] -> if null remainder then pure [] else parsePathCommands' remainder
    c : remainder -> do
        let c' = toLower c
            absolute = isUpper c
            (header, remainder') = break isCommandChar remainder
        args' <- case traverse readMaybe . words $ map commaToSpace header of
            Nothing -> Left $ "Failure reading path command: " ++ header
            Just as -> Right as
        pc <- case (c', args') of
            ('m', as) | Just (v : vs) <- pairsWith V2 as -> pure $
                MoveTo absolute (v :: V2 Double) : map (LineTo absolute) vs
            ('l', as) | Just vs <- pairsWith V2 as -> pure $
                map (LineTo absolute) vs
            ('h', ys@(_ : _)) -> pure $ map (HorizontalTo absolute) ys
            ('v', xs@(_ : _)) -> pure $ map (VerticalTo absolute) xs
            ('q', as)
                | Just v2s <- pairsWith V2 as
                , Just pairs <- pairsWith (,) v2s -> pure $
                    [QuadraticTo absolute q p | (q, p) <- pairs]
            ('z', []) -> pure [Close]
            _ -> Left $ "Invalid path command: " ++ c : header

        (pc ++) <$> parsePathCommands remainder'
  where
    isCommandChar c = c /= 'e' && isAlpha c
    commaToSpace c = if c == ',' then ' ' else c

    pairsWith f (x : y : zs) = (f x y :) <$> pairsWith f zs
    pairsWith _ []           = pure []
    pairsWith _ _            = Nothing

renderPathCommand :: Show a => PathCommand a -> String
renderPathCommand = \case
    MoveTo absolute (V2 x y) ->
        command absolute 'M' <> " " <> show x <> "," <> show y
    LineTo absolute (V2 x y) ->
        command absolute 'L' <> " " <> show x <> "," <> show y
    HorizontalTo absolute y ->
        command absolute 'H' <> " " <> show y
    VerticalTo absolute x ->
        command absolute 'V' <> " " <> show x
    QuadraticTo absolute (V2 x1 y1) (V2 x y) ->
        command absolute 'Q' <> " " <> show x1 <> "," <> show y1 <> " " <>
        show x <> "," <> show y
    Close -> "Z"
  where
    command absolute c = pure $ if absolute then c else toLower c

renderPathCommands :: Show a => [PathCommand a] -> String
renderPathCommands = unwords . map renderPathCommand

parseTransformCommands :: String -> Either String [TransformCommand Double]
parseTransformCommands = \str0 -> do
    let groups :: [[String]]
        groups = List.groupBy groupCommands . words $ map puncToSpace str0
    tuples <- mapM parseArgs groups
    mapM parseCmd tuples
  where
    parseArgs :: [String] -> Either String (String, [Double])
    parseArgs []       = Left "Empty command"
    parseArgs (c : xs) = case mapM readMaybe xs of
        Just doubles -> Right (c, doubles)
        Nothing      -> Left $ "Could not parse arguments of " ++ c
    groupCommands _ = not . all isAlpha
    puncToSpace = \case
        ',' -> ' '
        '(' -> ' '
        ')' -> ' '
        c   -> c
    parseCmd = \case
        ("translate", [x, y])             -> pure $ Translate (V2 x y)
        ("translate", [x])                -> pure $ Translate (V2 x 0)
        ("matrix",    [a, b, c, d, e, f]) -> pure $ Matrix a b c d e f
        ("skewX",     [a])                -> pure $ SkewX a
        ("skewY",     [a])                -> pure $ SkewY a
        (cmd, _)                          -> Left $ "Unknown command: " ++ cmd

renderTransformCommand :: Show a => TransformCommand a -> String
renderTransformCommand = \case
    Translate (V2 x y) -> "translate(" ++ show x ++ ", " ++ show y ++ ")"
    Scale x y -> "scale(" ++ show x ++ ", " ++ show y ++ ")"
    SkewX a -> "skewX(" ++ show a ++ ")"
    SkewY a -> "skewY(" ++ show a ++ ")"
    Matrix a b c d e f ->
        "matrix(" ++ List.intercalate ", " (map show [a, b, c, d, e, f]) ++ ")"

renderTransformCommands :: Show a => [TransformCommand a] -> String
renderTransformCommands = unwords . map renderTransformCommand

parseSvg :: Xml.Element -> Either String (Svg Double)
parseSvg el
    | Xml.qName (Xml.elName el) /= "svg" = Left
        "Expected top-level SVG element"
    | otherwise = do
        vb <- maybe (Left "Missing viewbox") pure $ xmlAttribute "viewBox" el
        svgViewBox@(_, _, vbw, vbh) <- AP.parseOnly parseViewBox $ T.pack vb
        svgElements <- catMaybes <$> traverse parseSvgElement (xmlChildren el)
        -- TODO(jaspervdj): width and height can be specified as `px`, `em`, ...
        let svgWidth  = fromMaybe vbw $ xmlAttribute "width" el >>= readUnit
            svgHeight = fromMaybe vbh $ xmlAttribute "height" el >>= readUnit
        pure Svg {..}
  where
    readUnit str = case reads str of
        [(x, unit)] | unit `elem` units -> Just x
        _                               -> Nothing
      where
        units = ["", "em", "px", "mm"]

readSvg :: FilePath -> IO (Svg Double)
readSvg path = do
    contents <- readFile path
    case Xml.parseXMLDoc contents of
        Nothing  -> fail $ path ++ ": Invalid XML"
        Just xml -> case parseSvg xml of
            Left err  -> fail $ path ++ ": Couldn't parse SVG: " ++ err
            Right svg -> pure svg

parseSvgElement :: Xml.Element -> Either String (Maybe (Element Double))
parseSvgElement el = do
    let elementId = xmlAttribute "label" el <|> xmlAttribute "id" el
        elementStyle = xmlAttribute "style" el
    maybeElementGuts <- case Xml.qName (Xml.elName el) of
        "g" -> do
            els <- fmap catMaybes . traverse parseSvgElement $ xmlChildren el
            pure . Just $ GroupElement els
        "clipPath" -> do
            els <- fmap catMaybes . traverse parseSvgElement $ xmlChildren el
            pure . Just $ ClipPathElement els
        "path" -> do
            d <- needAttr "d" $ xmlAttribute "d" el
            commands <- parsePathCommands d
            pure . Just $ PathElement commands
        "image" -> do
            let imagePosition = V2 <$>
                    (xmlAttribute "x" el >>= readMaybe) <*>
                    (xmlAttribute "y" el >>= readMaybe)
                imageWidth = xmlAttribute "width" el >>= readMaybe
                imageHeight = xmlAttribute "height" el >>= readMaybe
            imageHref <- needAttr "href" $ xmlAttribute "href" el
            pure . Just $ ImageElement Image {..}
        "line" -> do
            x1 <- needAttr "x1" $ xmlAttribute "x1" el >>= readMaybe
            y1 <- needAttr "y1" $ xmlAttribute "y1" el >>= readMaybe
            x2 <- needAttr "x2" $ xmlAttribute "x2" el >>= readMaybe
            y2 <- needAttr "y2" $ xmlAttribute "y2" el >>= readMaybe
            pure . Just $ LineElement (V2 x1 y1) (V2 x2 y2)
        "rect" -> do
            x <- needAttr "x" $ xmlAttribute "x" el >>= readMaybe
            y <- needAttr "y" $ xmlAttribute "y" el >>= readMaybe
            w <- needAttr "width" $ xmlAttribute "width" el >>= readMaybe
            h <- needAttr "height" $ xmlAttribute "height" el >>= readMaybe
            pure . Just $ RectElement (V2 x y) w h
        "circle" -> do
            x <- needAttr "cx" $ xmlAttribute "cx" el >>= readMaybe
            y <- needAttr "cy" $ xmlAttribute "cy" el >>= readMaybe
            r <- needAttr "r" $ xmlAttribute "r" el >>= readMaybe
            pure . Just $ CircleElement (V2 x y) r
        _ -> pure Nothing
    elementTransform <- case xmlAttribute "transform" el of
        Just str -> parseTransformCommands str
        Nothing  -> pure []
    let elementAnimate = []  -- TODO: parse <animate> children
        elementClipPath = Nothing
        elementClipRule = Nothing
    case maybeElementGuts of
        Nothing          -> pure Nothing
        Just elementGuts -> pure . Just $ Element {..}

  where
    needAttr str = maybe (Left $ "Missing attribute: " ++ str) pure

xmlChildren :: Xml.Element -> [Xml.Element]
xmlChildren element = [child | Xml.Elem child <- Xml.elContent element]

xmlAttribute :: String -> Xml.Element -> Maybe String
xmlAttribute key el =
    case break ((== key) . Xml.qName . Xml.attrKey) (Xml.elAttribs el) of
        (_, attr : _) -> Just $ Xml.attrVal attr
        _             -> Nothing

renderSvg :: Show a => Svg a -> Xml.Element
renderSvg Svg {..} = Xml.Element
    { Xml.elName = Xml.QName "svg" Nothing Nothing
    , Xml.elAttribs =
        [ Xml.Attr (Xml.unqual "xmlns") "http://www.w3.org/2000/svg"
        , Xml.Attr (Xml.QName "xlink" Nothing (Just "xmlns"))
            "http://www.w3.org/1999/xlink"
        , Xml.Attr (Xml.unqual "viewBox") $ case svgViewBox of
            (x, y, w, h) -> unwords $ map show [x, y, w, h]
        ]
    , Xml.elContent = map (Xml.Elem . renderSvgElement) svgElements
    , Xml.elLine = Nothing
    }

renderSvgElement :: Show a => Element a -> Xml.Element
renderSvgElement Element {..} = case elementGuts of
    GroupElement children -> Xml.Element
        { Xml.elName    = Xml.unqual "g"
        , Xml.elAttribs = attrs
        , Xml.elContent = content ++ map (Xml.Elem . renderSvgElement) children
        , Xml.elLine    = Nothing
        }
    ClipPathElement children -> Xml.Element
        { Xml.elName    = Xml.unqual "clipPath"
        , Xml.elAttribs = attrs
        , Xml.elContent = content ++ map (Xml.Elem . renderSvgElement) children
        , Xml.elLine    = Nothing
        }
    PathElement commands -> Xml.Element
        { Xml.elName    = Xml.unqual "path"
        , Xml.elAttribs = attr "d" (renderPathCommands commands) ++ attrs
        , Xml.elContent = content
        , Xml.elLine    = Nothing
        }
    ImageElement Image {..} -> Xml.Element
        { Xml.elName    = Xml.unqual "image"
        , Xml.elAttribs = concat
            [ optAttr "x"      (show . v2X <$> imagePosition)
            , optAttr "y"      (show . v2Y <$> imagePosition)
            , optAttr "width"  (show <$> imageWidth)
            , optAttr "height" (show <$> imageHeight)
            , [Xml.Attr (Xml.QName "href" Nothing (Just "xlink")) imageHref]
            , attrs
            ]
        , Xml.elContent = content
        , Xml.elLine    = Nothing
        }
    LineElement (V2 x1 y1) (V2 x2 y2) -> Xml.Element
        { Xml.elName    = Xml.unqual "line"
        , Xml.elAttribs = concat
            [ attr "x1" (show x1), attr "y1" (show y1)
            , attr "x2" (show x2), attr "y2" (show y2)
            , attrs
            ]
        , Xml.elContent = content
        , Xml.elLine    = Nothing
        }
    RectElement (V2 x y) w h -> Xml.Element
        { Xml.elName    = Xml.unqual "rect"
        , Xml.elAttribs = concat
            [ attr "x" (show x), attr "y" (show y)
            , attr "width" (show w), attr "height" (show h)
            , attrs
            ]
        , Xml.elContent = content
        , Xml.elLine    = Nothing
        }
    CircleElement (V2 x y) r -> Xml.Element
        { Xml.elName    = Xml.unqual "circle"
        , Xml.elAttribs = concat
            [attr "cx" (show x), attr "cy" (show y), attr "r" (show r), attrs]
        , Xml.elContent = content
        , Xml.elLine    = Nothing
        }
    TextElement Text {..} -> Xml.Element
        { Xml.elName    = Xml.unqual "text"
        , Xml.elAttribs = concat
            [ attr "x" (show . v2X $ textPosition)
            , attr "y" (show . v2Y $ textPosition)
            , case textCenter of
                False -> []
                True ->
                    attr "dominant-baseline" "middle" ++
                    attr "text-anchor" "middle"
            , attrs
            ]
        , Xml.elContent =
            [Xml.Text $ Xml.CData Xml.CDataText textContent Nothing]
        , Xml.elLine    = Nothing
        }
    RawXml el -> el
  where
    attrs = concat
        [ optAttr "id"        elementId
        , optAttr "style"     elementStyle
        , optAttr "clip-path" elementClipPath
        , optAttr "clip-rule" elementClipRule
        , optAttr "transform" $ case elementTransform of
            []  -> Nothing
            tfs -> Just $ renderTransformCommands tfs
        ]

    content = flip map elementAnimate $ \case
        Animate {..} -> Xml.Elem $ Xml.Element
            { Xml.elName    = Xml.unqual "animate"
            , Xml.elAttribs = concat
                [ attr    "attributeName" animationAttributeName
                , optAttr "begin"         animationBegin
                , attr    "dur"           animationDur
                , optAttr "keyTimes"      animationKeyTimes
                , optAttr "to"            animationTo
                , optAttr "values"        animationValues
                , optAttr "repeatCount"   animationRepeatCount
                ]
            , Xml.elContent = []
            , Xml.elLine    = Nothing
            }
        AnimateTransform {..} -> Xml.Elem $ Xml.Element
            { Xml.elName    = Xml.unqual "animateTransform"
            , Xml.elAttribs = concat
                [ attr    "attributeType" "xml"
                , attr    "attributeName" "transform"
                , attr    "type"          animationType
                , attr    "dur"           animationDur
                , optAttr "keyTimes"      animationKeyTimes
                , optAttr "values"        animationValues
                , optAttr "repeatCount"   animationRepeatCount
                , attr    "additive"      "sum"
                ]
            , Xml.elContent = []
            , Xml.elLine    = Nothing
            }

    attr    str = pure . Xml.Attr (Xml.unqual str)
    optAttr str = maybe [] (attr str)

encodeSvg :: Show a => Svg a -> String
encodeSvg = Xml.ppTopElement . renderSvg

makeSvg :: ViewBox a -> Svg a
makeSvg viewBox@(_, _, vbw, vbh) = Svg
    { svgViewBox  = viewBox
    , svgWidth    = vbw
    , svgHeight   = vbh
    , svgElements = []
    }

data ProblemToSvgSettings = ProblemToSvgSettings
    { ptssMargin   :: Integer
    , ptssFillHole :: Maybe String
    , ptssSolution :: Maybe Solution
    }

defaultProblemToSvgSettings :: ProblemToSvgSettings
defaultProblemToSvgSettings = ProblemToSvgSettings 5 Nothing Nothing

problemToSvg :: Problem -> Svg Integer
problemToSvg = problemToSvgWith defaultProblemToSvgSettings

problemToSvgWith
    :: ProblemToSvgSettings -> Problem -> Svg Integer
problemToSvgWith ProblemToSvgSettings {..} problem@Problem {..} =
    (makeSvg viewBox)
        { svgElements =
            [bonusGroup | not $ V.null problemBonuses] ++
            [holePath, figureGroup]
        }
  where
    holePath :: Element Integer
    holePath =
        let verts = fmap (.+. offset) . unPolygon $ unHole problemHole in
        Element
            { elementId        = Just "hole"
            , elementStyle     = Just $ case ptssFillHole of
                Nothing   -> "fill:none;stroke:#000000;stroke-linecap:round;stroke-linejoin:round;"
                Just fill -> "fill:" <> fill <> ";fill-rule:evenodd;stroke:none;"
            , elementClipPath  = Nothing
            , elementClipRule  = Nothing
            , elementTransform = []
            , elementAnimate   = []
            , elementGuts      = PathElement $
                MoveTo True (verts V.! 0) :
                map (LineTo True) (V.toList $ V.drop 1 verts) ++ [Close] ++
                case ptssFillHole of
                    Nothing -> []
                    Just _ ->
                        [ MoveTo True (V2 0       0      )
                        , LineTo True (V2 canvasW 0      )
                        , LineTo True (V2 canvasW canvasH)
                        , LineTo True (V2 0       canvasH)
                        ]
            }

    figure' = case ptssSolution of
        Just solution -> solutionFigure problem solution
        Nothing       -> problemFigure

    figureGroup :: Element Integer
    figureGroup = (edgesToSvgElement $ foldFigureEdges pure figure')
        { elementId    = Just "figure"
        , elementStyle = Just
            "fill:none;stroke:#ff0000;stroke-linecap:round"
        , elementTransform = [Translate offset]
        }

    bonusGroup :: Element Integer
    bonusGroup = (makeElement $ GroupElement $ do
        bonus <- V.toList problemBonuses
        let setStyle el = el {elementStyle = Just $ bonusStyle bonus}
        pure . setStyle . makeElement $ CircleElement (bonusPosition bonus) 5)
            { elementId = Just "bonus"
            , elementTransform = [Translate offset]
            }

    bonusStyle bonus = "fill:" <> bonusColor (bonusBonus bonus)

    box = problemBox problem
    canvasW = Box.width box + ptssMargin * 2
    canvasH = Box.height box + ptssMargin * 2
    viewBox = (0, 0, canvasW, canvasH)
    offset  = V2 ptssMargin ptssMargin .-. Box.topLeft box

edgesToSvgElement :: [Edge a] -> Element a
edgesToSvgElement edges = makeElement . GroupElement $ do
    Edge p q <- edges
    pure . makeElement $ PathElement [MoveTo True p, LineTo True q]

polygonToSvgElement :: Polygon a -> Element a
polygonToSvgElement (Polygon verts) = makeElement . PathElement $
    MoveTo True (verts V.! 0) :
    map (LineTo True) (V.toList $ V.drop 1 verts) ++ [Close]

type Transform a = L.M33 a

applyTransform :: Num a => Transform a -> V2 a -> V2 a
applyTransform mat (V2 x y) =
    case mat L.!* L.V3 x y 1 of L.V3 x' y' _ -> V2 x' y'

transformFromCommand :: Floating a => TransformCommand a -> L.M33 a
transformFromCommand = \case
    Matrix a b c d e f -> L.V3
        (L.V3 a c e)
        (L.V3 b d f)
        (L.V3 0 0 1)
    Scale x y -> L.V3
        (L.V3 x 0 0)
        (L.V3 0 y 0)
        (L.V3 0 0 1)
    SkewX a -> let t = tan (pi * a / 180) in L.V3
        (L.V3 1 t 0)
        (L.V3 0 1 0)
        (L.V3 0 0 1)
    SkewY a -> let t = tan (pi * a / 180) in L.V3
        (L.V3 1 0 0)
        (L.V3 t 1 0)
        (L.V3 0 0 1)
    Translate (V2 x y) -> L.V3
        (L.V3 1 0 x)
        (L.V3 0 1 y)
        (L.V3 0 0 1)

data Pen a = Pen
    { penDrawn     :: [NonEmpty (V2 a)]
    , penCurrent   :: [V2 a]
    , penPosition  :: V2 a
    , penTransform :: Transform a
    } deriving (Show)

emptyPen :: Num a => Pen a
emptyPen = Pen [] [] (V2 0 0) L.identity

traceSvgElement :: (Eq a, Floating a, Show a) => Element a -> State (Pen a) ()
traceSvgElement Element {..} = do
    oldTransform <- state $ \s ->
        let mat = penTransform s
            mat' = List.foldl'
                (\acc x -> acc L.!*! transformFromCommand x) mat elementTransform in
        (mat, s {penTransform = mat'})
    case elementGuts of
        GroupElement children -> for_ children traceSvgElement
        ClipPathElement _ -> pure ()
        PathElement commands -> do
            modify $ \s -> s {penPosition = V2 0 0}
            for_ commands tracePathCommand
            closePath
        ImageElement _ -> pure ()
        -- TODO: Allow parsing lines as well?  Maybe this is even easier for
        -- participants?  Also allow parsing rectangles?
        LineElement _ _ -> pure ()
        RectElement _ _ _ -> pure ()
        CircleElement _ _ -> pure ()
        TextElement _ -> pure ()
        RawXml _ -> pure ()
    modify $ \s -> s {penTransform = oldTransform}
  where
    closePath = do
        transform <- gets penTransform
        t <- map (applyTransform transform) <$> gets penCurrent
        modify $ \s -> s {penCurrent = []}
        unless (null t) $ modify $ \s -> s
            { penDrawn = penDrawn s ++ [NonEmpty.fromList t]
            }

    makeAbsolute :: Num a => Bool -> V2 a -> State (Pen a) (V2 a)
    makeAbsolute absolute point
        | absolute    = pure point
        | otherwise = do
            offset <- gets penPosition
            pure $ offset .+. point

    tracePathCommand = \case
        MoveTo absolute point -> do
            closePath
            point' <- makeAbsolute absolute point
            modify $ \s -> s {penPosition = point'}
        LineTo absolute point -> do
            point' <- makeAbsolute absolute point
            position <- gets penPosition
            modify $ \s -> s
                { penPosition = point'
                , penCurrent  = case penCurrent s of
                    []     -> [position, point']
                    points -> points ++ [point']
                }
        HorizontalTo absolute x -> do
            position <- gets penPosition
            let y = if absolute then v2Y position else 0
            tracePathCommand $ LineTo absolute (V2 x y)
        VerticalTo absolute y -> do
            position <- gets penPosition
            let x = if absolute then v2X position else 0
            tracePathCommand $ LineTo absolute (V2 x y)
        QuadraticTo absolute _ point ->
            -- Replace by straight line
            tracePathCommand $ LineTo absolute point
        Close -> do
            current <- gets penCurrent
            position <- gets penPosition
            when (not (null current) && position /= head current) $
                modify $ \s -> s
                    { penCurrent  = penCurrent s ++ [head current]
                    , penPosition = head current
                    }
            closePath

svgSetLongEdge :: Double -> Svg Double -> Svg Double
svgSetLongEdge longEdge svg
    | svgWidth svg > svgHeight svg = svg
        { svgWidth  = longEdge
        , svgHeight = svgHeight svg * (longEdge / svgWidth svg)
        }
    | otherwise = svg
        { svgWidth  = svgWidth svg * (longEdge / svgHeight svg)
        , svgHeight = longEdge
        }

problemFromSvg :: Svg Double -> Either String Problem
problemFromSvg svg = do
    holeElement <- elementById "hole" svg
    Polygon hole <- polygonFromSvg holeElement

    figureEl <- elementById "figure" svg
    -- Only do rounding at the very end.
    let problemHole = Hole . Polygon $ fmap toIntegerGrid hole
        problemFigure = figureFromSvg figureEl
        problemEpsilon = 10
        problemBonuses = mempty
    pure $ Problem {..}
  where
    figureFromSvg :: Element Double -> Figure
    figureFromSvg el =
        let drawn = penDrawn $ execState (traceSvgElement el) emptyPen
            drawnEdges = concat $ do
                points <- fmap toIntegerGrid . NonEmpty.toList <$> drawn
                pure $ List.zipWith Edge points $ drop 1 points

            vertices = V.fromList . Set.toList $ Set.fromList
                [x | Edge p q <- drawnEdges, x <- [p, q]]

            vertexIndices = Map.fromList $ zip (V.toList vertices) [0 ..]

            edges = V.fromList $ do
                Edge p q <- drawnEdges
                i <- maybeToList $ Map.lookup p vertexIndices
                j <- maybeToList $ Map.lookup q vertexIndices
                pure (i, j) in

        Figure vertices edges

    toIntegerGrid :: V2 Double -> V2 Integer
    toIntegerGrid (V2 x y) = fmap round $
        V2 (svgWidth svg * (x - vbx) / vbw) (svgHeight svg * (y - vby) / vbh)

    (vbx, vby, vbw, vbh) = svgViewBox svg

polygonFromSvg :: Element Double -> Either String (Polygon Double)
polygonFromSvg el =
    case penDrawn $ execState (traceSvgElement el) emptyPen of
        [p0 :| ps@(_ : _)] -> do
            when (last ps /= p0) . Left $ "Expected hole to close?"
            case mkPolygon . V.fromList $ p0 : init ps of
                Nothing -> Left "Invalid polygon"
                Just p  -> pure p
        _ -> Left "Expected a single hole drawing"

bonusColor :: BonusType -> String
bonusColor = \case
    Globalist -> "yellow"
    SuperFlex -> "cyan"
    WallHack  -> "orange"
    BreakALeg -> "blue"
