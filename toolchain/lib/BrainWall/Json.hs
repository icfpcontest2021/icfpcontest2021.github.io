{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module BrainWall.Json
    ( encodeProblem
    , decodeProblem
    , encodeSolution
    , decodeSolution
    , decodeFigure

    , decodeWith
    , decodeFileWith
    , encodeText

    , featuresEnabledBonuses
    ) where

import           BrainWall.Database      (Features (..), ProblemId (..),
                                          featuresAnyBonuses)
import           BrainWall.Polygon
import           BrainWall.Problem
import           BrainWall.V2
import           Control.Lens            (preview, review)
import qualified Data.Aeson              as Aeson
import qualified Data.Aeson.Types        as Aeson
import qualified Data.ByteString         as B
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Vector             as V

encodeV2 :: Aeson.ToJSON a => V2 a -> Aeson.Value
encodeV2 (V2 x y) = Aeson.toJSON (x, y)

decodeV2 :: Aeson.FromJSON a => Aeson.Value -> Aeson.Parser (V2 a)
decodeV2 val = do
    (x, y) <- Aeson.parseJSON val
    pure $ V2 x y

encodeHole :: Hole -> Aeson.Value
encodeHole = Aeson.toJSON . fmap encodeV2 . unPolygon . unHole

decodeHole :: Aeson.Value -> Aeson.Parser Hole
decodeHole val = do
    verts <- Aeson.parseJSON val >>= traverse decodeV2
    case mkPolygon verts of
        Nothing  -> fail "decodeHole: invalid polygon"
        Just pol -> pure $ Hole pol

encodeFigure :: Figure -> Aeson.Value
encodeFigure Figure {..} = Aeson.object
    [ "vertices" Aeson..= fmap encodeV2 figureVertices
    , "edges"    Aeson..= figureEdges
    ]

decodeFigure :: Aeson.Value -> Aeson.Parser Figure
decodeFigure = Aeson.withObject "figure" $ \obj -> Figure
    <$> (obj Aeson..: "vertices" >>= traverse decodeV2)
    <*> obj Aeson..: "edges"

encodeBonusType :: BonusType -> Aeson.Value
encodeBonusType = Aeson.toJSON . review bonusTypeFromText

decodeBonusType :: Aeson.Value -> Aeson.Parser BonusType
decodeBonusType = Aeson.withText "bonus type" $
    maybe (fail "unknown bonus type") pure . preview bonusTypeFromText

encodeBonus :: Bonus -> Aeson.Value
encodeBonus Bonus {..} = Aeson.object
    [ "problem"  Aeson..= bonusProblem
    , "bonus"    Aeson..= encodeBonusType bonusBonus
    , "position" Aeson..= encodeV2 bonusPosition
    ]

decodeBonus :: Aeson.Value -> Aeson.Parser Bonus
decodeBonus = Aeson.withObject "bonus" $ \obj -> Bonus
    <$> obj Aeson..: "problem"
    <*> (obj Aeson..: "bonus" >>= decodeBonusType)
    <*> (obj Aeson..: "position" >>= decodeV2)

includeBonus :: Features -> Bonus -> Bool
includeBonus features Bonus {..} =
    bonusBonus `elem` featuresEnabledBonuses features &&
    featuresEnabledProblems features (ProblemId bonusProblem)

encodeProblem :: Features -> Problem -> Aeson.Value
encodeProblem features Problem {..} = Aeson.object $
    [ "hole"    Aeson..= encodeHole problemHole
    , "figure"  Aeson..= encodeFigure problemFigure
    , "epsilon" Aeson..= problemEpsilon
    ] ++
    [ "bonuses" Aeson..= fmap encodeBonus
        (V.filter (includeBonus features) problemBonuses)
    | featuresAnyBonuses features
    ]

decodeProblem :: Features -> Aeson.Value -> Aeson.Parser Problem
decodeProblem features = Aeson.withObject "problem" $ \obj -> do
    problemHole    <- obj Aeson..: "hole" >>= decodeHole
    problemFigure  <- obj Aeson..: "figure" >>= decodeFigure
    problemEpsilon <- obj Aeson..: "epsilon"
    problemBonuses <- case featuresAnyBonuses features of
        False -> pure mempty
        True  ->
            (obj Aeson..:? "bonuses" Aeson..!= mempty) >>=
            traverse decodeBonus >>=
            pure . V.filter (includeBonus features)
    pure Problem {..}

featuresEnabledBonuses :: Features -> [BonusType]
featuresEnabledBonuses Features {..} =
    [SuperFlex | featuresBonusSuperFlexEnabled] ++
    [Globalist | featuresBonusGlobalistEnabled] ++
    [BreakALeg | featuresBonusBreakALegEnabled] ++
    [WallHack  | featuresBonusWallhackEnabled ]

encodeClaimedBonus :: ClaimedBonus -> Aeson.Value
encodeClaimedBonus ClaimedBonus {..} = Aeson.object $
    [ "problem" Aeson..= claimedBonusProblem
    , "bonus"   Aeson..= encodeBonusType claimedBonusBonus
    ] ++
    (case claimedBonusEdge of
        Nothing -> []
        Just e  -> ["edge" Aeson..= e])

decodeClaimedBonus :: Aeson.Value -> Aeson.Parser ClaimedBonus
decodeClaimedBonus = Aeson.withObject "bonus" $ \obj -> ClaimedBonus
    <$> obj Aeson..: "problem"
    <*> (obj Aeson..: "bonus" >>= decodeBonusType)
    <*> (obj Aeson..:? "edge")

encodeSolution :: Features -> Solution -> Aeson.Value
encodeSolution features Solution {..} = Aeson.object $
    ["vertices" Aeson..= fmap encodeV2 solutionVertices] ++
    [ "bonuses" Aeson..= fmap encodeClaimedBonus (V.filter
        (\ClaimedBonus {..} ->
            claimedBonusBonus `elem` featuresEnabledBonuses features)
        solutionBonuses)
    | featuresAnyBonuses features
    ]

decodeSolution :: Features -> Aeson.Value -> Aeson.Parser Solution
decodeSolution features = Aeson.withObject "solution" $ \obj -> do
    solutionVertices <- obj Aeson..: "vertices" >>= traverse decodeV2
    solutionBonuses <- case featuresAnyBonuses features of
        False -> pure mempty
        True  -> (obj Aeson..:? "bonuses" Aeson..!= mempty) >>=
            traverse decodeClaimedBonus >>=
            pure . V.filter
                (\ClaimedBonus {..} ->
                    claimedBonusBonus `elem` featuresEnabledBonuses features)
    pure Solution {..}

decodeWith :: (Aeson.Value -> Aeson.Parser a) -> B.ByteString -> Either String a
decodeWith f bs = do
    json <- maybe (Left "bad JSON") Right $ Aeson.decodeStrict bs
    case Aeson.parse f json of
        Aeson.Success x -> pure x
        Aeson.Error err -> Left err

decodeFileWith :: (Aeson.Value -> Aeson.Parser a) -> FilePath -> IO a
decodeFileWith f path = B.readFile path >>= either fail pure . decodeWith f

encodeText :: Aeson.ToJSON a => a -> T.Text
encodeText = TL.toStrict . TL.decodeUtf8 . Aeson.encode
