{-# LANGUAGE FlexibleContexts #-}
module BrainWall.Main.Texture
    ( main
    ) where

import qualified BrainWall.Box                   as Box
import           BrainWall.Database              (allFeatures)
import           BrainWall.Graphics.Delaunay
import           BrainWall.Graphics.Projective
import           BrainWall.Json
import           BrainWall.Planar
import           BrainWall.Polygon
import           BrainWall.Polygon.ContainsPoint
import           BrainWall.Polygon.Triangulate
import           BrainWall.Problem
import           BrainWall.Svg
import           BrainWall.V2                    as V2
import qualified Codec.Picture                   as Pic
import           Control.Monad                   (guard)
import qualified Data.ByteString.Base64.Lazy     as Base64
import qualified Data.ByteString.Lazy.Char8      as BL8
import qualified Data.HashMap.Strict             as HMS
import qualified Data.List                       as L
import           Data.Maybe                      (fromMaybe, maybeToList)
import qualified Data.Vector                     as V
import           System.Environment              (getArgs, getProgName)
import           System.Exit                     (exitFailure)
import qualified System.IO                       as IO

main :: IO ()
main = do
    progName <- getProgName
    args <- getArgs
    case args of
        [problemPath, srcImagePath, solutionPath, dstImagePath] -> do
            problem <- decodeFileWith (decodeProblem allFeatures) problemPath

            srcImage <- Pic.readImage srcImagePath >>=
                either fail (pure . Pic.convertRGBA8)

            solution <- solutionVertices <$>
                decodeFileWith (decodeSolution allFeatures) solutionPath

            let -- triangles = triangulate $ figurePoints
                triangulated = do
                    let faces = planarGraphFaces $ figureGraph figure
                    face <- fmap (figureVertices figure V.!) <$> faces
                    polygon <- maybeToList $ faceToPolygon face
                    triangulation <- maybeToList $ earCutting polygon
                    triangle <- delaunay triangulation
                    pure (triangle, face)

                figure = problemFigure problem
                solutionMapping = HMS.fromList $ do
                    (i, v) <- V.toList . V.imap (,) $ figureVertices figure
                    pure (v, solution V.! i)
                solutionTransform v = fromMaybe v $ HMS.lookup v solutionMapping

                fourPoints :: Triangle Integer -> FourPoints Double
                fourPoints (Triangle p q r) =
                    let p' = fromIntegral <$> p
                        q' = fromIntegral <$> q
                        r' = fromIntegral <$> r
                        c' = (q' .+. r') ./ 2
                        s' = p' .+. (c' .-. p') .* 2 in
                    (p', q', r', s')

                transformedTriangles = do
                    (triangle@(Triangle p q r), face) <- triangulated
                    let from = fourPoints triangle
                        p' = solutionTransform p
                        q' = solutionTransform q
                        r' = solutionTransform r
                        triangle' = Triangle p' q' r'
                        to = fourPoints triangle'
                        mapVertex = projectiveTransform to from
                        face' = solutionTransform <$> face

                        polygon' = do
                            verts <- unPolygon <$> faceToPolygon face'
                            mkPolygon $
                                fmap round . V2.zipWith (*) scale .
                                fmap fromIntegral <$> verts

                    pure
                        ( triangle'
                        , mapVertex :: V2 Double -> V2 Double
                        , face'
                        , polygon'
                        )

                (width, height) =
                    let b = problemBox problem in (Box.width b, Box.height b)
                scale :: V2 Double
                scale = V2
                    (fromIntegral (Pic.imageWidth srcImage) / fromIntegral width)
                    (fromIntegral (Pic.imageHeight srcImage) / fromIntegral height)
                image = Pic.generateImage
                    makePixel
                    (Pic.imageWidth srcImage)
                    (Pic.imageHeight srcImage)
                makePixel x y =
                    L.foldl' highestAlpha (Pic.PixelRGBA8 0 0 0 0) $ do
                        let p = V2.zipWith (/) (fromIntegral <$> V2 x y) scale
                        (t, f, _, polygon) <- transformedTriangles
                        guard $ containsPoint (fromIntegral <$> t) p
                        guard $ maybe False (pointInPolygon (V2 x y)) polygon
                        let p' = f p
                            V2 x' y' = round <$> V2.zipWith (*) p' scale
                        guard $ x' >= 0 && x' < Pic.imageWidth srcImage
                        guard $ y' >= 0 && y' < Pic.imageHeight srcImage
                        pure $ Pic.pixelAt srcImage x' y'
                highestAlpha x@(Pic.PixelRGBA8 _ _ _ xa)
                             y@(Pic.PixelRGBA8 _ _ _ ya) =
                    if xa >= ya then x else y

                problem' = problem
                    { problemFigure = (problemFigure problem)
                        { figureVertices = solution
                        }
                    }

                base64Image =
                   "data:image/png;base64," ++
                    BL8.unpack (Base64.encode $ Pic.encodePng image)
                svg = problemToSvg problem'
                svg' = svg
                    { svgElements =
                        makeElement (ImageElement Image
                            { imagePosition = Just $ V2 0 0
                            , imageWidth    = Just width
                            , imageHeight   = Just height
                            , imageHref     = base64Image
                            }) :
                        svgElements svg
                    }


            -- print $ makePixel 80 65
            Pic.writePng dstImagePath image
            --
            -- print $ graphSpanningTree $ figureGraph figure
            print $ figureGraph figure
            print $ planarGraphFaces $ figureGraph figure
            writeFile "out.svg" $ encodeSvg svg'

        _ -> do
            mapM_ (IO.hPutStrLn IO.stderr)
                [ "Usage:"
                , ""
                , "    " ++ progName ++ " file.problem texture.png file.solution out.png"
                ]
            exitFailure
