{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module BrainWall.V2
    ( V2 (..)
    , (.+.)
    , (.-.)
    , (.*)
    , (./)
    , distance
    , squaredDistance
    , crossProduct
    , dotProduct
    , zipWith

    , Polar (..)
    , fromPolar
    , toPolar
    , polarDegrees

    , v2FromString
    ) where

import           Control.Lens  (Prism', prism')
import           Data.Hashable (Hashable)
import           GHC.Generics  (Generic)
import           Prelude       hiding (zipWith)
import           Text.Read     (readMaybe)

data V2 a = V2 {v2X :: !a, v2Y :: !a} deriving (Eq, Functor, Generic, Ord)

instance Show a => Show (V2 a) where show (V2 x y) = show (x, y)

instance Hashable a => Hashable (V2 a)

(.+.) :: Num a => V2 a -> V2 a -> V2 a
V2 x0 y0 .+. V2 x1 y1 = V2 (x0 + x1) (y0 + y1)
infixl 6 .+.

(.-.) :: Num a => V2 a -> V2 a -> V2 a
V2 x0 y0 .-. V2 x1 y1 = V2 (x0 - x1) (y0 - y1)
infixl 6 .-.

(.*) :: Num a => V2 a -> a -> V2 a
V2 x y .* s = V2 (x * s) (y * s)
infixl 7 .*

(./) :: Fractional a => V2 a -> a -> V2 a
V2 x y ./ s = V2 (x / s) (y / s)
infixl 7 ./

distance :: Floating a => V2 a -> V2 a -> a
distance p q = sqrt $ squaredDistance p q

squaredDistance :: Num a => V2 a -> V2 a -> a
squaredDistance (V2 x0 y0) (V2 x1 y1) =
    let dx = x1 - x0
        dy = y1 - y0 in
    dx * dx + dy * dy

crossProduct :: Num a => V2 a -> V2 a -> a
crossProduct (V2 x0 y0) (V2 x1 y1) = x0 * y1 - y0 * x1

dotProduct :: Num a => V2 a -> V2 a -> a
dotProduct (V2 x0 y0) (V2 x1 y1) = x0 * x1 + y0 * y1

zipWith :: (a -> b -> c) -> V2 a -> V2 b -> V2 c
zipWith f (V2 x0 y0) (V2 x1 y1) = V2 (f x0 x1) (f y0 y1)

data Polar a = Polar {polarRho :: a, polarPhi :: a}
    deriving (Eq, Functor)

fromPolar :: Floating a => Polar a -> V2 a
fromPolar (Polar rho phi) = V2 (rho * cos phi) (rho * sin phi)

toPolar :: RealFloat a => V2 a -> Polar a
toPolar v@(V2 x y) = let phi = atan2 y x in Polar (sqrt $ dotProduct v v) phi

polarDegrees :: Floating a => Polar a -> a
polarDegrees (Polar _ phi) = phi * 180 / pi

v2FromString :: (Read a, Show a) => Prism' String (V2 a)
v2FromString = prism'
    (\(V2 x y) -> show x <> "," <> show y)
    (\str -> do
        [x, y] <- traverse readMaybe . words $
            map (\c -> if c == ',' then ' ' else c) str
        pure $ V2 x y)
