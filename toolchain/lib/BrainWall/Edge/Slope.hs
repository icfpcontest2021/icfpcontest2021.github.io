{-# LANGUAGE DeriveFunctor #-}
module BrainWall.Edge.Slope
    ( Slope (..)
    , fromV2
    , between
    ) where

import           BrainWall.V2
import           Data.Ratio     (Ratio, (%))

-- | Slope is an infinite precision representation of an angle.  It works
-- by storing the slope of the line in a "Quadrant".
--
--      Q3 | Q4
--     ----+----
--      Q2 | Q1
--
-- The constructors of this datatype are designed to have a good 'Ord' instance.
data Slope a
    = Q1 a
    | Q2 a
    | Q3 a
    | Q4 a
    deriving (Eq, Functor, Ord, Show)

fromV2 :: (Integral a, Ord a) => V2 a -> Slope (Ratio a)
fromV2 (V2 x y) = case compare x 0 of
    EQ -> case compare y 0 of
        EQ -> Q1 0
        LT -> Q4 0
        GT -> Q2 0
    LT -> case compare y 0 of
        EQ -> Q3 0
        LT -> Q3 $ (-y) % (-x)
        GT -> Q2 $ (-x) % y
    GT -> case compare y 0 of
        EQ -> Q1 0
        LT -> Q4 $ x % (-y)
        GT -> Q1 $ y % x

between :: Ord a => Slope a -> (Slope a, Slope a) -> Bool
between x (lo, hi)
    | lo < hi   = x >= lo && x <= hi
    | otherwise = x >= lo || x <= hi
