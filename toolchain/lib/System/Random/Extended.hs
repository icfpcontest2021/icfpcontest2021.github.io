{-# LANGUAGE BangPatterns #-}
module System.Random.Extended
    ( module System.Random
    , distribution
    ) where

import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Monoid        (Sum (..))
import           System.Random

distribution
    :: (Num w, Random w, RandomGen g, Ord w)
    => NonEmpty (w, a) -> g -> (a, g)
distribution options gen0 =
    let !total = getSum $ foldMap (Sum . fst) options
        !(!marker, !gen1) = randomR (0, total) gen0 in
    (pick marker (NonEmpty.toList options), gen1)
  where
    pick _      []     = error "distribution: impossible?"
    pick marker ((f0, x0) : xs)
        | marker <= f0 = x0
        | null xs      = x0
        | otherwise    = pick (marker - f0) xs
