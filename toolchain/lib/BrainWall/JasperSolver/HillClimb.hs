{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
module BrainWall.JasperSolver.HillClimb
    ( Options (..)
    , defaultOptions
    , hillClimb
    ) where

import           System.Random (RandomGen)

data Options g s a = Options
    { oScore     :: a -> s
    , oNeighbour :: a -> g -> (a, g)
    , oGiveUp    :: Maybe Int
    }

defaultOptions :: Options g () a
defaultOptions = Options
    { oScore     = \_ -> ()
    , oNeighbour = (,)
    , oGiveUp    = Just 20000
    }

hillClimb :: (RandomGen g, Ord s) => Options g s a -> a -> g -> (a, g)
hillClimb Options {..} = \initial gen ->
    go (oScore initial, initial) 0 gen
  where
    go !prev@(!bestScore, !best) !i !gen0
        | maybe False (i >=) oGiveUp = (best, gen0)
        | otherwise                  =
            let (n, gen1) = oNeighbour best gen0
                nScore    = oScore n
                next      = if nScore >= bestScore then (nScore, n) else prev in
            go next (i + 1) gen1
