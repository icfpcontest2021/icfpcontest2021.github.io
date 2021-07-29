{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module BrainWall.Triangle
    ( Triangle (..)
    , triangleEdges
    , triangleArea
    ) where

import           BrainWall.Edge
import           BrainWall.V2
import           Data.Hashable  (Hashable)
import           GHC.Generics   (Generic)

data Triangle a = Triangle !(V2 a) !(V2 a) !(V2 a)
    deriving (Eq, Functor, Generic, Show)

instance Hashable a => Hashable (Triangle a)

triangleEdges :: Triangle a -> [Edge a]
triangleEdges (Triangle p q r) = [Edge p q, Edge q r, Edge r p]

triangleArea :: Floating a => Triangle a -> a
triangleArea (Triangle p q r) =
    let a = distance p q
        b = distance q r
        c = distance r p
        s = (a + b + c) / 2 in
    sqrt $ s * (s - a) * (s - b) * (s - c)
