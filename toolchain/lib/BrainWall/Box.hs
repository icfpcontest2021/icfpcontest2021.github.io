-- Axis aligned bounding box
module BrainWall.Box
    ( Box (..)
    , width
    , height
    , fromV2
    ) where

import           BrainWall.V2 (V2 (..))
import qualified BrainWall.V2 as V2

data Box a = Box
    { topLeft     :: !(V2 a)
    , bottomRight :: !(V2 a)
    } deriving (Show)

instance Ord a => Semigroup (Box a) where
    b <> c = Box
        { topLeft     = V2.zipWith min (topLeft     b) (topLeft     c)
        , bottomRight = V2.zipWith max (bottomRight b) (bottomRight c)
        }

width :: Num a => Box a -> a
width b = v2X (bottomRight b) - v2X (topLeft b)

height :: Num a => Box a -> a
height b = v2Y (bottomRight b) - v2Y (topLeft b)

fromV2 :: V2 a -> Box a
fromV2 p = Box p p
