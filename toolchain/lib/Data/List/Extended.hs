module Data.List.Extended
    ( module Data.List
    , unique
    ) where

import           Data.List

unique :: Eq a => [a] -> Bool
unique []       = True
unique (x : xs) = all (/= x) xs && unique xs
