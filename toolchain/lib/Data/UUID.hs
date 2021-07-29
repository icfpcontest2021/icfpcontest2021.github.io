-- | Really an extension of `Data.UUID.Types`.
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.UUID
    ( module Data.UUID.Types
    ) where

import           Data.UUID.Types
import qualified Text.Blaze.Html5 as H

instance H.ToMarkup UUID where
    toMarkup = H.toMarkup . toText

instance H.ToValue UUID where
    toValue = H.toValue . toText
