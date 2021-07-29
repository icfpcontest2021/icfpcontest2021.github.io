module Text.XML.Light.Extended
    ( module Text.XML.Light
    , set_attr
    ) where

import Text.XML.Light

set_attr :: Attr -> Element -> Element
set_attr (Attr key val) el = el {elAttribs = go (elAttribs el)}
  where
    go []           = [Attr key val]
    go (Attr k v : attrs)
        | k == key  = go attrs
        | otherwise = Attr k v : go attrs
