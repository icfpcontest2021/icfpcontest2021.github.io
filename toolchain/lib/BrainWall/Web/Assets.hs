{-# LANGUAGE TemplateHaskell #-}
module BrainWall.Web.Assets
    ( judges_svg
    , style_css
    ) where

import           Data.ByteString (ByteString)
import           Data.FileEmbed

judges_svg :: ByteString
judges_svg = $(embedFile "assets/judges.svg")

style_css :: ByteString
style_css = $(embedFile "assets/style.css")
