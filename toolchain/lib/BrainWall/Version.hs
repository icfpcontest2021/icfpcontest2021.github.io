{-# LANGUAGE TemplateHaskell #-}
module BrainWall.Version
    ( version
    ) where

import           Control.Monad.Trans (liftIO)
import qualified Language.Haskell.TH as TH
import           System.Process      (readProcess)

version :: String
version = $(do
    hash <- liftIO $ readProcess "git" ["rev-parse", "HEAD"] ""
    pure . TH.LitE . TH.StringL $ take 8 hash)
