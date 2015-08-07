{-# LANGUAGE TemplateHaskell #-}
-- | This is here so that MIMO.Main can say import App and get the app
-- specific spec value implied by Hs-Source-Dirs.
module App
    ( module Types
    , theAppInfo
    ) where

import Types
import Spec (spec)
import Data.Time.Clock (UTCTime)
import MIMO.App (AppInfo(..))

theAppInfo :: AppInfo
theAppInfo
    = AppInfo
      { _spec = spec
      , _idField =
          let f n | n == ''Paste = Just (''PasteId, 'pasteId)
              f n | n == ''PasteMeta = Nothing
              f _ = Nothing
          in f
      , _indexTypes =
          let f name | name == ''Paste = [''UTCTime]
              f _ = []
          in f
      , _hints = hints
      , _typeNames = [''Paste, ''PasteMeta] }
