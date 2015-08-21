-- | This module is the authoritative source of all information
-- used to generate the Paste web site.
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Stage1Def where

import Control.Lens(iso)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Distribution.License (License(..))
import Happstack.Authenticate.Core (UserId(..))
import Happstack.Foundation (Data, PathInfo, Typeable)
import Language.Haskell.TH
import Language.Haskell.TH.Path.Graph (SinkType)
import Language.Haskell.TH.Path.View (View(..))
import Language.Haskell.TH.TypeGraph.Shape (fName)
import Language.Haskell.TH.TypeGraph.Stack (StackElement(StackElement), TypeStack(_typeStack))
import MIMO.App (AppInfo(..))
import MIMO.Base (TextFormat(..), version)
import MIMO.Hint (Hint(Flatten, HideColumn, Div, HideField, Link, TimeStamp), TextArea(..))
import MIMO.Id (IdField(idField))
import MIMO.Parsable ()
import MIMO.Spec (Spec(..))
import Ports (paste)
import Prelude hiding (pi)

newtype PasteId = PasteId {unPasteId :: Integer} deriving (Enum, Eq, Ord, Show, Data, Typeable, PathInfo)
instance SinkType PasteId

data Paste = Paste
    { pasteId :: PasteId
    , pasteMeta :: PasteMeta
    , paste :: Text
    } deriving (Eq, Ord, Show, Data, Typeable)

data PasteMeta = PasteMeta
    { title :: Text
    , nickname :: Text
    , format :: TextFormat
    , pasted :: UTCTime
    } deriving (Eq, Ord, Show, Data, Typeable)

{-
keyType :: Name -> KeyType
keyType typeName | typeName == ''Paste = Private
keyType _ = NoKey
-}

instance SinkType Text
instance SinkType Integer
instance SinkType UTCTime
instance SinkType UserId

-- | The Paste type, flattened for a better UI, and with the paste
-- field wrapped with TextArea
data Paste' =
    Paste'
    { pasteId' :: PasteId
    , title' :: Text
    , nickname' :: Text
    , format' :: TextFormat
    , pasted' :: UTCTime
    , paste' :: TextArea Text
    } deriving (Show)

instance View Paste where
    type ViewType Paste = Paste'
    viewLens =
        iso (\p ->
                 Paste'
                 { title' = title (pasteMeta p)
                 , nickname' = nickname (pasteMeta p)
                 , format' = format (pasteMeta p)
                 , pasted' = pasted (pasteMeta p)
                 , pasteId' = pasteId p
                 , paste' = TextArea (Stage1Def.paste p) })
            (\v ->
                 Paste
                 { pasteId = pasteId' v
                 , pasteMeta =
                     PasteMeta
                     { title = title' v
                     , nickname = nickname' v
                     , format = format' v
                     , pasted = pasted' v }
                 , Stage1Def.paste = _unTextArea (paste' v) })

theAppInfo :: AppInfo
theAppInfo
    = AppInfo
      { _spec = theSpec
      , _indexTypes =
          let f name | name == ''Paste = [''UTCTime]
              f _ = []
          in f
      , _hints = theHints }

instance IdField Paste PasteId where
    idField _ = [|pasteId|]

theSpec :: Spec
theSpec =
    Spec { siteName = "Paste"
         , siteVersion = version "1.2.3"
         , siteHomepage = "homepage"
         , siteAuthor = "author"
         , siteLicense = PublicDomain
         , siteSynopsis = "synopsis"
         , siteDescription = "description"
         , siteOwner = UserId 1
         , siteDomain = "algebrazam.com"
         , siteTestHost = Nothing
         , sitePorts = Ports.paste
         , siteAdmin = "logic@seereason.com"
         , siteParent = "/srv"
         , siteBackupsDir = "/srv/backups"
         , siteBackupsUser = "upload"
         , siteRowTypes = [''Paste, ''PasteMeta] }

theHints :: TypeStack -> [Hint]
theHints =
    concatMap hints' . _typeStack
    where
      hints' (StackElement fld _con dec) = decHints dec ++ fieldHints fld
      decHints (TySynD _ _ _) = []
      decHints (NewtypeD _ name _ _ _) = typeNameHints name
      decHints (DataD _ name _ _ _) = typeNameHints name
      decHints _ = []
      typeNameHints x | x == ''PasteMeta = [Flatten]
      typeNameHints _ = []
      fieldHints fld =
          case fName fld of
            Right x | x == 'Stage1Def.paste -> [HideColumn, Div]
            Right x | x == 'pasteId -> [HideField]
            Right x | x == 'title -> [Link]
            Right x | x == 'pasted -> [TimeStamp]
            _ -> []
