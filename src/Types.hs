-- | This module is the authoritative source of all information
-- used to generate the Paste web site.
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Types where

import Control.Lens(iso)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Happstack.Authenticate.Core (UserId)
import Happstack.Foundation (Data, PathInfo, Typeable)
import Language.Haskell.TH
import Language.Haskell.TH.Path.Graph (SinkType)
import Language.Haskell.TH.Path.View (View(..))
import Language.Haskell.TH.TypeGraph.Shape (fName)
import Language.Haskell.TH.TypeGraph.Stack (StackElement(StackElement))
import MIMO.Base (TextFormat(..))
import MIMO.Hint (Hint(Flatten, HideColumn, Div, HideField, Link, TimeStamp), TextArea(..))
import MIMO.Parsable ()
--import MIMO.Rec (KeyType(..))
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

hints :: [StackElement] -> [Hint]
hints =
    concatMap hints'
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
            Right x | x == 'paste -> [HideColumn, Div]
            Right x | x == 'pasteId -> [HideField]
            Right x | x == 'title -> [Link]
            Right x | x == 'pasted -> [TimeStamp]
            _ -> []

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
                 , paste' = TextArea (paste p) })
            (\v ->
                 Paste
                 { pasteId = pasteId' v
                 , pasteMeta =
                     PasteMeta
                     { title = title' v
                     , nickname = nickname' v
                     , format = format' v
                     , pasted = pasted' v }
                 , paste = _unTextArea (paste' v) })
