{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Stage2aDef where

import Stage1Def
import Stage1Gen (App')
import Control.Lens(iso)
import Data.Text (Text)
import Data.Time (UTCTime)
import HSP (EmbedAsChild(asChild))
import Language.Haskell.TH.Path.View (View(..))
import MIMO.Base (TextFormat(..))
import MIMO.Hint (TextArea(TextArea, _unTextArea))
import MIMO.ListForm (Column(Column))

instance EmbedAsChild App' (Column UTCTime) where
    asChild (Column x) = asChild x
