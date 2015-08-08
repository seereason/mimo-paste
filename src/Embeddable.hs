{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Embeddable where

import Data.Time (UTCTime)
import HSP (EmbedAsChild(asChild))
import MIMO.ListForm (Column(Column))
import App.Types (App')

instance EmbedAsChild App' (Column UTCTime) where
    asChild (Column x) = asChild x
