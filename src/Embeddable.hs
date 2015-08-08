{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Embeddable where

import App.Stage1 (App')
import Data.Time (UTCTime)
import HSP (EmbedAsChild(asChild))
import MIMO.ListForm (Column(Column))

instance EmbedAsChild App' (Column UTCTime) where
    asChild (Column x) = asChild x
