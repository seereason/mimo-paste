{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Stage2 where

-- import Stage0
import App.Stage1 (App')
import Data.Time (UTCTime)
import HSP (EmbedAsChild(asChild))
import MIMO.ListForm (Column(Column))

instance EmbedAsChild App' (Column UTCTime) where
    asChild (Column x) = asChild x
