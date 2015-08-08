{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Stage2Def where

-- import Stage1Def
import Stage1Gen (App')
import Data.Time (UTCTime)
import HSP (EmbedAsChild(asChild))
import MIMO.ListForm (Column(Column))

instance EmbedAsChild App' (Column UTCTime) where
    asChild (Column x) = asChild x
