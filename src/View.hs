-- | View instances for the Paste app.
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module View where
#if 0
import Control.Lens
import Data.Default (Default(def))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Language.Haskell.TH.Path.View (View(..))
import MIMO.Base (TextFormat(..))

import Stage1

-- | The Paste type, flattened to produce a better UI.
data Paste' =
    Paste'
    { title'    :: Text
    , nickname' :: Text
    , format'   :: TextFormat
    , pasted'   :: UTCTime
    , pasteId'  :: PasteId
    , paste'    :: Text
    } deriving (Show)

instance View Paste where
    type ViewType Paste = Paste'
    viewLens =
        iso (\Paste{paste = t, pasteId = i, pasteMeta = PasteMeta{..}} ->
                 Paste' { title' = title
                        , nickname' = nickname
                        , format' = format
                        , pasted' = pasted
                        , pasteId' = i
                        , paste' = t })
            (\Paste'{..} ->
                 Paste { pasteMeta = PasteMeta { title = title'
                                               , nickname = nickname'
                                               , format = format'
                                               , pasted = pasted' }
                       , paste = paste'
                       , pasteId = pasteId' })

instance Default Paste' where
    def = view (viewLens :: Lens' Paste Paste') (def :: Paste)
#endif
