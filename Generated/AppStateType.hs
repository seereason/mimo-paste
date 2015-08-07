data AppState
    = AppState {pastes :: (IxSet Paste), nextPasteId :: PasteId}
    deriving (Data, Typeable)
