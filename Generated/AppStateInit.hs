initialAppState :: AppState
initialAppState = AppState{pastes = empty,
                           nextPasteId = PasteId (1 :: Integer)}
