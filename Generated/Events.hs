getPasteByIdEvent :: PasteId -> Query AppState (Maybe Paste)
getPasteByIdEvent i = (getOne . (getEQ i . pastes)) <$> ask
createPasteEvent :: Paste -> Update AppState PasteId
createPasteEvent p = do {cvs <- get;
                         let pid' = nextPasteId cvs;
                         let p' = p{pasteId = pid'};
                         put cvs{pastes = insert p' (pastes cvs), nextPasteId = succ pid'};
                         return pid'}
updatePasteEvent :: Paste -> Update AppState PasteId
updatePasteEvent p = do {cvs <- get;
                         put cvs{pastes = updateIx (pasteId p) p (pastes cvs)};
                         return (pasteId p)}
deletePasteEvent :: Paste -> Update AppState ()
deletePasteEvent p = do {cvs <- get;
                         let _pid = pasteId p;
                         put cvs{pastes = delete p (pastes cvs)}}
somePastesEvent :: Int -> Int -> Query AppState ([Paste])
somePastesEvent limit offset = do {cvs <- ask;
                                   return $ ((if limit > 1
                                               then take limit
                                               else id) $ (drop offset $ toDescList (Proxy :: Proxy UTCTime) (pastes cvs)))}
