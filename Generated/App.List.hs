instance ListForm App' Paste
    where listForm' xs = (fieldset $ (((\xs' t -> if t == pack "cancel"
                                                   then Nothing
                                                   else Just xs') <$> mapView (\rows -> [(elt "table" <: (([asChild (elt "th" <: fromStringLit "Select")] :: [GenChildList App']) ++ headers (undefined :: Paste))) <: rows,
                                                                                         elt "hr",
                                                                                         elt "div" <: ((elt "a" <@ ("href" := AppURL CreatePaste :: Attr AppText
                                                                                                                                                         (URL AppURL))) <: fromStringLit "create a new Paste")]) (foldr (\xf xsf -> ((:) <$> xf) <*> xsf) (pure []) (case xs of
                                                                                                                                                                                                                                                                         [] -> [fmap (\(()) -> Nothing) (label (elt "tr" <: ((elt "td" <@ ("colspan" := "8" :: Attr AppText
                                                                                                                                                                                                                                                                                                                                                                    AppText)) <: fromStringLit "No Pastes found")))]
                                                                                                                                                                                                                                                                         _ -> map (\x -> fmap (\b -> if b
                                                                                                                                                                                                                                                                                                      then Just x
                                                                                                                                                                                                                                                                                                      else Nothing) (mapView (\xml -> [elt "tr" <: xml]) $ (mapView (\xml -> [elt "td" <: xml]) (inputCheckbox False) <++ label (columns x)))) xs))) <*> (((\d c -> fromMaybe (fromMaybe (error "No button?") d) c) <$> inputSubmit "delete selected") <*> inputSubmit "cancel"))) `transformEitherM` maybe (return $ (Right $ Nothing)) (return . (Right . (Just . catMaybes)))
          headers _ = [asChild (elt "th" <: fromStringLit "Paste Id") :: GenChildList App',
                       asChild (elt "th" <: fromStringLit "Title") :: GenChildList App',
                       asChild (elt "th" <: fromStringLit "Nickname") :: GenChildList App',
                       asChild (elt "th" <: fromStringLit "Format") :: GenChildList App',
                       asChild (elt "th" <: fromStringLit "Pasted") :: GenChildList App']
          columns = \p -> [asChild (elt "td" <: ((elt "a" <@ ("href" := AppURL (ViewPaste ((\x -> view lensPasteId x :: PasteId) p)) :: Attr AppText
                                                                                                                                             (URL AppURL))) <: asChild ((\x -> view lensPasteId x :: PasteId) p))),
                           asChild (elt "td" <: ((elt "a" <@ ("href" := AppURL (ViewPaste (pasteId p)) :: Attr AppText
                                                                                                               (URL AppURL))) <: asChild ((\x -> view (lensPasteMeta . lensTitle) x :: Text) p))),
                           asChild (elt "td" <: asChild ((\x -> view (lensPasteMeta . lensNickname) x :: Text) p)),
                           asChild (elt "td" <: asChild ((\x -> view (lensPasteMeta . lensFormat) x :: TextFormat) p))]
