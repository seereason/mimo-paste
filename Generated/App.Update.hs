instance Updatable App (TextArea Text)
    where updForm x = pure ((textarea 80 12 (unparse (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                              0) (0,
                                                                                                                  0)))) x)) `transformEither` parse (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                                                                                             0) (0,
                                                                                                                                                                                                                 0)))) x)) <++ errorList :: AppForm (TextArea Text))
instance Updatable App TextFormat
    where updForm = pure . (\x -> select [(y,
                                           show y) | y <- [minBound..maxBound]] (== fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                            0) (0,
                                                                                                                                                0)))) x) :: AppForm TextFormat)
instance Updatable App Paste
    where updForm mx = do {let v = view viewLens (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                          0) (0,
                                                                                                              0)))) mx) :: Paste';
                           vform <- updForm (Just v);
                           let xform = (set viewLens <$> vform) <*> pure (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                  0) (0,
                                                                                                                                      0)))) mx);
                           pure xform}
instance Updatable App Paste'
    where updForm = \x -> (\(Just (Paste' _a₁
                                          _a₂
                                          _a₃
                                          _a₄
                                          _a₅
                                          _a₆)) -> (ul :: AppForm Paste' ->
                                                          AppForm Paste') <$> (\a1 a2 a3 a4 a5 a6 -> ((((fmap (\a1 a2 a3 a4 a5 a6 -> ((((fmap Paste' a1 <*> a2) <*> a3) <*> a4) <*> a5) <*> a6) a1 <*> a2) <*> a3) <*> a4) <*> a5) <*> a6) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Paste Id'" ++ ": ")) <: form]))) (Just _a₁)) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Title'" ++ ": ")) <: form]))) (Just _a₂)) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Nickname'" ++ ": ")) <: form]))) (Just _a₃)) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Format'" ++ ": ")) <: form]))) (Just _a₄)) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Pasted'" ++ ": ")) <: form]))) (Just _a₅)) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Paste'" ++ ": ")) <: form]))) (Just _a₆))) x >>= (return . mapView (\v -> [elt "span" <: fromStringLit "Paste'"] <> v)) :: Maybe Paste' ->
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                App (AppForm Paste')
instance Updatable App PasteId
    where updForm = pure . (\x -> fmap (const (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                       0) (0,
                                                                                                           0)))) x)) (inputHidden empty :: AppForm Text))
instance Updatable App PasteMeta
    where updForm = \x -> (\(Just (PasteMeta _a₁
                                             _a₂
                                             _a₃
                                             _a₄)) -> (ul :: AppForm PasteMeta ->
                                                             AppForm PasteMeta) <$> (\a1 a2 a3 a4 -> ((fmap (\a1 a2 a3 a4 -> ((fmap PasteMeta a1 <*> a2) <*> a3) <*> a4) a1 <*> a2) <*> a3) <*> a4) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Title" ++ ": ")) <: form]))) (Just _a₁)) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Nickname" ++ ": ")) <: form]))) (Just _a₂)) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Format" ++ ": ")) <: form]))) (Just _a₃)) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Pasted" ++ ": ")) <: form]))) (Just _a₄))) x >>= (return . mapView (\v -> [elt "span" <: fromStringLit "Paste Meta"] <> v)) :: Maybe PasteMeta ->
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  App (AppForm PasteMeta)
instance Updatable App Text
    where updForm x = pure ((inputText (unparse (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                         0) (0,
                                                                                                             0)))) x)) `transformEither` parse (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                                                                                        0) (0,
                                                                                                                                                                                                            0)))) x)) <++ errorList :: AppForm Text)
