instance Updatable App (TextArea Text)
    where updForm x = (textarea 80 12 (unparse (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                        0) (0,
                                                                                                            0)))) x)) `transformEither` parse (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                                                                                       0) (0,
                                                                                                                                                                                                           0)))) x)) <++ errorList :: AppForm (TextArea Text)
          updForm' = pure . updForm
instance Updatable App TextFormat
    where updForm = \x -> select [(y,
                                   show y) | y <- [minBound..maxBound]] (== fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                    0) (0,
                                                                                                                                        0)))) x) :: AppForm TextFormat
          updForm' = pure . updForm
instance Updatable App Paste
    where updForm x = (set viewLens <$> updForm (Just (view viewLens (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                              0) (0,
                                                                                                                                  0)))) x) :: Paste'))) <*> pure (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                                                                                                          0) (0,
                                                                                                                                                                                                                              0)))) x)
          updForm' = pure . updForm
instance Updatable App Paste'
    where updForm = \x -> mapView (\v -> [elt "span" <: fromStringLit "Paste'"] <> v) ((\(Just (Paste' _a₁
                                                                                                       _a₂
                                                                                                       _a₃
                                                                                                       _a₄
                                                                                                       _a₅
                                                                                                       _a₆)) -> (ul :: AppForm Paste' ->
                                                                                                                       AppForm Paste') ((((((pure Paste' <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Paste Id'" ++ ": ")) <: form]) (updForm x)) (Just _a₁)) <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Title'" ++ ": ")) <: form]) (updForm x)) (Just _a₂)) <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Nickname'" ++ ": ")) <: form]) (updForm x)) (Just _a₃)) <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Format'" ++ ": ")) <: form]) (updForm x)) (Just _a₄)) <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Pasted'" ++ ": ")) <: form]) (updForm x)) (Just _a₅)) <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Paste'" ++ ": ")) <: form]) (updForm x)) (Just _a₆)) :: Maybe Paste' ->
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    AppForm Paste') x) :: Maybe Paste' ->
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          AppForm Paste'
          updForm' = pure . updForm
instance Updatable App PasteId
    where updForm = \x -> fmap (const (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                               0) (0,
                                                                                                   0)))) x)) (inputHidden empty :: AppForm Text)
          updForm' = pure . updForm
instance Updatable App PasteMeta
    where updForm = \x -> mapView (\v -> [elt "span" <: fromStringLit "Paste Meta"] <> v) ((\(Just (PasteMeta _a₁
                                                                                                              _a₂
                                                                                                              _a₃
                                                                                                              _a₄)) -> (ul :: AppForm PasteMeta ->
                                                                                                                              AppForm PasteMeta) ((((pure PasteMeta <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Title" ++ ": ")) <: form]) (updForm x)) (Just _a₁)) <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Nickname" ++ ": ")) <: form]) (updForm x)) (Just _a₂)) <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Format" ++ ": ")) <: form]) (updForm x)) (Just _a₃)) <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Pasted" ++ ": ")) <: form]) (updForm x)) (Just _a₄)) :: Maybe PasteMeta ->
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        AppForm PasteMeta) x) :: Maybe PasteMeta ->
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 AppForm PasteMeta
          updForm' = pure . updForm
instance Updatable App Text
    where updForm x = (inputText (unparse (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                   0) (0,
                                                                                                       0)))) x)) `transformEither` parse (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                                                                                  0) (0,
                                                                                                                                                                                                      0)))) x)) <++ errorList :: AppForm Text
          updForm' = pure . updForm
