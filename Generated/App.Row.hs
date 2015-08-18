instance Row Paste
    where type AppType Paste = App
          type AppType' Paste = App'
          type AppURLType Paste = URL AppURL
          type AppFormType Paste = AppForm
          type IdType Paste = PasteId
          createForm _ = do {here <- whereami;
                             frm <- updForm (Just def);
                             liftIO $ logM "Create" DEBUG ("EVENT " ++ "GetPasteByIdEvent");
                             reform (form here) "add" (maybe (seeOtherURL (AppURL SomePastes)) (\x -> do {xid <- update (CreatePasteEvent x);
                                                                                                          seeOtherURL (AppURL (ViewPaste xid))})) Nothing (createForm' frm :: AppFormType Paste
                                                                                                                                                                                          (Maybe Paste))}
          listForm _ = do {here <- whereami;
                           recent <- query (SomePastesEvent (20 :: Int) (0 :: Int));
                           liftIO $ logM "List" DEBUG ("EVENT " ++ ("GetPasteByIdEvent" ++ (" " ++ show recent)));
                           reform (form here) "list" (maybe (seeOtherURL (AppURL SomePastes)) (\xs' -> do {liftIO $ logM "Delete" DEBUG ("EVENT " ++ ("DeletePasteEvent" ++ (" " ++ show (map pasteId xs'))));
                                                                                                           _ <- mapM (update . DeletePasteEvent) xs';
                                                                                                           seeOtherURL (AppURL SomePastes)})) Nothing (listForm' recent :: AppFormType Paste
                                                                                                                                                                                       (Maybe ([Paste])))}
          updateForm x = do {here <- whereami;
                             liftIO $ logM "Update" DEBUG ("EVENT " ++ ("GetPasteByIdEvent" ++ (" " ++ show x)));
                             frm <- updForm (Just x);
                             reform (form here) "update" (maybe (seeOtherURL (AppURL SomePastes)) (\x' -> do {xid' <- update (UpdatePasteEvent x');
                                                                                                              seeOtherURL (AppURL (ViewPaste xid'))})) Nothing (updateForm' frm :: AppFormType Paste
                                                                                                                                                                                               (Maybe Paste))}
          createPage _ = do {mUserId <- currentUser;
                             appTemplate ("Add a " <> "Paste") () $ asChild [genElement (Nothing,
                                                                                         fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := False)] [asChild $ ([asChild menuList,
                                                                                                                                                                                asChild (fromStringLit " ")] <> loginForm)],
                                                                             genElement (Nothing,
                                                                                         fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := True)] [asChild $ [asChild menuList,
                                                                                                                                                                              asChild (fromStringLit " "),
                                                                                                                                                                              asChild (elt "h1" <: fromStringLit (maybe "You Are Not Logged In" (const "Add a Paste") mUserId)),
                                                                                                                                                                              asChild (createForm (undefined :: Paste) :: AppType Paste
                                                                                                                                                                                                                                  ([AppType Paste
                                                                                                                                                                                                                                            XML]))]]]}
          listPage _ = do {mUserId <- currentUser;
                           appTemplate ("List of " <> "Paste") () $ asChild [genElement (Nothing,
                                                                                         fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := False)] [asChild $ ([asChild menuList,
                                                                                                                                                                                asChild (fromStringLit " ")] ++ (loginForm <> maybe [asChild (fromStringLit " ")] (\_ -> [asChild (listForm (undefined :: Paste)),
                                                                                                                                                                                                                                                                          asChild (fromStringLit " ")]) mUserId))],
                                                                             genElement (Nothing,
                                                                                         fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := True)] ([asChild $ ([asChild menuList,
                                                                                                                                                                                asChild (fromStringLit " "),
                                                                                                                                                                                asChild (fromStringLit (show mUserId)),
                                                                                                                                                                                asChild (elt "h1" <: fromStringLit "Pastes:")] <> maybe [asChild (fromStringLit " ")] (const [asChild (listForm (undefined :: Paste)),
                                                                                                                                                                                                                                                                              asChild (fromStringLit " ")]) mUserId)] <> logoutForm mUserId)]}
          updatePage _ xid = do {mRow <- query (GetPasteByIdEvent xid);
                                 case mRow of
                                     Nothing -> do {notFound ();
                                                    appTemplate ("Paste" <> " not found.") () $ ((((elt "p" <: menuList) <: fromStringLit "Paste ") <: asChild xid) <: fromStringLit " could not be found.")}
                                     Just x -> do {mUserId <- currentUser;
                                                   appTemplate ("Update a " <> "Paste") () $ (asChild $ ([asChild menuList,
                                                                                                          asChild (fromStringLit " "),
                                                                                                          asChild (maybe (elt "h1" <: fromStringLit "You Are Not Logged In") (\_ -> (elt "h1" <: fromStringLit "Update Paste ") <: asChild xid) mUserId)] ++ maybe [asChild (fromStringLit " ")] (\_ -> [asChild (updateForm x),
                                                                                                                                                                                                                                                                                                         asChild (fromStringLit " ")]) mUserId))}}
          viewPage _ xid = do {method GET;
                               mRow <- query (GetPasteByIdEvent xid);
                               case mRow of
                                   Nothing -> do {notFound ();
                                                  appTemplate ("Paste" <> " not found.") () $ ((((elt "p" <: menuList) <: fromStringLit "Paste ") <: asChild xid) <: fromStringLit " could not be found.")}
                                   Just x -> do {ok ();
                                                 appTemplate (appPack $ ("Paste" <> (" " <> (show $ (unPasteId $ (pasteId $ x)))))) () $ (((((elt "div" <@ ("class" := "row" :: Attr AppText
                                                                                                                                                                                     AppText)) <: menuList) <: ((elt "dl" <@ ("class" := "row-header" :: Attr AppText
                                                                                                                                                                                                                                                              AppText)) <: mkDescList x)) <: ((elt "div" <@ ("class" := "row-body" :: Attr AppText
                                                                                                                                                                                                                                                                                                                                           AppText)) <: mkDiv x)) <: (elt "div" <: ((elt "a" <@ ("href" := AppURL (UpdatePaste xid) :: Attr AppText
                                                                                                                                                                                                                                                                                                                                                                                                                                            (AppURLType Paste))) <: fromStringLit ("update this " ++ "Paste"))))}
                                       where mkDescList x = map (\f -> f x) [\_ -> asChild (elt "dt" <: fromStringLit "Paste Id:"),
                                                                             \x -> asChild (elt "dd" <: asChild ((\x -> view lensPasteId x :: PasteId) x)),
                                                                             \_ -> asChild (elt "dt" <: fromStringLit "Title:"),
                                                                             \x -> asChild (elt "dd" <: asChild ((\x -> view (lensPasteMeta . lensTitle) x :: Text) x)),
                                                                             \_ -> asChild (elt "dt" <: fromStringLit "Nickname:"),
                                                                             \x -> asChild (elt "dd" <: asChild ((\x -> view (lensPasteMeta . lensNickname) x :: Text) x)),
                                                                             \_ -> asChild (elt "dt" <: fromStringLit "Format:"),
                                                                             \x -> asChild (elt "dd" <: asChild ((\x -> view (lensPasteMeta . lensFormat) x :: TextFormat) x)),
                                                                             \_ -> asChild (elt "dt" <: fromStringLit "Pasted:"),
                                                                             \x -> asChild (elt "dd" <: asChild ((\x -> view (lensPasteMeta . lensPasted) x :: UTCTime) x))] :: [GenChildList (AppType' Paste)]
                                             mkDiv x = map (\f -> f x) [\x -> asChild (doTextFormat (getTextFormat x) (appPack (unpack ((\x -> view lensPaste x :: Text) x))))] :: [GenChildList (AppType' Paste)]}
          updateForm' frm = (fieldset $ (ul $ (((\v' t -> if t == pack "cancel"
                                                           then Nothing
                                                           else Just v') <$> (frm :: AppFormType Paste
                                                                                                 Paste)) <*> (((\u c -> fromMaybe (fromMaybe (error "No button pushed!") u) c) <$> inputSubmit "update") <*> inputSubmit "cancel")))) `transformEitherM` maybe (return $ (Right $ Nothing)) (return . (Right . Just)) :: AppFormType Paste
                                                                                                                                                                                                                                                                                                                                     (Maybe Paste)
          createForm' frm = (fieldset $ (ul $ (((\v' t -> if t == pack "cancel"
                                                           then Nothing
                                                           else Just v') <$> (frm :: AppFormType Paste
                                                                                                 Paste)) <*> (((\u c -> fromMaybe (fromMaybe (error "No button pushed!") u) c) <$> inputSubmit "create") <*> inputSubmit "cancel")))) `transformEitherM` maybe (return $ (Right $ Nothing)) (return . (Right . Just)) :: AppFormType Paste
                                                                                                                                                                                                                                                                                                                                     (Maybe Paste)
