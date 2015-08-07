instance Row Paste
    where type AppType Paste = App
          type IdType Paste = PasteId
          createPage _ = do {here <- whereami;
                             mUserId <- currentUser;
                             appTemplate ("Add a " <> "Paste") () $ asChild [genElement (Nothing,
                                                                                         fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := False)] [asChild $ ([asChild menuList,
                                                                                                                                                                                asChild (fromStringLit " ")] <> (loginForm <> maybe [asChild (fromStringLit " ")] (\_ -> [asChild (reform (form here) "add" (maybe (seeOtherURL (AppURL SomePastes)) (\x -> do {xid <- update (CreatePasteEvent x);
                                                                                                                                                                                                                                                                                                                                                                seeOtherURL (AppURL (ViewPaste xid))})) Nothing (createForm :: AppForm (Maybe Paste))),
                                                                                                                                                                                                                                                                          asChild (fromStringLit " ")]) mUserId))],
                                                                             genElement (Nothing,
                                                                                         fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := True)] [asChild $ ([asChild menuList,
                                                                                                                                                                               asChild (fromStringLit " "),
                                                                                                                                                                               asChild (elt "h1" <: fromStringLit (maybe "You Are Not Logged In" (const "Add a Paste") mUserId))] ++ maybe [asChild (fromStringLit " ")] (\_ -> [asChild (reform (form here) "add" (maybe (seeOtherURL (AppURL SomePastes)) (\x -> do {xid <- update (CreatePasteEvent x);
                                                                                                                                                                                                                                                                                                                                                                                                                       seeOtherURL (AppURL (ViewPaste xid))})) Nothing (createForm :: AppForm (Maybe Paste))),
                                                                                                                                                                                                                                                                                                                                 asChild (fromStringLit " ")]) mUserId)]]}
          listPage _ = do {recent <- query (SomePastesEvent (20 :: Int) (0 :: Int));
                           liftIO $ logM "List" DEBUG ("EVENT " ++ ("GetPasteByIdEvent" ++ (" " ++ show recent)));
                           here <- whereami;
                           mUserId <- currentUser;
                           appTemplate ("List of " <> "Paste") () $ asChild [genElement (Nothing,
                                                                                         fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := False)] [asChild $ ([asChild menuList,
                                                                                                                                                                                asChild (fromStringLit " ")] ++ (loginForm <> maybe [asChild (fromStringLit " ")] (\_ -> [asChild (reform (form here) "list" (maybe (seeOtherURL (AppURL SomePastes)) (\xs' -> do {liftIO $ logM "Delete" DEBUG ("EVENT " ++ ("DeletePasteEvent" ++ (" " ++ show (map pasteId xs'))));
                                                                                                                                                                                                                                                                                                                                                                   _ <- mapM (update . DeletePasteEvent) xs';
                                                                                                                                                                                                                                                                                                                                                                   seeOtherURL (AppURL SomePastes)})) Nothing (listForm recent :: AppForm (Maybe ([Paste])))),
                                                                                                                                                                                                                                                                          asChild (fromStringLit " ")]) mUserId))],
                                                                             genElement (Nothing,
                                                                                         fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := True)] ([asChild $ ([asChild menuList,
                                                                                                                                                                                asChild (fromStringLit " "),
                                                                                                                                                                                asChild (fromStringLit (show mUserId)),
                                                                                                                                                                                asChild (elt "h1" <: fromStringLit "Pastes:")] <> maybe [asChild (fromStringLit " ")] (const [asChild (reform (form here) "list" (maybe (seeOtherURL (AppURL SomePastes)) (\xs' -> do {liftIO $ logM "Delete" DEBUG ("EVENT " ++ ("DeletePasteEvent" ++ (" " ++ show (map pasteId xs'))));
                                                                                                                                                                                                                                                                                                                                                                       _ <- mapM (update . DeletePasteEvent) xs';
                                                                                                                                                                                                                                                                                                                                                                       seeOtherURL (AppURL SomePastes)})) Nothing (listForm recent :: AppForm (Maybe ([Paste])))),
                                                                                                                                                                                                                                                                              asChild (fromStringLit " ")]) mUserId)] <> logoutForm mUserId)]}
          updatePage _ xid = do {mRow <- query (GetPasteByIdEvent xid);
                                 case mRow of
                                     Nothing -> do {notFound ();
                                                    appTemplate ("Paste" <> " not found.") () $ ((((elt "p" <: menuList) <: fromStringLit "Paste ") <: asChild xid) <: fromStringLit " could not be found.")}
                                     Just x -> do {here <- whereami;
                                                   mUserId <- currentUser;
                                                   let success :: Maybe Paste -> App Response
                                                       success (Nothing) = seeOtherURL (AppURL SomePastes)
                                                       success (Just x') = do {xid' <- update (UpdatePasteEvent x');
                                                                               seeOtherURL (AppURL (ViewPaste xid'))}
                                                       updateHeader (Just _) = (elt "h1" <: fromStringLit "Update Paste ") <: asChild xid
                                                       updateHeader (Nothing) = elt "h1" <: fromStringLit "You Are Not Logged In"
                                                       updateForm' :: Maybe UserId ->
                                                                      Paste -> [GenChildList App']
                                                       updateForm' (Just _) x' = [asChild (reform (form here) "update" success Nothing (updateForm x' :: AppForm (Maybe Paste))),
                                                                                  asChild (fromStringLit " ")]
                                                       updateForm' (Nothing) _ = [asChild (fromStringLit " ")];
                                                   appTemplate ("Update a " <> "Paste") () $ (asChild $ ([asChild menuList,
                                                                                                          asChild (fromStringLit " "),
                                                                                                          asChild (updateHeader mUserId)] ++ updateForm' mUserId x))}}
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
                                                                                                                                                                                                                                                                                                                                                                                                                                            (URL AppURL))) <: fromStringLit ("update this " ++ "Paste"))))}
                                       where mkDescList x = map (\f -> f x) [\_ -> asChild (elt "dt" <: fromStringLit "Paste Id:"),
                                                                             \x -> asChild (elt "dd" <: asChild ((\x -> view lensPasteId x :: PasteId) x)),
                                                                             \_ -> asChild (elt "dt" <: fromStringLit "Title:"),
                                                                             \x -> asChild (elt "dd" <: asChild ((\x -> view (lensPasteMeta . lensTitle) x :: Text) x)),
                                                                             \_ -> asChild (elt "dt" <: fromStringLit "Nickname:"),
                                                                             \x -> asChild (elt "dd" <: asChild ((\x -> view (lensPasteMeta . lensNickname) x :: Text) x)),
                                                                             \_ -> asChild (elt "dt" <: fromStringLit "Format:"),
                                                                             \x -> asChild (elt "dd" <: asChild ((\x -> view (lensPasteMeta . lensFormat) x :: TextFormat) x)),
                                                                             \_ -> asChild (elt "dt" <: fromStringLit "Pasted:"),
                                                                             \x -> asChild (elt "dd" <: asChild ((\x -> view (lensPasteMeta . lensPasted) x :: UTCTime) x))] :: [GenChildList App']
                                             mkDiv x = map (\f -> f x) [\x -> asChild (doTextFormat (getTextFormat x) (appPack (unpack ((\x -> view lensPaste x :: Text) x))))] :: [GenChildList App']}
          updateForm v = (fieldset $ (ul $ (((\v' t -> if t == pack "cancel"
                                                        then Nothing
                                                        else Just v') <$> appForm (Just v)) <*> (((\u c -> fromMaybe (fromMaybe (error "No button pushed!") u) c) <$> inputSubmit "update") <*> inputSubmit "cancel")))) `transformEitherM` maybe (return $ (Right $ Nothing)) (return . (Right . Just)) :: AppForm (Maybe Paste)
          createForm = (fieldset $ (ul $ (((\v' t -> if t == pack "cancel"
                                                      then Nothing
                                                      else Just v') <$> appForm (Just def)) <*> (((\u c -> fromMaybe (fromMaybe (error "No button pushed!") u) c) <$> inputSubmit "create") <*> inputSubmit "cancel")))) `transformEitherM` maybe (return $ (Right $ Nothing)) (return . (Right . Just)) :: AppForm (Maybe Paste)
