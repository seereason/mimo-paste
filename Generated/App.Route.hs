route :: (AuthenticateURL ->
          RouteT AuthenticateURL (ServerPartT IO) Response) ->
         Text -> URL AppURL -> App Response
route routeAuthenticate _baseURL theurl = case theurl of
                                              Authenticate authURL -> lift $ (nestURL Authenticate $ (mapRouteT lift $ routeAuthenticate authURL))
                                              AppJs -> ok $ (toResponse $ appJs)
                                              CSS -> serveFile (asContentType "text/css") "style.css"
                                              JQuery -> serveDirectory DisableBrowsing [] "/usr/share/javascript/jquery/"
                                              JQueryUI -> serveDirectory DisableBrowsing [] "/usr/share/javascript/jquery-ui/"
                                              AppURL appurl -> case appurl of
                                                                   SomePastes -> listPage (undefined :: Paste)
                                                                   ViewPaste pid -> viewPage (undefined :: Paste) pid
                                                                   CreatePaste -> createPage (undefined :: Paste)
                                                                   UpdatePaste pid -> updatePage (undefined :: Paste) pid
