instance (Functor m,
          Monad m) => EmbedAsAttr (RouteT AuthenticateURL m)
                                  (Attr AppText (URL AppURL))
    where asAttr (n := u) = asAttr $ MkAttr (toName n,
                                             pAttrVal (appPack $ concatMap ((++) "/" . appUnpack) (map (appPack . unpack) (toPathSegments u))))
