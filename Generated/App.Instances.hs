instance SafeCopy Paste
    where putCopy (Paste arg
                         arg
                         arg) = contain (do {safePut_PasteId <- getSafePut;
                                             safePut_PasteMeta <- getSafePut;
                                             safePut_Text <- getSafePut;
                                             safePut_PasteId arg;
                                             safePut_PasteMeta arg;
                                             safePut_Text arg;
                                             return ()})
          getCopy = contain (label "Stage1Def.Paste:" (do {safeGet_PasteId <- getSafeGet;
                                                           safeGet_PasteMeta <- getSafeGet;
                                                           safeGet_Text <- getSafeGet;
                                                           ((return Paste <*> safeGet_PasteId) <*> safeGet_PasteMeta) <*> safeGet_Text}))
          version = 0
          kind = base
          errorTypeName _ = "Stage1Def.Paste"
instance SafeCopy PasteId
    where putCopy (PasteId arg) = contain (do {safePut_Integer <- getSafePut;
                                               safePut_Integer arg;
                                               return ()})
          getCopy = contain (label "Stage1Def.PasteId:" (do {safeGet_Integer <- getSafeGet;
                                                             return PasteId <*> safeGet_Integer}))
          version = 0
          kind = base
          errorTypeName _ = "Stage1Def.PasteId"
instance SafeCopy PasteMeta
    where putCopy (PasteMeta arg
                             arg
                             arg
                             arg) = contain (do {safePut_Text <- getSafePut;
                                                 safePut_TextFormat <- getSafePut;
                                                 safePut_UTCTime <- getSafePut;
                                                 safePut_Text arg;
                                                 safePut_Text arg;
                                                 safePut_TextFormat arg;
                                                 safePut_UTCTime arg;
                                                 return ()})
          getCopy = contain (label "Stage1Def.PasteMeta:" (do {safeGet_Text <- getSafeGet;
                                                               safeGet_TextFormat <- getSafeGet;
                                                               safeGet_UTCTime <- getSafeGet;
                                                               (((return PasteMeta <*> safeGet_Text) <*> safeGet_Text) <*> safeGet_TextFormat) <*> safeGet_UTCTime}))
          version = 0
          kind = base
          errorTypeName _ = "Stage1Def.PasteMeta"
instance Default Paste
    where def = Paste def def def
instance Default PasteId
    where def = PasteId def
instance Default PasteMeta
    where def = PasteMeta def def def def
instance Default PasteV
    where def = PasteV def def def def def def
instance Default Day
    where def = ModifiedJulianDay def
instance Default DiffTime
    where def = MkDiffTime def
