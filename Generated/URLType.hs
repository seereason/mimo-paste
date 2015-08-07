data AppURL
    = ViewPaste PasteId
    | SomePastes
    | CreatePaste
    | UpdatePaste PasteId
    deriving (Eq, Ord, Show, Data, Typeable)
instance Default AppURL
    where def = SomePastes
