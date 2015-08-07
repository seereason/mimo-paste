(elt "ul" <@ ("class" := "menu" :: Attr AppText
                                        AppText)) <: (elt "li" <: ((elt "a" <@ ("href" := AppURL SomePastes :: Attr AppText
                                                                                                                    (URL AppURL))) <: fromStringLit "Pastes"))