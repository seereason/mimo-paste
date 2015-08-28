lensPaste :: forall . Lens' Paste Text
lensPaste f (Paste x x x) = fmap (\y -> Paste x x y) (f x)
{-# INLINE lensPaste #-}
lensPasteId :: forall . Lens' Paste PasteId
lensPasteId f (Paste x x x) = fmap (\y -> Paste y x x) (f x)
{-# INLINE lensPasteId #-}
lensPasteMeta :: forall . Lens' Paste PasteMeta
lensPasteMeta f (Paste x x x) = fmap (\y -> Paste x y x) (f x)
{-# INLINE lensPasteMeta #-}
lensUnPasteId :: forall . Iso' PasteId Integer
lensUnPasteId = iso (\(PasteId x) -> x) PasteId
{-# INLINE lensUnPasteId #-}
lensFormat :: forall . Lens' PasteMeta TextFormat
lensFormat f (PasteMeta x
                        x
                        x
                        x) = fmap (\y -> PasteMeta x x y x) (f x)
{-# INLINE lensFormat #-}
lensNickname :: forall . Lens' PasteMeta Text
lensNickname f (PasteMeta x
                          x
                          x
                          x) = fmap (\y -> PasteMeta x y x x) (f x)
{-# INLINE lensNickname #-}
lensPasted :: forall . Lens' PasteMeta UTCTime
lensPasted f (PasteMeta x
                        x
                        x
                        x) = fmap (\y -> PasteMeta x x x y) (f x)
{-# INLINE lensPasted #-}
lensTitle :: forall . Lens' PasteMeta Text
lensTitle f (PasteMeta x
                       x
                       x
                       x) = fmap (\y -> PasteMeta y x x x) (f x)
{-# INLINE lensTitle #-}
lensFormat' :: forall . Lens' PasteV TextFormat
lensFormat' f (PasteV x
                      x
                      x
                      x
                      x
                      x) = fmap (\y -> PasteV x x x y x x) (f x)
{-# INLINE lensFormat' #-}
lensNickname' :: forall . Lens' PasteV Text
lensNickname' f (PasteV x
                        x
                        x
                        x
                        x
                        x) = fmap (\y -> PasteV x x y x x x) (f x)
{-# INLINE lensNickname' #-}
lensPaste' :: forall . Lens' PasteV (TextArea Text)
lensPaste' f (PasteV x
                     x
                     x
                     x
                     x
                     x) = fmap (\y -> PasteV x x x x x y) (f x)
{-# INLINE lensPaste' #-}
lensPasteId' :: forall . Lens' PasteV PasteId
lensPasteId' f (PasteV x
                       x
                       x
                       x
                       x
                       x) = fmap (\y -> PasteV y x x x x x) (f x)
{-# INLINE lensPasteId' #-}
lensPasted' :: forall . Lens' PasteV UTCTime
lensPasted' f (PasteV x
                      x
                      x
                      x
                      x
                      x) = fmap (\y -> PasteV x x x x y x) (f x)
{-# INLINE lensPasted' #-}
lensTitle' :: forall . Lens' PasteV Text
lensTitle' f (PasteV x
                     x
                     x
                     x
                     x
                     x) = fmap (\y -> PasteV x y x x x x) (f x)
{-# INLINE lensTitle' #-}
lensUtctDay :: forall . Lens' UTCTime Day
lensUtctDay f (UTCTime x x) = fmap (\y -> UTCTime y x) (f x)
{-# INLINE lensUtctDay #-}
lensUtctDayTime :: forall . Lens' UTCTime DiffTime
lensUtctDayTime f (UTCTime x x) = fmap (\y -> UTCTime x y) (f x)
{-# INLINE lensUtctDayTime #-}
