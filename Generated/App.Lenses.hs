lensPaste :: forall . Lens' Paste Text
lensPaste f (Paste x x x) = fmap (\y -> Paste x x y) (f x)
{-# INLINE lensPaste #-}
lensPasteId :: forall . Lens' Paste PasteId
lensPasteId f (Paste x x x) = fmap (\y -> Paste y x x) (f x)
{-# INLINE lensPasteId #-}
lensPasteMeta :: forall . Lens' Paste PasteMeta
lensPasteMeta f (Paste x x x) = fmap (\y -> Paste x y x) (f x)
{-# INLINE lensPasteMeta #-}
lensFormat' :: forall . Lens' Paste' TextFormat
lensFormat' f (Paste' x
                      x
                      x
                      x
                      x
                      x) = fmap (\y -> Paste' x x x y x x) (f x)
{-# INLINE lensFormat' #-}
lensNickname' :: forall . Lens' Paste' Text
lensNickname' f (Paste' x
                        x
                        x
                        x
                        x
                        x) = fmap (\y -> Paste' x x y x x x) (f x)
{-# INLINE lensNickname' #-}
lensPaste' :: forall . Lens' Paste' (TextArea Text)
lensPaste' f (Paste' x
                     x
                     x
                     x
                     x
                     x) = fmap (\y -> Paste' x x x x x y) (f x)
{-# INLINE lensPaste' #-}
lensPasteId' :: forall . Lens' Paste' PasteId
lensPasteId' f (Paste' x
                       x
                       x
                       x
                       x
                       x) = fmap (\y -> Paste' y x x x x x) (f x)
{-# INLINE lensPasteId' #-}
lensPasted' :: forall . Lens' Paste' UTCTime
lensPasted' f (Paste' x
                      x
                      x
                      x
                      x
                      x) = fmap (\y -> Paste' x x x x y x) (f x)
{-# INLINE lensPasted' #-}
lensTitle' :: forall . Lens' Paste' Text
lensTitle' f (Paste' x
                     x
                     x
                     x
                     x
                     x) = fmap (\y -> Paste' x y x x x x) (f x)
{-# INLINE lensTitle' #-}
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
lensUtctDay :: forall . Lens' UTCTime Day
lensUtctDay f (UTCTime x x) = fmap (\y -> UTCTime y x) (f x)
{-# INLINE lensUtctDay #-}
lensUtctDayTime :: forall . Lens' UTCTime DiffTime
lensUtctDayTime f (UTCTime x x) = fmap (\y -> UTCTime x y) (f x)
{-# INLINE lensUtctDayTime #-}
