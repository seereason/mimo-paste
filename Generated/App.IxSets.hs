instance Indexable Paste
    where empty = ixSet [Ix (empty :: forall . Map PasteId
                                                   (Set Paste)) (flattenWithCalcs noCalcs),
                         Ix (empty :: forall . Map UTCTime
                                                   (Set Paste)) (flattenWithCalcs noCalcs)]
type Pastes = IxSet Paste
