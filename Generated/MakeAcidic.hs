instance IsAcidic AppState
    where acidEvents = [QueryEvent (\(GetPasteByIdEvent arg) -> getPasteByIdEvent arg),
                        QueryEvent (\(SomePastesEvent arg arg) -> somePastesEvent arg arg),
                        UpdateEvent (\(CreatePasteEvent arg) -> createPasteEvent arg),
                        UpdateEvent (\(UpdatePasteEvent arg) -> updatePasteEvent arg),
                        UpdateEvent (\(DeletePasteEvent arg) -> deletePasteEvent arg)]
newtype GetPasteByIdEvent
  = GetPasteByIdEvent PasteId
    deriving (Typeable)
instance SafeCopy GetPasteByIdEvent
    where putCopy (GetPasteByIdEvent arg) = contain (do {safePut arg;
                                                         return ()})
          getCopy = contain (return GetPasteByIdEvent <*> safeGet)
instance Method GetPasteByIdEvent
    where type MethodResult GetPasteByIdEvent = Maybe Paste
          type MethodState GetPasteByIdEvent = AppState
instance QueryEvent GetPasteByIdEvent
data SomePastesEvent = SomePastesEvent Int Int deriving (Typeable)
instance SafeCopy SomePastesEvent
    where putCopy (SomePastesEvent arg arg) = contain (do {safePut arg;
                                                           safePut arg;
                                                           return ()})
          getCopy = contain ((return SomePastesEvent <*> safeGet) <*> safeGet)
instance Method SomePastesEvent
    where type MethodResult SomePastesEvent = [Paste]
          type MethodState SomePastesEvent = AppState
instance QueryEvent SomePastesEvent
newtype CreatePasteEvent
  = CreatePasteEvent Paste
    deriving (Typeable)
instance SafeCopy CreatePasteEvent
    where putCopy (CreatePasteEvent arg) = contain (do {safePut arg;
                                                        return ()})
          getCopy = contain (return CreatePasteEvent <*> safeGet)
instance Method CreatePasteEvent
    where type MethodResult CreatePasteEvent = PasteId
          type MethodState CreatePasteEvent = AppState
instance UpdateEvent CreatePasteEvent
newtype UpdatePasteEvent
  = UpdatePasteEvent Paste
    deriving (Typeable)
instance SafeCopy UpdatePasteEvent
    where putCopy (UpdatePasteEvent arg) = contain (do {safePut arg;
                                                        return ()})
          getCopy = contain (return UpdatePasteEvent <*> safeGet)
instance Method UpdatePasteEvent
    where type MethodResult UpdatePasteEvent = PasteId
          type MethodState UpdatePasteEvent = AppState
instance UpdateEvent UpdatePasteEvent
newtype DeletePasteEvent
  = DeletePasteEvent Paste
    deriving (Typeable)
instance SafeCopy DeletePasteEvent
    where putCopy (DeletePasteEvent arg) = contain (do {safePut arg;
                                                        return ()})
          getCopy = contain (return DeletePasteEvent <*> safeGet)
instance Method DeletePasteEvent
    where type MethodResult DeletePasteEvent = ()
          type MethodState DeletePasteEvent = AppState
instance UpdateEvent DeletePasteEvent
data Acid
    = Acid {acidAuthenticate :: (AcidState AuthenticateState),
            acidApp :: (AcidState AppState)}
withAcid :: forall a . AcidState AuthenticateState ->
                       (Acid -> IO a) -> IO a
withAcid authenticateState f = bracket (openLocalStateFrom "state/AppState" initialAppState) createCheckpointAndClose $ (\app -> f (Acid authenticateState app))
instance (Functor m, Monad m) => HasAcidState (FoundationT' url
                                                            Acid
                                                            reqSt
                                                            m)
                                              AuthenticateState
    where getAcidState = acidAuthenticate <$> getAcidSt
instance (Functor m, Monad m) => HasAcidState (FoundationT' url
                                                            Acid
                                                            requestState
                                                            m)
                                              AppState
    where getAcidState = acidApp <$> getAcidSt
