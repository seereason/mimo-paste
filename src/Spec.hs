-- | This module is the authoritative source of all information
-- used to generate the Paste web site.
{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, RankNTypes, ScopedTypeVariables, TemplateHaskell #-}
module Spec where

import Distribution.License (License(PublicDomain))
import Happstack.Authenticate.Core (UserId(UserId))
import MIMO.Base (version)
import MIMO.Spec (Spec(..))
import qualified Ports (paste)
import Types

spec :: Spec
spec = Spec { siteName = "Paste"
            , siteVersion = version "1.2.3"
            , siteHomepage = "homepage"
            , siteAuthor = "author"
            , siteLicense = PublicDomain
            , siteSynopsis = "synopsis"
            , siteDescription = "description"
            , siteOwner = UserId 1
            , siteDomain = "algebrazam.com"
            , siteTestHost = Nothing
            , sitePorts = Ports.paste
            , siteAdmin = "logic@seereason.com"
            , siteParent = "/srv"
            , siteBackupsDir = "/srv/backups"
            , siteBackupsUser = "upload"
            , siteRowTypes = [''Paste, ''PasteMeta] }
