{-# LANGUAGE TemplateHaskell #-}

module Station.Types.VersionContext where

import           Import

import qualified Data.Hashable         as HA
import           Lens.Micro.TH

import           Station.Types.Card
import           Station.Types.Version

-- | @Nothing@ means a local version. @Just Text@ means a version found at
-- that URL.
newtype VersionLocation = VersionLocation
    { _unVersionLocation :: Maybe Text }
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

instance HA.Hashable VersionLocation

type VersionInfo = VersionContext (CardHash, CardBytes)

-- | Things we want to know about each version while the app is running.
data VersionContext a = VersionContext
    { _vcHash     :: VersionHash
    , _vcVersion  :: Version a
    , _vcLocation :: NonEmpty VersionLocation
    } deriving (Eq, Show, Functor)

instance FromJSON a => FromJSON (VersionContext a) where
    parseJSON = withObject "VersionContext a" $ \o -> VersionContext
        <$> o .: "hash"
        <*> o .: "version"
        <*> o .: "locations"

instance ToJSON a => ToJSON (VersionContext a) where
    toJSON a = object
        [ "hash"      .= _vcHash a
        , "version"   .= _vcVersion a
        , "locations" .= _vcLocation a
        ]

linkFromVersionContext :: VersionContext a -> Link VersionHash
linkFromVersionContext vc = Link
    { _linkId   = _versionId (_vcVersion vc)
    , _linkHash = _vcHash vc
    }

-- * Lenses

makeLenses ''VersionLocation
makeLenses ''VersionContext
