{-# LANGUAGE TemplateHaskell #-}

module Station.Types.Card.Link where

import           Import

import           Data.Aeson.Types        (Parser)
import qualified Data.Hashable           as HA
import qualified Data.HashMap.Strict     as HM
import           Lens.Micro.TH

import           Station.Types.Card.Hash
import           Station.Types.Card.Id

--------------------------------------------------
-- * Simple Hash Wrappers
--------------------------------------------------

newtype VersionHash
    = VersionHash { _unVersionHash :: Hash }
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

newtype CardHash
    = CardHash { _unCardHash :: Hash }
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

newtype BlobHash
    = BlobHash { _unBlobHash :: Hash }
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

instance HA.Hashable VersionHash
instance HA.Hashable CardHash
instance HA.Hashable BlobHash

--------------------------------------------------
-- * Sum Hashes
--------------------------------------------------

data BlobOrVersionHash
    = BVVersion VersionHash
    | BVBlob    BlobHash
    deriving (Eq, Ord, Show, Generic)

instance HA.Hashable BlobOrVersionHash

--------------------------------------------------
-- * Link
--
-- TODO: Should all links fail if there's more than one of
-- @"blob"@/@"version"@/@"card"@ in them, or just the ones
-- link @Link BlobOrVersionHash@ that would have trouble
-- picking which one to use?
--------------------------------------------------

data Link a = Link
    { _linkId   :: Id
    , _linkHash :: a
    } deriving (Eq, Ord, Show, Generic, Functor)

instance HA.Hashable a => HA.Hashable (Link a)

instance FromJSON (Link VersionHash) where
    parseJSON = withObject "Link VersionHash" $ \o -> Link
        <$> o .: "id"
        <*> o .: "version"

instance ToJSON (Link VersionHash) where
    toJSON a = object [ "id"      .= _linkId a
                      , "version" .= _linkHash a
                      ]

instance FromJSON (Link BlobOrVersionHash) where
    parseJSON = withObject "Link BlobOrVersionHash" $ \o -> do
        forbidAnd o
        Link <$> o .: "id" <*> (f o <|> g o)
      where
        f :: HashMap Text Value -> Parser BlobOrVersionHash
        f o = BVVersion <$> o .: versionKey

        g :: HashMap Text Value -> Parser BlobOrVersionHash
        g o = BVBlob <$> o .: blobKey

        versionKey :: Text
        versionKey = "version"

        blobKey :: Text
        blobKey = "blob"

        forbidAnd :: HashMap Text Value -> Parser ()
        forbidAnd o =
            case (HM.lookup versionKey o, HM.lookup blobKey o) of
                (Just _, Just _) -> fail "can't have both version and blob"
                _                -> pure ()

instance ToJSON (Link BlobOrVersionHash) where
    toJSON a = object [ "id" .= _linkId a
                      , case _linkHash a of
                            BVVersion b -> "version" .= b
                            BVBlob    b -> "blob"    .= b
                      ]

--------------------------------------------------
-- * Lenses
--------------------------------------------------

unVersionHash :: Lens' VersionHash Hash
unVersionHash f a = (\b -> a { _unVersionHash = b }) <$> f (_unVersionHash a)

unCardHash :: Lens' CardHash Hash
unCardHash f a = (\b -> a { _unCardHash = b }) <$> f (_unCardHash a)

unBlobHash :: Lens' BlobHash Hash
unBlobHash f a = (\b -> a { _unBlobHash = b }) <$> f (_unBlobHash a)

makeLenses ''BlobOrVersionHash

linkId :: Lens' (Link a) Id
linkId f a = (\b -> a { _linkId = b }) <$> f (_linkId a)

linkHash :: Lens' (Link a) a
linkHash f a = (\b -> a { _linkHash = b }) <$> f (_linkHash a)
