{-# LANGUAGE TemplateHaskell #-}

module Station.Types.Version where

import           Import

import           Control.Exception  (Exception)
import qualified Data.Text          as T
import           Lens.Micro.TH

import           Station.Types.Card

newtype AuthorLink
    = AuthorLink { _unAuthorLink :: Link VersionHash }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

data Version a = Version

    -- Keys required.

    { _versionId      :: Id
    , _versionParents :: [VersionHash]
    , _versionCard    :: Maybe a

    -- Keys optional.

    , _versionAuthors :: [AuthorLink]
    -- ^ Optional so that versions can be released anonymously.
    , _versionTime    :: Maybe TAI
    } deriving (Eq, Show, Functor)

instance FromJSON (Version CardHash) where
    parseJSON = withObject "Version CardHash" $ \o -> Version
        <$> o .: "id"
        <*> o .: "parents"
        <*> o .: "card"
        <*> o .:! "authors" .!= mempty
        <*> o .:! "time"

instance ToJSON (Version CardHash) where
    toJSON a = object $
                   [ "id"      .= _versionId a
                   , "parents" .= _versionParents a
                   , "card"    .= _versionCard a
                   ]
            <> catMaybes
                   [ "authors" `optionalPair` mAuthors
                   , "time" `optionalPair` _versionTime a
                   ]
      where
        mAuthors :: Maybe [AuthorLink]
        mAuthors = case _versionAuthors a of
                       [] -> Nothing
                       _  -> Just (_versionAuthors a)

makeLenses ''Version

data ToContentsFailure
    = TCNoCard CardHash
    | TCCardMagDecode Text
    | TCCardFromMag (Card BlobHash)
    deriving (Eq, Show, Typeable)

instance Exception ToContentsFailure

versionContents
    :: (Hash -> Maybe ByteString)
    -> Version CardHash
    -> Either ToContentsFailure (Version (CardHash, CardBytes))
versionContents _ v@Version {_versionCard = Nothing} =
    Right (v & versionCard .~ Nothing)
versionContents f v@Version {_versionCard = Just cardHash } = do
    cardBts <- eitherFromMaybe (TCNoCard cardHash) (f (_unCardHash cardHash))
    cardMag <- first (TCCardMagDecode . T.pack) (eitherDecodeStrict cardBts)
    card    <- eitherFromMaybe (TCCardFromMag cardMag)
                   (cardFromMag (f . _unBlobHash) cardMag)
    Right (v & versionCard .~ Just (cardHash,card))
