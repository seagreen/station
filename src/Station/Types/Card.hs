{-# LANGUAGE TemplateHaskell #-}

module Station.Types.Card
    ( module Station.Types.Card
    , module STC
    ) where

import           Import

import qualified Data.Hashable                as HA
import           Lens.Micro.TH

import           Station.Types.Card.Hardcoded as STC
import           Station.Types.Card.Hash      as STC
import           Station.Types.Card.Id        as STC
import           Station.Types.Card.Link      as STC
import           Station.Types.Card.Time      as STC
import           Station.Types.Card.URI       as STC

newtype SchemaLink
    = SchemaLink { _unSchemaLink :: Link BlobOrVersionHash }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance HA.Hashable SchemaLink

type CardBytes = Card ByteString

data Card a = Card

    -- Keys required.

    { _cardSchema   :: SchemaLink
    , _cardInstance :: a

    -- Keys optional.

    , _cardName :: Maybe Text
    } deriving (Eq, Show, Functor)

instance FromJSON a => FromJSON (Card a) where
    parseJSON = withObject "Card" $ \o -> Card
        <$> o .: "schema"
        <*> o .: "instance"
        <*> o .:! "name"

instance ToJSON a => ToJSON (Card a) where
    toJSON a = object $
                   [ "schema"   .= _cardSchema a
                   , "instance" .= _cardInstance a
                   ]
            <> catMaybes
                   [ "name" `optionalPair` _cardName a
                   ]

magFromCard :: CardBytes -> Card BlobHash
magFromCard a = a { _cardInstance = hashFront a }

cardFromMag
    :: (BlobHash -> Maybe ByteString)
    -> Card BlobHash
    -> Maybe CardBytes
cardFromMag f card = do
    front <- f (_cardInstance card)
    Just (card { _cardInstance = front })

hashFront :: CardBytes -> BlobHash
hashFront = BlobHash . STC.hashProper . _cardInstance

-- * Lenses

unSchemaLink :: Lens' SchemaLink (Link BlobOrVersionHash)
unSchemaLink f a = (\b -> a { _unSchemaLink = b }) <$> f (_unSchemaLink a)

makeLenses ''Card
