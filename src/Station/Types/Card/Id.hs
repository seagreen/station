
module Station.Types.Card.Id where

import           Import

import qualified Data.Hashable as HA
import qualified Data.UUID     as UUID
import qualified Data.UUID.V4  as UUID4

-- | Version 4 UUIDs.
--
-- <https://tools.ietf.org/html/rfc4122#page-14>
newtype Id
    = UnsafeId { _unId :: Text }
    deriving (Eq, Ord, Show, Generic, ToJSON)

instance HA.Hashable Id

instance FromJSON Id where
    parseJSON = withText "Id" $ \t ->
        case idFromText t of
            Nothing -> fail ("not a valid UUID: " <> show t)
            Just i  -> pure i

newId :: IO Id
newId = UnsafeId . UUID.toText <$> UUID4.nextRandom

-- | For hardcoding a known-correct 'Id' into source code.
unsafeId :: Text -> Id
unsafeId t = fromMaybe (panic ("unsafeId invalid: " <> t))
                       (idFromText t)

idFromText :: Text -> Maybe Id
idFromText t = const (UnsafeId t) <$> UUID.fromText t

-- * Lenses

unId :: Lens' Id Text
unId f a = (\b -> a { _unId = b }) <$> f (_unId a)
