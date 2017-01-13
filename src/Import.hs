
module Import
    ( module Export
    , eitherFromMaybe
    , fromJSONMaybe
    , fromJSONEither
    , optionalPair
    , whenLeft
    , unsafeDecodeUtf8
    ) where

import           Protolude           as Export hiding ((&), (<$!>), link, to)

import           Control.Monad       as Export (fail)
import           Control.Monad.Base  as Export
import           Control.Monad.Catch as Export (MonadThrow(..))
import           Data.Aeson          as Export hiding (Result(..))
import           Data.List.NonEmpty  as Export (NonEmpty(..))
import           Data.Foldable       as Export (foldrM, traverse_)
import           Data.HashMap.Strict as Export (HashMap)
import           Data.List           as Export (unlines)
import           Data.Set            as Export (Set)
import           Data.String         as Export (String)
import           Data.Text.Encoding  as Export (decodeUtf8', encodeUtf8)
import           Data.Typeable       as Export (Typeable)
import           Lens.Micro          as Export

import qualified Data.Aeson          as AE
import qualified Data.Text           as T
import           Data.Text.Encoding  (decodeUtf8)

-- | JSON key/value pair that should only be serialized
-- if the value exists.
--
-- For use with 'object' and 'catMaybes'.
optionalPair :: ToJSON a => Text -> Maybe a -> Maybe (Text, Value)
optionalPair k = fmap (k .=)

eitherFromMaybe :: a -> Maybe b -> Either a b
eitherFromMaybe a = maybe (Left a) Right

fromJSONMaybe :: FromJSON a => Value -> Maybe a
fromJSONMaybe a =
    case fromJSON a of
        AE.Error _   -> Nothing
        AE.Success b -> Just b

fromJSONEither :: FromJSON a => Value -> Either Text a
fromJSONEither a =
    case fromJSON a of
        AE.Error e   -> Left (T.pack e)
        AE.Success b -> Right b

-- | From the `either` package.
whenLeft :: Applicative m => Either a b -> (a -> m ()) -> m ()
whenLeft (Left x) f = f x
whenLeft _ _        = pure ()

unsafeDecodeUtf8 :: ByteString -> Text
unsafeDecodeUtf8 = decodeUtf8
