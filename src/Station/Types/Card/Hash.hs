
module Station.Types.Card.Hash where

import           Import

import           Control.Exception          (Exception)
import qualified Crypto.Hash.BLAKE2.BLAKE2b as BK
import qualified Data.Hashable              as HA

import           Base64URL

--------------------------------------------------
-- * Types
--------------------------------------------------

data Hash
    = Hash { _unHash :: Base64URL }
    deriving (Eq, Ord, Show, Generic)

instance HA.Hashable Hash

unsafeHash :: Text -> Hash
unsafeHash = Hash
           . (\a -> case a of
                        Left e  -> panic ("unsafeHash failure: " <> e)
                        Right b -> b)
           . validBase64URL

-- | The @ue@ part stands for "URL encoded". I originally used @url@ for
-- this, but it made URIs confiusing to read to have @url@ show up partway
-- through.
blake2b :: Text
blake2b = "blake2b-64ue"

instance FromJSON Hash where
    parseJSON = withObject "Hash" $ \o -> Hash <$> o .: blake2b

instance ToJSON Hash where
    toJSON a = object [ blake2b .= _unHash a ]

--------------------------------------------------
-- * Functionality
--------------------------------------------------

data HashMismatch = HashMismatch
    { _mismatchBytes    :: ByteString
    , _mismatchByteHash :: Hash
    , _mismatchRecorded :: Text
    } deriving (Show, Typeable)

instance Exception HashMismatch

bytesMatchHash :: Text -> ByteString -> Either HashMismatch Hash
bytesMatchHash t bytes
    | byteHash == expected = Right expected
    | otherwise            = Left (HashMismatch bytes byteHash t)
  where
    expected :: Hash
    expected = Hash (AlreadyBase64URL t)

    byteHash :: Hash
    byteHash = hashProper bytes

-- | Blake2b hash, URL encoded using @-@ and @_@, padding stripped.
hashProper :: ByteString -> Hash
hashProper = Hash . encodeBase64URL . hashByteString

hashByteString :: ByteString -> ByteString
hashByteString =
    BK.hash
        64 -- output length
        mempty -- key

--------------------------------------------------
-- * Lenses
--------------------------------------------------

unHash :: Lens' Hash Base64URL
unHash f a = (\b -> a { _unHash = b }) <$> f (_unHash a)
