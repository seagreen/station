
module Base64URL where

import           Protolude

import           Data.Aeson
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.Hashable              as HA
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           GHC.Generics               (Generic)

newtype Base64URL
    = AlreadyBase64URL { _unBase64URL :: Text }
    deriving (Eq, Ord, Show, FromJSON, ToJSON, Generic)

instance HA.Hashable Base64URL

encodeBase64URL :: ByteString -> Base64URL
encodeBase64URL =
      AlreadyBase64URL
    . TE.decodeUtf8
    . stripPadding
    . B64URL.encode

decodeBase64URL :: Base64URL -> Either Text ByteString
decodeBase64URL (AlreadyBase64URL t) =
    case validBase64URL t of
        Left e  -> Left e
        Right _ -> Right . B64URL.decodeLenient . TE.encodeUtf8 $ t

validBase64URL :: Text -> Either Text Base64URL
validBase64URL t
    | T.all valid t = Right (AlreadyBase64URL t)
    | otherwise = Left ("Invalid Base64URL due to forbidden character: " <> t)
  where
    -- I had hoped to use B64URL.decode instead of this custom check,
    -- but the former also enforces that padding with '=' is present.
    valid :: Char -> Bool
    valid c = elem c ('-' : '_' : ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'])

stripPadding :: ByteString -> ByteString
stripPadding bts =
    case BS.unsnoc bts of
        Nothing            -> mempty
        Just (start,final) ->
            if BS.singleton final == "="
                then stripPadding start
                else bts
