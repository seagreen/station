
module Station.JSON where

import           Import

import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Builder   as LB
import qualified Turtle                   as TU

encodeProper :: ToJSON a => a -> ByteString
encodeProper = LBS.toStrict . AP.encodePretty' config

encodeProperWithEOL :: ToJSON a => a -> ByteString
encodeProperWithEOL a =
      encodeUtf8
    . TL.toStrict
    . LB.toLazyText
    $ AP.encodePrettyToTextBuilder' config a <> LB.fromText "\n"

-- | Take a JSON value encoded as bytes. Reserialize it standardized
-- (no indentation, sorted keys, etc).
convertProper :: ByteString -> Either Text ByteString
convertProper a =
    case eitherDecodeStrict a of
        Left e             -> Left (T.pack e)
        Right (b :: Value) -> Right (encodeProper b)

config :: AP.Config
config = AP.Config
    { AP.confIndent    = AP.Spaces 0
    , AP.confCompare   = AP.compare
    , AP.confNumFormat = AP.Decimal
    }

-- * Using JQ

-- | This will throw an error if the argument isn't valid JSON.
convertJQ :: Text -> IO Text
convertJQ jsonText = TU.strict (TU.inshell jqCommand inpt)
  where
    inpt :: TU.Shell TU.Line
    inpt = TU.select (TU.textToLines jsonText)

    jqCommand :: Text
    jqCommand = "jq --compact-output --sort-keys ."
