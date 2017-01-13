
module Station.Types.Card.URI where

import           Import

import qualified Data.Hashable            as HA
import qualified Data.HashMap.Strict      as HM
import qualified Data.Text                as T
import qualified URI.ByteString           as URI

import           Station.Types.Card.Link

-- Unparsed, no guarantee this is actually a valid URI.
newtype CardURI a
    = CardURI { _unCardURI :: Text }
    deriving (Eq, Ord, Show, Generic)

instance HA.Hashable a => HA.Hashable (CardURI a)

data CardURIError
    = URIParseError         URI.URIParseError
    | RelativeRefNoFragment (URI.URIRef URI.Relative)
    | LinkError             LinkConversionError
    deriving (Eq, Show)

-- | Within the "$ref" JSON schema validator we allow content-addressable
-- links to other cards or JSON Pointer links within the document, but
-- not both.
data LinkOrFragment
    = LFLink     (Link VersionHash)
    | LFFragment Text
    deriving (Eq, Show)

linkFromURI :: CardURI a -> Either CardURIError LinkOrFragment
linkFromURI cardURI =
    case parseURI cardURI of
        Left e                  -> Left (URIParseError e)
        Right (URIRelative uri) ->
            case URI.rrFragment uri of
                Nothing -> Left (RelativeRefNoFragment uri)
                Just a  -> Right $ LFFragment (unsafeDecodeUtf8 a)
        Right (URIAbsolute uri) ->
            bimap LinkError LFLink $ linkFromParsedURI uri

-- * URI to Link

data LinkConversionError
    = LCENotCard              URI.Scheme
    | LCEFromIntermediateJSON (HashMap Text Value) Text
      -- ^ The first argument is the JSON Object we turned the card URI into.
      --
      -- The second argument is the error.
    deriving (Eq, Show)

linkFromParsedURI
    :: URI.URIRef URI.Absolute
    -> Either LinkConversionError (Link VersionHash)
linkFromParsedURI uri
    | URI.uriScheme uri /= URI.Scheme "card"
        = Left (LCENotCard (URI.uriScheme uri))
    | otherwise
        = first (LCEFromIntermediateJSON hm) (fromJSONEither (Object hm))
  where
    hm :: HashMap Text Value
    hm = fmap f
       . HM.fromList
       . fmap (bimap unsafeDecodeUtf8 unsafeDecodeUtf8)
       . URI.queryPairs . URI.uriQuery
       $ uri

    -- Text that has a @:@ becomes a JSON Object.
    --
    -- Otherwise it becomes a JSON String.
    f :: Text -> Value
    f t =
        let (a,b) = T.breakOn ":" t
        in case T.stripPrefix ":" b of
            Nothing -> String a
            Just b' -> Object (HM.singleton a (String b'))

-- * URIs and Parsing

-- | @uri-bytestring@ provides URI types that both have and do not have
-- a @scheme@ field. Make a type that covers both cases.
data URI
    = URIAbsolute (URI.URIRef URI.Absolute)
    | URIRelative (URI.URIRef URI.Relative)
    deriving (Eq, Show)

parseURI :: CardURI a -> Either URI.URIParseError URI
parseURI (CardURI t) =
    let uriBytes = encodeUtf8 t
    in case URI.parseURI URI.strictURIParserOptions uriBytes of
        Right a -> Right (URIAbsolute a)
        Left  _ -> URIRelative
               <$> URI.parseRelativeRef URI.strictURIParserOptions uriBytes

-- * Lenses

unCardURI :: Lens' (CardURI a) Text
unCardURI f a = (\b -> a { _unCardURI = b }) <$> f (_unCardURI a)
