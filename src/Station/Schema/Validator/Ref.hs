
module Station.Schema.Validator.Ref where

import           Import

import qualified Data.List.NonEmpty             as NE
import qualified Data.Text                      as T
import qualified JSONPointer                    as JP
import qualified JSONSchema.Validator.Draft4    as VAL
import           JSONSchema.Validator.Types     (Validator(..))
import           Network.HTTP.Types.URI         (urlDecode)

import           Station.Lookup                 (bytesFromVersionHash)
import qualified Station.Types                  as ST

newtype Scope schema
    = Scope { _unScope :: schema }
    deriving (Eq, Show)

refValidator
    :: forall err schema. (FromJSON schema, ToJSON schema)
    => (ST.Hash -> Maybe ByteString)
    -> Scope schema
    -> (Scope schema -> schema -> Value -> [err])
    -> Validator schema (Maybe Ref) (RefInvalid err)
refValidator getContent scope validate =
    Validator
        VAL.noEmbedded
        (VAL.run (refVal getContent scope validate))

newtype Ref
    = Ref { _unRef :: ST.CardURI (ST.Link ST.VersionHash) }
    deriving (Eq, Show)

instance FromJSON Ref where
    parseJSON = withObject "Ref" $ \o ->
        Ref . ST.CardURI <$> o .: "$ref"

data RefInvalid err
    = RefSyntax       Ref ST.CardURIError
    | RefResolution   Ref
    | RefParseFailure Ref ByteString Text
      -- ^ Last argument is the error message.
    | RefInvalid      Ref (NonEmpty err)
    deriving (Eq, Show)

refVal
    :: forall err schema. (FromJSON schema, ToJSON schema)
    => (ST.Hash -> Maybe ByteString)
    -> Scope schema
    -> (Scope schema -> schema -> Value -> [err])
    -> Ref
    -> Value
    -> Maybe (RefInvalid err)
refVal getContent scope f ref x =
    case ST.linkFromURI (_unRef ref) of
        Left e               -> Just (RefSyntax ref e)
        Right linkOrFragment ->
            case linkOrFragment of
                ST.LFLink a ->
                    case getSchema (ST._linkHash a) of
                        Left e  -> Just e
                        Right b -> RefInvalid ref
                               <$> NE.nonEmpty (f (Scope b) b x)
                ST.LFFragment a ->
                    -- NOTE: hjsonschema's @resolveFragment@ isn't perfect
                    -- here since it takes a @Maybe@ as its first argument.
                    case resolveFragment (Just a) (_unScope scope) of
                        Nothing -> Just (RefResolution ref)
                        Just b  -> RefInvalid ref
                               <$> NE.nonEmpty (f scope b x)
  where
    getSchema :: ST.VersionHash -> Either (RefInvalid err) schema
    getSchema hash = do
        bts <- case bytesFromVersionHash getContent hash of
                   Nothing  -> Left (RefResolution ref)
                   Just bts -> Right bts
        case eitherDecodeStrict bts of
            Left e  -> Left (RefParseFailure ref bts (T.pack e))
            Right a -> Right a

resolveFragment
    :: (FromJSON schema, ToJSON schema)
    => Maybe Text
    -> schema
    -> Maybe schema
resolveFragment Nothing schema        = Just schema
resolveFragment (Just pointer) schema = do
    let urlDecoded = decodeUtf8 . urlDecode True . encodeUtf8 $ pointer
    p <- either (const Nothing) Just (JP.unescape urlDecoded)
    x <- either (const Nothing) Just (JP.resolve p (toJSON schema))
    fromJSONMaybe x
