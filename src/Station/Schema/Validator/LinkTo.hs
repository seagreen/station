-- | NOTE: The code in this module should be kept similar to
-- 'Station.Schema.Validator.HashOf', as they're almost the same.
--
-- (We're violating DRY on purpose here for straightforwardness).
module Station.Schema.Validator.LinkTo where

import           Import

import qualified Data.List.NonEmpty          as NE
import qualified Data.Text                   as T
import qualified JSONSchema.Validator.Draft4 as VAL
import           JSONSchema.Validator.Types  (Validator(..))

import           Station.Lookup              (bytesFromBlobOrVersionHash)
import qualified Station.Types               as ST

linkToValidator
    :: forall err schema.
       (ST.Hash -> Maybe ByteString)
    -> (schema -> Value -> [err])
    -> Validator schema (Maybe (LinkTo schema)) (LinkToInvalid err)
linkToValidator getContent validate =
    Validator
        (\a -> case a of
                 Just (LinkTo b) -> (pure b, mempty)
                 Nothing         -> (mempty, mempty))
        (VAL.run (linkToVal getContent validate))

newtype LinkTo schema
    = LinkTo { _unLinkTo :: schema }
    deriving (Eq, Show)

instance FromJSON schema => FromJSON (LinkTo schema) where
    parseJSON = withObject "LinkTo" $ \o ->
        LinkTo <$> o .: "linkTo"

data LinkToInvalid err
    = LinkToNotUsable        (HashMap Text Value) Text
      -- ^ First argument is the link, second is the error message.
    | LinkToNotFound         (ST.Link ST.BlobOrVersionHash)
    | LinkToContentParseFail (ST.Link ST.BlobOrVersionHash) ByteString Text
      -- ^ Last argument is the error message.
    | LinkToInvalid          (ST.Link ST.BlobOrVersionHash) (NonEmpty err)
    deriving (Eq, Show)

linkToVal
    :: forall err schema.
       (ST.Hash -> Maybe ByteString)
    -> (schema -> Value -> [err])
    -> LinkTo schema
    -> HashMap Text Value
    -> Maybe (LinkToInvalid err)
linkToVal getContent f (LinkTo schema) x =
    case res of
        Left e   -> Just e
        Right () -> Nothing
  where
    res :: Either (LinkToInvalid err) ()
    res = do
        link <- first (LinkToNotUsable x) (fromJSONEither (Object x))
        bts <- case bytesFromBlobOrVersionHash getContent (ST._linkHash link) of
                   Nothing  -> Left (LinkToNotFound link)
                   Just bts -> Right bts
        v <- first (LinkToContentParseFail link bts . T.pack)
                   (eitherDecodeStrict bts)
        case NE.nonEmpty (f schema v) of
            Just failures -> Left (LinkToInvalid link failures)
            Nothing       -> Right ()
