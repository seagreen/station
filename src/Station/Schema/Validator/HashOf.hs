-- | NOTE: The code in this module should be kept similar to
-- 'Station.Schema.Validator.LinkTo', as they're almost the same.
module Station.Schema.Validator.HashOf where

import           Import

import qualified Data.List.NonEmpty          as NE
import qualified Data.Text                   as T
import qualified JSONSchema.Validator.Draft4 as VAL
import           JSONSchema.Validator.Types  (Validator(..))

import qualified Station.Types               as ST

hashOfValidator
    :: forall err schema.
       (ST.Hash -> Maybe ByteString)
    -> (schema -> Value -> [err])
    -> Validator schema (Maybe (HashOf schema)) (HashOfInvalid err)
hashOfValidator getContent validate =
    Validator
        (\a -> case a of
                 Just (HashOf b) -> (pure b, mempty)
                 Nothing         -> (mempty, mempty))
        (VAL.run (hashOfVal getContent validate))

newtype HashOf schema
    = HashOf { _unHashOf :: schema }
    deriving (Eq, Show)

instance FromJSON schema => FromJSON (HashOf schema) where
    parseJSON = withObject "HashOf" $ \o ->
        HashOf <$> o .: "hashOf"

data HashOfInvalid err
    = HashOfNotUsable        (HashMap Text Value) Text
      -- ^ First argument is the hash JSON object, second is the error message.
    | HashOfNotFound         ST.Hash
    | HashOfContentParseFail ST.Hash ByteString Text
      -- ^ Last argument is the error message.
    | HashOfInvalid          ST.Hash (NonEmpty err)
    deriving (Eq, Show)

hashOfVal
    :: forall err schema.
       (ST.Hash -> Maybe ByteString)
    -> (schema -> Value -> [err])
    -> HashOf schema
    -> HashMap Text Value
    -> Maybe (HashOfInvalid err)
hashOfVal getContent f (HashOf schema) x =
    case res of
        Left e   -> Just e
        Right () -> Nothing
  where
    res :: Either (HashOfInvalid err) ()
    res = do
        hash <- first (HashOfNotUsable x) (fromJSONEither (Object x))
        bts <- case getContent hash of
                   Nothing  -> Left (HashOfNotFound hash)
                   Just bts -> Right bts
        v <- first (HashOfContentParseFail hash bts . T.pack)
                   (eitherDecodeStrict bts)
        case NE.nonEmpty (f schema v) of
            Just failures -> Left (HashOfInvalid hash failures)
            Nothing       -> Right ()
