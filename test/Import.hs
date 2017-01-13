
module Import
    ( module Export
    , fromJSONEither
    , shouldBeNothing
    , shouldBeJust
    , shouldBeLeft
    , shouldBeRight
    ) where

import           Protolude           as Export hiding (to)

import           Data.Aeson          as Export hiding (Result(..))
import           Data.HashMap.Strict as Export (HashMap)
import           Data.List.NonEmpty  as Export (NonEmpty(..))
import           Data.Text.Encoding  as Export (decodeUtf8', encodeUtf8)
import           Lens.Micro          as Export hiding ((&))

import qualified Data.Aeson          as AE
import qualified Data.Text           as T
import           Test.Hspec

fromJSONEither :: FromJSON a => Value -> Either Text a
fromJSONEither a =
    case fromJSON a of
        AE.Error e   -> Left (T.pack e)
        AE.Success b -> Right b

-- | A shortcut for '`shouldNotBe` Nothing'.
--
-- Also @a@ doesn't have to be an instance of 'Eq'.
shouldBeNothing :: Show a => Maybe a -> Expectation
shouldBeNothing Nothing  = pure ()
shouldBeNothing (Just a) =
    expectationFailure ("shouldBeNothing: instead of Nothing got Just "
                        <> show a)

shouldBeJust :: MonadIO m => Maybe a -> m a
shouldBeJust Nothing = liftIO $ do
    expectationFailure "shouldBeJust failed"
    panic "shouldBeJust should not reach this point"
shouldBeJust (Just a) = pure a

shouldBeLeft :: (MonadIO m, Show b) => Either a b -> m a
shouldBeLeft (Left a)  = pure a
shouldBeLeft (Right b) = liftIO $ do
    expectationFailure ("shouldBeLeft: instead of Left got Right " <> show b)
    panic "shouldBeLeft should not reach this point"

shouldBeRight :: (MonadIO m, Show a) => Either a b -> m b
shouldBeRight (Left a) = liftIO $ do
    expectationFailure ("shouldBeRight: instead of Right got Left " <> show a)
    panic "shouldBeRight should not reach this point"
shouldBeRight (Right b) = pure b
