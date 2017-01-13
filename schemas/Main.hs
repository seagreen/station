
module Main where

import           Protolude                  hiding ((<.>))

import           Control.Monad.Trans.Reader (runReaderT)
import qualified Data.ByteString            as BS
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import qualified System.Directory           as SD
import           System.FilePath

import           Base64URL
import           Station
import           Station.JSON
import qualified Station.Original           as SO
import           Station.Types

human :: FilePath -> FilePath
human t = "schemas/human-optimized" </> t <.> "json"

canonical :: FilePath -> FilePath
canonical t = "schemas/canonical" </> t <.> "json"

versionText :: VersionContext a -> Text
versionText = _unBase64URL . _unHash . _unVersionHash . _vcHash

main :: IO ()
main = do
    dir <- SD.getCurrentDirectory
    let paths = defaultPaths (dir </> "deck")
    (impl,deck) <- loadDeckWithPaths paths
    let station = StationDetails
                      { _stationImplementation = impl
                      , _stationAuthors        = mempty
                      }
    void . flip runStateT deck . flip runReaderT station $ do

        metaVC <- fromMaybe (panic "Couldn't resolve meta schema")
              <$> resolve customSchemaId
        card <- derive "card" (T.replace "{metaVersion}" (versionText metaVC))

        authorVC <- fromMaybe (panic "Couldn't resolve author schema")
                <$> resolve authorSchemaId
        void $ derive "version"
                      ( T.replace "{cardInstance}" (_unBase64URL (_unHash card))
                      . T.replace "{authorFront}" (versionText authorVC)
                      )

-- | This returns the hash of the derived canoncial schema.
--
-- We can't use the ones in the station library
-- (eg 'Station.Schema.cardSchemaHash') because they're built at
-- compile time using template haskell, so if a schema is changed
-- during the run of this script they will become out of date.
derive :: MonadIO m => FilePath -> (Text -> Text) -> m Hash
derive  t fillTemplate = liftIO $ do
    bts <- fmap encodeUtf8
         . convertJQ
         . fillTemplate
         . decodeUtf8
         =<< BS.readFile (human t)
    BS.writeFile (canonical t) bts
    pure $ hashProper (SO.unsafeStripTrailingEOL bts)
