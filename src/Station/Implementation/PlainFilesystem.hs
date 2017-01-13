{-# LANGUAGE TemplateHaskell #-}

module Station.Implementation.PlainFilesystem where

import           Import

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict   as HM
import           Data.List             (dropWhileEnd)
import qualified Data.Set              as SET
import qualified Data.Text             as T
import           Lens.Micro.TH
import qualified Pipes                 as P
import qualified Pipes.Prelude         as PP
import qualified System.Directory      as SD
import           System.FilePath       ((</>))
import qualified System.FilePath       as FP
import           System.IO             (IOMode(..), withBinaryFile)

import           Base64URL
import qualified Station.Original      as SO
import qualified Station.Types         as ST

data Paths = Paths
    { _pathHashes   :: FilePath
      -- ^ For example: /home/foo/deck/hashes (a directory).
    , _pathLocal    :: FilePath
      -- ^ For example: /home/foo/deck/local_versions (a file).
    , _pathVersions :: FilePath
      -- ^ For example: /home/foo/deck/versions (a directory).
    } deriving (Eq, Show)

insert :: (MonadIO m, MonadState ST.Deck m) => Paths -> ByteString -> m ()
insert paths bts = do
    deck <- get
    let hash = ST.hashProper bts
    liftIO $ writeBytes paths hash bts
    put $ deck & ST.deckBytes %~ HM.insert hash bts

data IncorrectEOL
    = IncorrectEOL FilePath (Maybe Char)
    deriving (Show, Typeable)

instance Exception IncorrectEOL

buildBytes :: FilePath -> IO (HashMap ST.Hash ByteString)
buildBytes dir = PP.fold (\acc (x,y) -> HM.insert x y acc) mempty identity
               $ f P.<-< forDirectoryContents dir
  where
    f :: P.Pipe FilePath (ST.Hash, ByteString) IO ()
    f = PP.mapM $ \path -> do
            bts <- stripTrailingEOL' path =<< BS.readFile path
            case ST.bytesMatchHash (T.pack (FP.takeFileName path)) bts of
                Left hashMismatch -> throwIO hashMismatch
                Right hash        -> pure (hash, bts)

    stripTrailingEOL' :: FilePath -> ByteString -> IO ByteString
    stripTrailingEOL' path oldBts =
        case SO.stripTrailingEOL oldBts of
            Left err  -> throwIO $ IncorrectEOL path err
            Right bts -> pure bts

-- | Modified from here:
-- https://gist.github.com/FranklinChen/133cb61af931a08bbe20
forDirectoryContents :: FilePath -> P.Producer FilePath IO ()
forDirectoryContents dir = do
    -- 'listDirectory' is like 'getDirectoryContents' except it doesn't
    -- return the @.@ and @..@ special files.
    names <- P.lift (SD.listDirectory dir)
    forM_ names $ \name -> do
        let path = dir </> name
        isDirectory <- P.lift (SD.doesDirectoryExist path)
        unless isDirectory (P.yield path)

buildVersions :: Paths -> IO (HashMap ST.VersionLocation (Set ST.VersionHash))
buildVersions paths = do
    localVersionBytes <- unsafeDecodeUtf8 <$> BS.readFile (_pathLocal paths)
    let hm = HM.singleton (ST.VersionLocation Nothing)
                          (buildFromLocation localVersionBytes)
    PP.fold (\acc (x,y) -> HM.insert (ST.VersionLocation (Just x)) y acc) hm identity
        $ loadLocation P.<-< forDirectoryContents (_pathVersions paths)
  where
    loadLocation :: P.Pipe FilePath (Text, Set ST.VersionHash) IO ()
    loadLocation =
        PP.mapM $ \path -> do
           t <- unsafeDecodeUtf8 <$> BS.readFile path
           pure (T.pack path, buildFromLocation t)

    buildFromLocation :: Text -> Set ST.VersionHash
    buildFromLocation = SET.fromList
                      . fmap f
                      . dropWhileEnd T.null
                      . T.splitOn "\n"

    -- TODO: Check that it's valid.
    f :: Text -> ST.VersionHash
    f = ST.VersionHash . ST.Hash . AlreadyBase64URL

writeVersion :: Paths -> ST.VersionHash -> ByteString -> IO ()
writeVersion paths hash bts = do
    writeBytes paths (ST._unVersionHash hash) bts
    BS.appendFile
        (_pathLocal paths)
        (encodeUtf8
            (T.snoc (_unBase64URL (ST._unHash (ST._unVersionHash hash)))
                    '\n'))

writeBytes :: Paths -> ST.Hash -> ByteString -> IO ()
writeBytes path hash bts = do
    let p = _pathHashes path </> T.unpack (_unBase64URL (ST._unHash hash))
    res <- SD.doesFileExist p
    unless res (writeFileLn p bts)

-- | Internal. Modified from 'Data.ByteString.Char8.writeFile'.
writeFileLn :: FilePath -> ByteString -> IO ()
writeFileLn f txt = withBinaryFile f WriteMode (\h -> BSC.hPutStrLn h txt)

-- * Lenses

makeLenses ''Paths
