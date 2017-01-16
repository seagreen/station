
module Station where

import           Import                                 hiding (set)

import qualified Data.ByteString                        as BS
import qualified Data.HashMap.Strict                    as HM
import qualified Data.Set                               as SET
import qualified Data.Text                              as T
import qualified System.Directory                       as SD
import           System.FilePath                        ((</>))

import qualified Station.Implementation                 as IM
import qualified Station.Implementation.PlainFilesystem as PF
import qualified Station.Procedures.Add                 as SPA
import           Station.Procedures.Build               (VersionNotFound(..),
                                                         buildDeck)
import qualified Station.Procedures.General             as SP
import           Station.JSON                           (encodeProper)
import qualified Station.Original                       as SO
import qualified Station.Schema                         as SC
import           Station.Schema.Failure                 (Invalid)
import           Station.Types

-- * Main API
--
-- These are your basic CRUD operations (only renamed).
--
-- This is definitely a datastore and not a database. To perform more
-- compicated queries than 'resolve' you have to get the 'Deck' value
-- out of the @MonadState@ and examine it manually.

resolve :: Station m n => Id                            -> m (Maybe VersionInfo)
new     :: Station m n =>                     CardBytes -> m (Either Invalid (Link VersionHash))
update  :: Station m n => Link VersionHash -> CardBytes -> m (Either Invalid VersionHash)
archive :: Station m n => Link VersionHash              -> m ()

new card = do
    station <- ask
    i       <- liftBase (_imNewId (_stationImplementation station))
    deck    <- get
    res     <- SPA.add station deck i Nothing card
    case res of
        Left e             -> pure (Left e)
        Right (deck',hash) -> put deck' >> pure (Right (Link i hash))

data CardNotFound
    = CardNotFound Id
    deriving (Show, Typeable)

instance Exception CardNotFound

data ParentHasChanged
    = ParentHasChanged Id VersionHash VersionHash
    deriving (Show, Typeable)

instance Exception ParentHasChanged

data IdConflict
    = IdConflict Id [VersionInfo]
    | IdConflictHashes Id [VersionHash]
    deriving (Show, Typeable)

instance Exception IdConflict

update (Link i argHash) card = do
    station <- ask
    deck    <- get
    case SP.resolveId deck i of
        []       -> throwM (CardNotFound i)
        [parent] -> do
            let parentHash = _vcHash parent
            when (argHash /= parentHash)
                 (throwM (ParentHasChanged i argHash parentHash))
            res <- SPA.add station deck i (Just parent) card
            case res of
                Left e             -> pure (Left e)
                Right (deck',hash) -> put deck' >> pure (Right hash)
        parents -> throwM (IdConflict i parents)

resolve i = do
    deck <- get
    case SP.resolveId deck i of
        []            -> pure Nothing
        [versionInfo] -> pure (Just versionInfo)
        versionInfos  -> throwM (IdConflict i versionInfos)

-- NOTE: If the target card isn't found then nothing happens.
archive (Link i argHash) = do
    station <- ask
    tm      <- liftBase (_imGetTAI (_stationImplementation station))
    deck    <- get
    case SP.resolveId deck i of
        []       -> pure ()
        [parent] -> do
            let parentHash = _vcHash parent
            when (argHash /= parentHash)
                 (throwM (ParentHasChanged i argHash parentHash))
            let version = Version
                    { _versionId      = i
                    , _versionParents = pure parentHash
                    , _versionCard    = Nothing
                    , _versionAuthors = _stationAuthors station
                    , _versionTime    = Just tm
                    } :: Version CardHash
                versionBts  = encodeProper version
                versionHash = VersionHash (hashProper versionBts)
            whenLeft (SP.validateVersion deck version)
                     (throwM . SPA.VersionInvalid version)
            vli <- case versionContents (`HM.lookup` _deckBytes deck) version of
                       Left e  -> throwM e
                       Right a -> pure a
            let location    = VersionLocation Nothing
                versionInfo = VersionContext
                    { _vcHash     = versionHash
                    , _vcVersion  = vli
                    , _vcLocation = pure location
                    }
            liftBase (_imWriteVersion (_stationImplementation station)
                                   versionHash versionBts)
            put $ deck & deckBytes %~ HM.insert (_unVersionHash versionHash) versionBts
                       & deckVersions %~ HM.adjust (SET.insert versionHash) location
                       & deckIds %~ HM.insert i (pure versionInfo)
        parents -> throwM (IdConflict i parents)

-- * Convenience

versionCount :: forall m n. Station m n => Id -> m Int
versionCount i = do
    deck <- get
    case SP.resolveId deck i of
        []            -> pure 0
        [versionInfo] -> f deck 1 (_vcHash versionInfo)
        versionInfos  -> throwM (IdConflict i versionInfos)
  where
    f :: Deck -> Int -> VersionHash -> m Int
    f deck n hash =
        case SP.getVersion deck hash of
            Nothing      -> throwM (VersionNotFound hash)
            Just version ->
                case _versionParents version of
                    []           -> pure n
                    [parentHash] -> f deck (n+1) parentHash
                    parents      -> throwM (IdConflictHashes i parents)

-- | A glorified @type@ wrapper, not used to enforce invariants.
newtype SchemaBytes
    = SchemaBytes { _unSchemaBytes :: ByteString }
    deriving (Eq, Show)

-- | For casual use in places like tests.
--
-- For actual schemas it's better to write them as bytestrings instead
-- of 'SC.Schema's. Encoding an `SC.Schema` will do things like change
-- `1` to `1.0`, which may not be what you want.
encodeSchemaBytes :: SC.Schema -> SchemaBytes
encodeSchemaBytes = SchemaBytes . encodeProper . Object . SC._unSchema

-- | Specialized 'new'.
newSchema
    :: Station m n
    => SchemaBytes
    -> Maybe Text
       -- ^ Schema name
    -> m (Either Invalid SchemaLink)
newSchema sch name = fmap schemaLink <$> new card
  where
    card :: CardBytes
    card = Card
        { _cardSchema   = SO.customSchemaLink
        , _cardInstance = _unSchemaBytes sch
        , _cardName     = name
        }

    -- NOTE: By turning a @VersionHash@ into a @BlobOrVersionHash@
    -- here using @BVVersion@ we lose data about what we're returning.
    -- Should this be changed?
    schemaLink :: Link VersionHash -> SchemaLink
    schemaLink = SchemaLink . fmap BVVersion

-- * Convenience
--
-- The following all use 'Station.Implementation.PlainFilesystem'.

defaultPaths :: FilePath -> PF.Paths
defaultPaths dir = PF.Paths
    { PF._pathHashes   = dir </> "hashes"
    , PF._pathLocal    = dir </> "local_versions"
    , PF._pathVersions = dir </> "versions"
    }

loadDeckWithPaths :: PF.Paths -> IO (Implementation IO, Deck)
loadDeckWithPaths paths = do
    SD.createDirectoryIfMissing
        True -- Create its parents too
        (PF._pathHashes paths)
    SD.createDirectoryIfMissing
        True -- Create its parents too
        (PF._pathVersions paths)

    traverse_ (\(t,bts) -> BS.writeFile (PF._pathHashes paths </> t) bts)
              SO.originalHashDir

    hasLocalVersions <- SD.doesFileExist (PF._pathLocal paths)
    unless hasLocalVersions $ writeFile (PF._pathLocal paths) mempty

    BS.writeFile (PF._pathVersions paths </> "builtin") SO.originalVersionBytes

    let impl = IM.plainFilesystem paths
    (impl,) <$> buildDeck impl

-- | Loads from the current directory.
loadDeck :: IO (Implementation IO, Deck)
loadDeck = do
    dir <- SD.getCurrentDirectory
    loadDeckWithPaths (defaultPaths dir)

runDeck
    :: PF.Paths
    -> Deck
    -> [AuthorLink]
    -> ReaderT (StationDetails IO) (StateT Deck IO) a
    -> IO (a, Deck)
runDeck paths deck authors f =
    runStateT
        (runReaderT f (StationDetails (IM.plainFilesystem paths) authors))
        deck

tempDeck :: IO (PF.Paths, Deck)
tempDeck = do
    tmp <- SD.getTemporaryDirectory
    i   <- _unId <$> newId
    let dir = tmp </> T.unpack i
        paths = defaultPaths dir
    (_,deck) <- loadDeckWithPaths (defaultPaths dir)
    pure (paths,deck)

-- | Doesn't set any authorship.
runTempDeck
    :: (PF.Paths -> ReaderT (StationDetails IO) (StateT Deck IO) a)
       -- ^ The @PF.Paths@ is provided for debugging.
    -> IO (a, Deck)
runTempDeck f = do
    (paths,deck) <- tempDeck
    runDeck paths deck mempty (f paths)
