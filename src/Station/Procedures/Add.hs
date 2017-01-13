
module Station.Procedures.Add where

import           Import

import qualified Data.HashMap.Strict        as HM
import qualified Data.List.NonEmpty         as NE
import qualified Data.Semigroup             as SG
import qualified Data.Set                   as SET
import qualified Data.Text                  as T

import           Station.JSON
import           Station.Lookup
import qualified Station.Procedures.General as GEN
import qualified Station.Schema             as SC
import           Station.Schema.Failure     (Invalid(..), InvalidData(..))
import qualified Station.Schema.Failure     as SCF
import           Station.Types

data SchemaNotFound
    = SchemaNotFound SchemaLink
    deriving (Show, Typeable)

instance Exception SchemaNotFound

data SchemaDecodeFailure
    = SchemaDecodeFailure SchemaLink Text
    deriving (Show, Typeable)

instance Exception SchemaDecodeFailure

data VersionInvalid = VersionInvalid
    { _vInvalidVerserion :: Version CardHash
    , _vInvalidFailure   :: InvalidData
    } deriving (Show, Typeable)

instance Exception VersionInvalid

add :: (MonadThrow m, MonadBase n m)
    => StationDetails n
    -> Deck
    -> Id
    -> Maybe VersionInfo
    -> CardBytes
    -> m (Either Invalid (Deck, VersionHash))
add station oldDeck i maybeParent card =
    case maybeParent of
        -- NOTE: Rewrite with less branching.
        Nothing     -> addNoDuplicateCheck station oldDeck i mempty card
        Just parent ->
            if Just card == (snd <$> _versionCard (_vcVersion parent))
                then pure (Right (oldDeck, _vcHash parent))
                else addNoDuplicateCheck
                         station
                         oldDeck
                         i
                         [_vcHash parent]
                         card

addNoDuplicateCheck
    :: forall m n. (MonadThrow m, MonadBase n m)
    => StationDetails n
    -> Deck
    -> Id
    -> [VersionHash]
    -> CardBytes
    -> m (Either Invalid (Deck, VersionHash))
addNoDuplicateCheck station oldDeck i parents card = do
    tm <- liftBase (_imGetTAI impl)
    let version = Version
            { _versionId      = i
            , _versionParents = parents
            , _versionCard    = Just cardHash
            , _versionAuthors = _stationAuthors station
            , _versionTime    = Just tm
            }
        versionBts = encodeProper version
        versionHash = VersionHash (hashProper versionBts)
        location = VersionLocation Nothing
        versionContext = VersionContext
            { _vcHash     = versionHash
            , _vcVersion  = version & versionCard .~ Just (cardHash, card)
            , _vcLocation = pure location
            }

        deck = oldDeck & deckBytes %~ HM.insert (_unVersionHash versionHash) versionBts
                       & deckBytes %~ HM.insert (_unBlobHash frontHash) frontBts
                       & deckBytes %~ HM.insert (_unCardHash cardHash) cardBts
                       & deckVersions %~ HM.adjust (SET.insert versionHash) location
                       & deckIds %~ HM.insertWith adjustId i (pure versionContext)

    case eitherDecodeStrict frontBts of
        Left e  -> pure . Left $ SCF.InvalidEncoding frontBts (T.pack e)
        Right v -> do
            schema <- getSchema deck
            case SC.validate (GEN.get deck) schema v of
                Left invalid -> pure . Left . SCF.InvalidContent $ invalid
                Right ()     -> do
                    whenLeft (GEN.validateVersion deck version)
                             (throwM . VersionInvalid version)
                    liftBase $ do
                        _imWriteBytes impl (_unBlobHash frontHash) frontBts
                        _imWriteBytes impl (_unCardHash cardHash) cardBts
                        _imWriteVersion impl versionHash versionBts
                    pure (Right (deck, versionHash))
  where
    impl :: Implementation n
    impl = _stationImplementation station

    frontBts  = _cardInstance card
    frontHash = BlobHash (hashProper frontBts)

    cardMag  = magFromCard card
    cardBts  = encodeProper cardMag
    cardHash = CardHash (hashProper cardBts)

    getSchema :: Deck -> m SC.Schema
    getSchema deck =
        let link = _cardSchema card
            hash = _linkHash (_unSchemaLink link)
        in case bytesFromBlobOrVersionHash (GEN.get deck) hash of
            Nothing    -> throwM (SchemaNotFound link)
            Just bytes ->
                case eitherDecodeStrict bytes of
                    Left e -> throwM (SchemaDecodeFailure link (T.pack e))
                    Right schema -> pure schema

    adjustId
        :: NonEmpty VersionInfo
        -> NonEmpty VersionInfo
        -> NonEmpty VersionInfo
    adjustId newVersion xs =
        let ys = NE.filter (\info -> _vcHash info `notElem` parents) xs
        in case NE.nonEmpty ys of
            Nothing -> newVersion
            Just zs -> GEN.sortVersions $ newVersion SG.<> zs
