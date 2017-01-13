
module Station.Procedures.Build where

import           Import

import qualified Data.HashMap.Strict        as HM
import qualified Data.List.NonEmpty         as NE
import qualified Data.Semigroup             as SG
import qualified Data.Text                  as T

import           Station.Procedures.General
import           Station.Types

data ParseVersionFailure = ParseVersionFailure
    { _pvfVersionBytes :: ByteString
    , _pvfErrorMessage :: Text
    } deriving (Show, Typeable)

instance Exception ParseVersionFailure

-- TODO: check the validity of each card with respect to its schema.
buildDeck :: MonadThrow m => Implementation m -> m Deck
buildDeck impl = do
    cardHm   <- _imBuildBytes impl
    versions <- _imBuildVersions impl
    Deck cardHm versions <$> buildVersions cardHm versions

data VersionNotFound
    = VersionNotFound VersionHash
    deriving (Show, Typeable)

instance Exception VersionNotFound

data VersionCycle
    = VersionCycle (NonEmpty VersionInfo)
    deriving (Show, Typeable)

instance Exception VersionCycle

buildVersions
    :: forall m. MonadThrow m
    => HashMap Hash ByteString
    -> HashMap VersionLocation (Set VersionHash)
    -> m (HashMap Id (NonEmpty VersionInfo))
buildVersions cardsHm locationHm = do
    versionHm <- foldrM addVersion mempty flatLocations
    let sortedHm = sortVersions <$> versionHm
    traverse removeSuperseded sortedHm
  where
    flatLocations :: [(VersionHash, NonEmpty VersionLocation)]
    flatLocations = HM.toList (HM.foldrWithKey f mempty locationHm)
      where
        f :: VersionLocation
          -> Set VersionHash
          -> HashMap VersionHash (NonEmpty VersionLocation)
          -> HashMap VersionHash (NonEmpty VersionLocation)
        f location versions acc = foldr g acc versions
          where
            g :: VersionHash
              -> HashMap VersionHash (NonEmpty VersionLocation)
              -> HashMap VersionHash (NonEmpty VersionLocation)
            g hash = HM.insertWith (SG.<>) hash (pure location)

    addVersion
        :: (VersionHash, NonEmpty VersionLocation)
        -> HashMap Id (NonEmpty VersionInfo)
        -> m (HashMap Id (NonEmpty VersionInfo))
    addVersion (hash,locations) acc =
        case HM.lookup (_unVersionHash hash) cardsHm of
            Nothing  -> throwM (VersionNotFound hash)
            Just bts -> do
                version <- fillContents =<< parseVersion bts
                let versionContext = VersionContext
                                         { _vcHash     = hash
                                         , _vcVersion  = version
                                         , _vcLocation = locations
                                         }
                pure $ HM.alter (\a -> case a of
                                           Nothing -> Just (pure versionContext)
                                           Just b  -> Just (versionContext NE.<| b)
                                ) (_versionId version) acc

    parseVersion :: ByteString -> m (Version CardHash)
    parseVersion bts =
        case eitherDecodeStrict bts of
            Left e  -> throwM (ParseVersionFailure bts (T.pack e))
            Right a -> pure a

    fillContents :: Version CardHash -> m (Version (CardHash, CardBytes))
    fillContents x =
        case versionContents (flip HM.lookup cardsHm) x of
            Left e  -> throwM e
            Right a -> pure a

    removeSuperseded :: NonEmpty VersionInfo -> m (NonEmpty VersionInfo)
    removeSuperseded xs =
        -- PERFORMANCE: so bad
        let allParents = _versionParents . _vcVersion =<< NE.toList xs
            res = NE.filter (\x -> _vcHash x `notElem` allParents) xs
        in case NE.nonEmpty res of
            Nothing -> throwM (VersionCycle xs)
            Just ys -> pure ys
