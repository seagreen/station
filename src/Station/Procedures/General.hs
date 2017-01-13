
module Station.Procedures.General where

import           Import                 hiding (get)

import qualified Data.HashMap.Strict    as HM
import qualified Station.Original       as SO
import qualified Station.Schema         as SC
import           Station.Schema.Failure (InvalidData)
import           Station.Types

import qualified Data.List.NonEmpty     as NE

get :: Deck -> Hash -> Maybe ByteString
get deck hash = HM.lookup hash (_deckBytes deck)

getVersion :: Deck -> VersionHash -> Maybe (Version CardHash)
getVersion deck hash = decodeStrict =<< get deck (_unVersionHash hash)

-- | This should return one card, if there's a versioning conflict
-- it will return more.
resolveId :: Deck -> Id -> [VersionInfo]
resolveId deck i =
    case HM.lookup i (_deckIds deck) of
        Nothing -> mempty
        Just xs -> NE.toList xs

-- | Also validates the back of the card (this happens automatically
-- through the version schema's @"linkTo"@ validator).
validateVersion :: Deck -> Version CardHash -> Either InvalidData ()
validateVersion deck version =
    SC.validate (get deck) SO.versionSchemaRefFilled (toJSON version)

-- | '_deckIds' should really use a non-empty set instead of a non-empty list.
-- In the meantime it's super important that list is sorted deterministically
-- both in 'add' and 'buildVersions'. Otherwise when we compare our decks on
-- disk and in memory they may be different.
sortVersions
    :: NonEmpty VersionInfo
    -> NonEmpty VersionInfo
sortVersions = NE.sortWith _vcHash
