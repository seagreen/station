{-# LANGUAGE ConstraintKinds #-}

module Station.Types.Implementation where

import           Import

import           Station.Types.Card
import           Station.Types.Deck
import           Station.Types.Version
import           Station.Types.VersionContext

-- | 'n' is what you code your 'Implementation' to. This is how station
-- knows how to store bytes, get the current time, get random numbers, etc.
-- It has no frilly requirements other than being a monad. For example,
-- the 'plainFilesystem' implementation uses 'IO'.
--
-- 'm' is what the functions in the station API run in. It just exists to
-- make them more readable. For instance, without it 'Station.new' would have
-- to take two more arguments: @StationDetails n@ and @Deck@, and well as
-- returning a more complicated error type along with a modified @Deck@
-- (in addition to what it already returns).
type Station m n = ( MonadBase n m
                   , Monad n
                   , MonadReader (StationDetails n) m
                   , MonadState Deck m
                   , MonadThrow m
                   )

data StationDetails n = StationDetails
    { _stationImplementation :: Implementation n
    , _stationAuthors        :: [AuthorLink]
      -- ^ For decks with a sigle owner this will have one item.
      -- It defines who will be credited with each new version
      -- added to the deck.
    }

data Implementation n = Implementation
    { _imWriteBytes    :: Hash -> ByteString -> n ()
    , _imWriteVersion  :: VersionHash -> ByteString -> n ()
    , _imBuildBytes    :: n (HashMap Hash ByteString)
    , _imBuildVersions :: n (HashMap VersionLocation (Set VersionHash))
    , _imNewId         :: n Id
    , _imGetTAI        :: n TAI
    }
