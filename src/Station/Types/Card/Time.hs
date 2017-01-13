
module Station.Types.Card.Time where

import           Import

import           Data.Scientific                      (Scientific)
import qualified Data.Time.Calendar                   as CAL
import           Data.Time.Clock                      (UTCTime(..), DiffTime,
                                                       getCurrentTime)
import           Data.Time.Clock.AnnouncedLeapSeconds (lst)
import qualified Data.Time.Clock.TAI                  as TAI

-- | International Atomic Time (Temps Atomique International).
--
-- Stored and serialized as the number of seconds since the Unix epoch.
newtype TAI
    = TAI { _unTAI :: Scientific }
    deriving (Eq, Show, FromJSON, ToJSON)

getTAI :: IO TAI
getTAI = TAI
       . truncateToThreeDecimals
         -- ^ TODO: Move truncation to serialization.
       . taiFromUTC
     <$> getCurrentTime

taiFromUTC :: UTCTime -> Scientific
taiFromUTC = realToFrac . f . TAI.utcToTAITime lst
  where
    -- The only way the library allows getting out of @AbsoluteTime@
    -- is through @diffAbsoluteTime@.
    f :: TAI.AbsoluteTime -> DiffTime
    f current = TAI.diffAbsoluteTime -- diffAbsoluteTime a b = a - b
                    current
                    (TAI.utcToTAITime lst unixEpoch)

unixEpoch :: UTCTime
unixEpoch = UTCTime
    { utctDay     = CAL.ModifiedJulianDay 40587
    , utctDayTime = 0
    }

-- | See this answer:
-- http://stackoverflow.com/a/31952975/1132816
--
-- TODO: There really should be a robost rounding function in a lib
-- somewhere. For one thing we should be rounding not truncating.
truncateToThreeDecimals :: Scientific -> Scientific
truncateToThreeDecimals a = fromIntegral (floor (a * b) :: Int) / b
  where
    b :: Scientific
    b = 10 ^ (3 :: Int)
