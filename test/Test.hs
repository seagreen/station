
module Main where

import           Import

import           Test.Hspec

import           Station.Types.Card.Time (truncateToThreeDecimals)
import qualified Test.Hash
import qualified Test.Integration
import qualified Test.Link
import qualified Test.Time
import qualified Test.URI

-- import qualified Data.Validator.Utils           as VU
-- import           Test.QuickCheck                (property)
-- import           Station.JSON                   (encodeProper)

main :: IO ()
main = hspec $ do
    Test.Hash.test
    Test.Link.test
    Test.Time.test
    Test.URI.test
    describe "truncateDecimal" $ do
        it "passes sanity checks" $ do
            truncateToThreeDecimals 1.2345 `shouldBe` 1.234
            truncateToThreeDecimals 1.0 `shouldBe` 1.0
            truncateToThreeDecimals 1 `shouldBe` 1
    Test.Integration.test
    -- (Failing and somewhat obsolete)
    --
    -- This isn't a huge deal, but we'd like the output of our standardized
    -- JSON encoding (no spaces, ordered keys, etc.) to match that of `jq`.
    --
    --
    -- describe "encodeProper" $ do
    --     it "gives the same result as using `jq`" $ do
    --         property $ \a -> do
    --             let v = VU._unArbitraryValue a
    --             jqRes <- encodeProperJQ v
    --             encodeProper v `shouldBe` jqRes
