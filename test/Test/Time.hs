
module Test.Time where

import           Import

import           Test.Hspec

import           Station.Types.Card.Time

test :: SpecWith ()
test = do
    describe "the TAI implementation" $ do
        it "has the correct unixEpoch" $ do
            (show unixEpoch :: Text) `shouldBe` "1970-01-01 00:00:00 UTC"
