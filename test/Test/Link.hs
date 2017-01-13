
module Test.Link where

import           Import

import           Test.Hspec

import           Station.Types

test :: SpecWith ()
test = do
    describe "the Link implementation" $ do
        it "doesn't allow both versions and hashes" $ do
            let v = object
                        [ "id"      .= String "a4954652-8c2b-41d2-9f1d-dec61a2b4f97"
                        , "version" .= object [ "blake2b-64ue" .= String "123" ]
                        , "blob"    .= object [ "blake2b-64ue" .= String "456" ]
                        ]
                a = fromJSONEither v :: Either Text (Link BlobOrVersionHash)
            void (shouldBeLeft a) :: IO ()
