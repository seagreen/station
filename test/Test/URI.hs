
module Test.URI where

import           Import

import           Test.Hspec
import           URI.ByteString

import           Base64URL
import           Station.Types

test :: SpecWith ()
test = do
    describe "the 'card' URI scheme implementation" $ do
        it "handles a simple uri correctly" $ do
            let uri = CardURI "card:?id=76cff443-d5d0-4420-872b-df0aaf664f93&version=blake2b-64ue:123" :: CardURI (Link VersionHash)
                expected = LFLink Link
                    { _linkId   = unsafeId "76cff443-d5d0-4420-872b-df0aaf664f93"
                    , _linkHash = VersionHash
                        { _unVersionHash = Hash
                            { _unHash = AlreadyBase64URL {_unBase64URL = "123" }
                            }
                        }
                    }
            linkFromURI uri `shouldBe` Right expected
        it "fails correctly if given an empty string" $ do
            let expected = RelativeRef
                               { rrAuthority = Nothing
                               , rrPath      = ""
                               , rrQuery     = Query {queryPairs = []}
                               , rrFragment  = Nothing
                               }
            linkFromURI (CardURI mempty ::  CardURI (Link VersionHash))
                `shouldBe` Left (RelativeRefNoFragment expected)
