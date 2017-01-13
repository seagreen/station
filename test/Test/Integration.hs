
module Test.Integration where

import           Import

import           Control.Monad.IO.Class     (MonadIO(..))
import qualified Data.ByteString            as BS
import           Data.Text.Encoding         (decodeUtf8)
import           Test.Hspec

import           Base64URL
import qualified Station                    as SN
import qualified Station.Implementation     as IM
import           Station.Procedures.Build   (buildDeck)
import           Station.Procedures.General (validateVersion)
import           Station.JSON
import qualified Station.Original           as SO
import qualified Station.Types              as ST
import           Test.Integration.Schema

test :: SpecWith ()
test = do
    describe "basic operations" $ do
        it "work as expected" $ void . SN.runTempDeck $ \paths -> do

            -- Print diagnostics

            liftIO (print paths)

            -- Make sure the meta schema version is valid.
            --
            -- Once we have a way to validate entire decks this will
            -- automatically be handled by validating the starting deck.

            startingDeck <- get
            metaSchemaVersion <- fmap ST._vcVersion
                               . shouldBeJust
                             =<< SN.resolve ST.customSchemaId
            shouldBeRight $ validateVersion startingDeck (fst <$> metaSchemaVersion)

            -- Check that the meta schema in the deck is really a canonicalized
            -- version of the human readable one.
            --
            -- Then do the same for the author schema.

            deckMetaVC <- shouldBeJust =<< SN.resolve ST.customSchemaId
            deckMetaBts <- shouldBeJust . fmap (ST._cardInstance . snd)
                         . ST._versionCard . ST._vcVersion $ deckMetaVC
            humanMeta <- liftIO (BS.readFile "schemas/human-optimized/draft4-modified.json")
            humanMetaJQ <- liftIO $ fmap encodeUtf8 . convertJQ . decodeUtf8 $ humanMeta
            liftIO $ deckMetaBts `shouldBe` SO.unsafeStripTrailingEOL humanMetaJQ

            deckAuthorVC <- shouldBeJust =<< SN.resolve ST.authorSchemaId
            deckAuthorBts <- shouldBeJust . fmap (ST._cardInstance . snd)
                           . ST._versionCard . ST._vcVersion $ deckAuthorVC
            humanAuthor <- liftIO (BS.readFile "schemas/human-optimized/author.json")
            humanAuthorJQ <- liftIO $ fmap encodeUtf8 . convertJQ . decodeUtf8 $ humanAuthor
            liftIO $ deckAuthorBts `shouldBe` SO.unsafeStripTrailingEOL humanAuthorJQ

            -- Add schemas

            void . shouldBeLeft =<< SN.newSchema (SN.encodeSchemaBytes invalidSchema) Nothing
            schNone    <- shouldBeRight =<< SN.newSchema (SN.encodeSchemaBytes noneSchema) Nothing
            schAny     <- shouldBeRight =<< SN.newSchema (SN.encodeSchemaBytes anySchema) Nothing
            schMinimal <- shouldBeRight =<< SN.newSchema (SN.encodeSchemaBytes minimalSchema) Nothing
            schLink    <- shouldBeRight =<< SN.newSchema (SN.encodeSchemaBytes linkSchema) Nothing

            -- Add instances of those schemas

            void . shouldBeLeft  =<< SN.new (ST.Card schNone (encodeProper Null) Nothing)
            void . shouldBeRight =<< SN.new (ST.Card schAny (encodeProper Null) Nothing)
            void . shouldBeRight =<< SN.new (ST.Card schMinimal (encodeProper Null) Nothing)
            void . shouldBeLeft  =<< SN.new (ST.Card schMinimal (encodeProper (Bool True)) Nothing)

            -- Test that "linkTo" is actually enforced

            let arrayCard = ST.Card schAny (encodeProper emptyArray) Nothing
            arrayLink@(ST.Link arrayId _) <- shouldBeRight =<< SN.new arrayCard
            let c1 = ST.Card schLink (encodeProper (mkLink arrayLink)) Nothing
            void . shouldBeRight =<< SN.new c1

            let emptyHash = ST.VersionHash . ST.Hash . AlreadyBase64URL $ mempty
                badLink = arrayLink&ST.linkHash.~emptyHash
                c2 = ST.Card schLink (encodeProper (mkLink badLink)) Nothing
            void . shouldBeLeft =<< SN.new c2

            let objectCard = ST.Card schAny (encodeProper emptyObject) Nothing
            objectLink <- shouldBeRight =<< SN.new objectCard
            let c3 = ST.Card schLink (encodeProper (mkLink objectLink)) Nothing
            void . shouldBeLeft =<< SN.new c3

            -- Test updating

            arrayN <- SN.versionCount arrayId
            liftIO $ arrayN `shouldBe` 1

            void . shouldBeRight =<< SN.update arrayId objectCard
            arrayNUpdated <- SN.versionCount arrayId
            liftIO $ arrayNUpdated `shouldBe` 2

            void . shouldBeRight =<< SN.update arrayId objectCard -- This shouldn't do anything.
            arrayNUnchanged <- SN.versionCount arrayId
            liftIO $ arrayNUnchanged `shouldBe` 2

            -- Test deletion

            SN.archive (ST._linkId objectLink)

            -- Test that the store in memory equals that on disk

            liveDeck <- get
            reloaded <- liftIO (buildDeck (IM.plainFilesystem paths))
            liftIO $ do
                ST._deckBytes liveDeck `shouldBe` ST._deckBytes reloaded
                ST._deckVersions liveDeck `shouldBe` ST._deckVersions reloaded
                ST._deckIds liveDeck `shouldBe` ST._deckIds reloaded
