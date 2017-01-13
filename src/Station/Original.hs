{-# LANGUAGE TemplateHaskell #-}

module Station.Original where

import           Import

import           Data.Aeson.Lens
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict   as HM
import           Data.FileEmbed        (embedDir, embedFile,
                                        makeRelativeToProject)
import           JSONSchema.Types      (Schema(..))

import           Base64URL
import qualified Station.Schema        as SC
import qualified Station.Types         as ST

-- * Original Deck

originalHashDir :: [(FilePath, ByteString)]
originalHashDir =
    $(makeRelativeToProject "deck/hashes" >>= embedDir)

originalVersionBytes :: ByteString
originalVersionBytes =
    $(makeRelativeToProject "deck/local_versions" >>= embedFile)

-- * Custom JSON Schema for Station

customSchema :: Schema
customSchema = unsafeSchema customSchemaBytes

customSchemaLink :: ST.SchemaLink
customSchemaLink = ST.SchemaLink ST.Link
    { ST._linkId   = ST.customSchemaId
    , ST._linkHash = ST.BVVersion customSchemaVersionHash
    }

customSchemaVersionHash :: ST.VersionHash
customSchemaVersionHash =
    ST.VersionHash . ST.unsafeHash $
        "6sZrwr61VVVdqvioSeQQpLzWIveeyDk3grCiv10ge6I4wU3Mxp7_aVHvDdcpfEBCusgu5GiQEnuI4L65w5Cx9w"

customSchemaHash :: ST.Hash
customSchemaHash = ST.hashProper customSchemaBytes

customSchemaBytes :: ByteString
customSchemaBytes =
    unsafeStripTrailingEOL
        $( makeRelativeToProject
               ( concat [ "deck/"
                        , "hashes/"
                        , "9W1glLd04HDurDkySkwBCcJ234Cht2ZiqEoDiDBQpH_g"
                        , "xAxLuXQvFO7lnBb4ZS6JtGt6Del4_m05f6ixHpXE5A"
                        ]
               )
           >>= embedFile
         )

-- * Card back schema

cardSchema :: Schema
cardSchema = unsafeSchema cardSchemaBytes

cardSchemaHash :: ST.Hash
cardSchemaHash = ST.hashProper cardSchemaBytes

cardSchemaBytes :: ByteString
cardSchemaBytes =
    unsafeStripTrailingEOL
        $(makeRelativeToProject "schemas/canonical/card.json" >>= embedFile)

-- * Version schema

-- | Version schemas reference card schemas. We don't want to add either
-- to the datastore as actual cards yet because they may change too much,
-- for now they're just built into this library.
--
-- This means that we can't add the card schema to the datastore so version
-- schema's "hashOf" field can look it up -- the reference might get garbage
-- collected (when that's added). So instead when doing validation we
-- use this overwritten version schema that's had it card schema
-- reference replaced with the actual card schema.
--
-- NOTE: We should validate the modified schema itself.
versionSchemaRefFilled :: Schema
versionSchemaRefFilled =
    let v = _unSchema versionSchema
    in case (^? refGet) =<< HM.lookup "properties" v of
        Nothing -> panic "versionSchemaRefFilled failed - not found"
        Just a  ->
            if a == expected
                then Schema (HM.adjust (refSet .~ _unSchema cardSchema) "properties" v)
                else panic "versionSchemaRefFilled failed - not replacing unexpected value"
  where
    -- | TODO:
    --
    -- 1. Unify the two lens related functions.
    --
    -- 2. Move the lookup of "properties" into them.
    --
    -- 3. Add type signatures.
    refGet = key "card" . key "oneOf" . nth 1 . key "hashOf" . _Object

    refSet = key "card" . key "oneOf" . nth 1 . key "hashOf" . _Object

    expected :: HashMap Text Value
    expected =
        HM.singleton
            "$ref"
            (String (  "card:?id="
                    <> ST._unId ST.cardSchemaId
                    <> "&hash=blake2b-64ue:"
                    <> _unBase64URL (ST._unHash cardSchemaHash)
                    ))

versionSchema :: Schema
versionSchema = unsafeSchema versionSchemaBytes

versionSchemaHash :: ST.Hash
versionSchemaHash = ST.hashProper versionSchemaBytes

versionSchemaBytes :: ByteString
versionSchemaBytes =
    unsafeStripTrailingEOL
        $(makeRelativeToProject "schemas/canonical/version.json" >>= embedFile)

-- | Internal.
unsafeStripTrailingEOL :: ByteString -> ByteString
unsafeStripTrailingEOL bts =
    case stripTrailingEOL bts of
        Left _  -> panic ("unsafeStripTrailingEOL: " <> show bts)
        Right a -> a

-- | Internal.
stripTrailingEOL :: ByteString -> Either (Maybe Char) ByteString
stripTrailingEOL oldBts =
    case BSC.unsnoc oldBts of
        Nothing      -> Left Nothing
        Just (bts,c) ->
            if c == '\n'
                then Right bts
                else Left (Just c)

-- | Internal.
unsafeSchema :: ByteString -> Schema
unsafeSchema bts =
    case decodeStrict bts of
        Nothing -> panic "unsafeSchema decode failed"
        Just hm  ->
            case SC.validate failGet customSchemaUnvalidated (Object hm) of
                Left e  -> panic ("unsafeSchema validation failed: " <> show e)
                Right _ -> Schema hm
  where
    failGet :: a -> Maybe b
    failGet = panic "unsafeSchema doesn't handle references"

    customSchemaUnvalidated :: Schema
    customSchemaUnvalidated =
        fromMaybe (panic "unsafeSchema could not decode metaschema")
                  (decodeStrict customSchemaBytes)
