-- | These are the main lookup function for the library.
--
-- Get the data instance refered to by a version or a card.

module Station.Lookup where

import           Import        hiding (get)

import           Station.Types

bytesFromBlobOrVersionHash
    :: (Hash -> Maybe ByteString)
    -> BlobOrVersionHash
    -> Maybe ByteString
bytesFromBlobOrVersionHash get (BVVersion hash) = bytesFromVersionHash get hash
bytesFromBlobOrVersionHash get (BVBlob hash)    = get (_unBlobHash hash)

bytesFromVersionHash
    :: (Hash -> Maybe ByteString)
    -> VersionHash
    -> Maybe ByteString
bytesFromVersionHash get (VersionHash hash) = do
    versionBts <- get hash
    case eitherDecodeStrict versionBts of
        Left _                              -> Nothing -- NOTE: Better error?
        Right (version :: Version CardHash) -> do
            cardHash <- _versionCard version
            bytesFromCardHash get cardHash

bytesFromCardHash
    :: (Hash -> Maybe ByteString)
    -> CardHash
    -> Maybe ByteString
bytesFromCardHash get (CardHash hash) = do
    cardBts <- get hash
    case eitherDecodeStrict cardBts of
        Left _     -> Nothing -- NOTE: Better error?
        Right card -> get (_unBlobHash (_cardInstance card))
