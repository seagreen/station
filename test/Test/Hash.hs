
module Test.Hash where

import           Import

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as HEX
import qualified Data.Text              as T
import qualified System.Directory       as SD
import           System.FilePath        ((</>))
import qualified Turtle                 as TU

import qualified Station.Original       as SO
import qualified Station.Types          as ST

import           Base64URL              (Base64URL(..), encodeBase64URL)
import           Test.Hspec

versionSchemaHash :: ST.Hash
versionSchemaHash = ST.unsafeHash
    "0HrMk5vdexw18b-4mj3mmw8bptVK5BMqDx1kU3MEvATJUjfDk3QV9ClmpBP5ElAvXXDoO47uzIITrq4n3UXO2Q"

cardSchemaHash :: ST.Hash
cardSchemaHash = ST.unsafeHash
    "r3DGCv1k--u3umo1onVrovCp2bbcC2h_3zOfIFSJpcZZbDexik55KfoZQRM4TIt1tDvDsp1rOYlIA61GurCAUw"

customSchemaHash :: ST.Hash
customSchemaHash = ST.unsafeHash
    "9W1glLd04HDurDkySkwBCcJ234Cht2ZiqEoDiDBQpH_gxAxLuXQvFO7lnBb4ZS6JtGt6Del4_m05f6ixHpXE5A"

test :: SpecWith ()
test = do
    describe "the hash of our custom draft 4 schema" $ do
        testHash SO.customSchemaHash customSchemaHash SO.customSchemaBytes
    describe "the hash of our card schema" $ do
        testHash SO.cardSchemaHash cardSchemaHash SO.cardSchemaBytes
    describe "the hash of our version schema" $ do
        testHash SO.versionSchemaHash versionSchemaHash SO.versionSchemaBytes

testHash :: ST.Hash -> ST.Hash -> ByteString -> SpecWith ()
testHash generatedHash recordedHash bts = do
    it "matches the hash on store" $ do
        generatedHash `shouldBe` recordedHash
    it "matches the output of b2sum" $ do
        res <- hashWithB2Sum bts
        generatedHash `shouldBe` ST.unsafeHash res

hashWithB2Sum :: ByteString -> IO Text
hashWithB2Sum bts = do
    path <- T.pack <$> tempFile bts
    let b2sumCommand = "b2sum -a blake2b " <> path
    t <- TU.strict (TU.inshell b2sumCommand mempty)
    pure . _unBase64URL
         . encodeBase64URL
         -- ^ NOTE: These should be replaced with a function that isn't from
         -- our library, perhaps by calling out to the command line.
         . hexDecode
         . extractHash
         $ t
  where
    -- Extract the hash from the output of b2sum, for example from:
    --
    -- 34eccfc82668a1b01e92cc9530e5d94f38148cfee42f6f905cc92a4a5677ec58378779194b79a19e97e3484d84783ced08e502a657b2a19c10f355d41cc65bee /run/user/1000/faa98fed-c8ef-43fa-bee3-635b0683257a\n
    extractHash :: Text -> Text
    extractHash t =
        case T.splitOn " " t of
            [a,_] -> a
            _     -> panic ("hashWithB2Sum process failed: " <> show t)

    hexDecode :: Text -> ByteString
    hexDecode t =
        case HEX.decode (encodeUtf8 t) of
            (a, "") -> a
            _       -> panic ("hashWithB2Sum HEX.decode failed: " <> show t)

tempFile :: ByteString -> IO FilePath
tempFile bts = do
    tmp <- SD.getTemporaryDirectory
    i <- ST._unId <$> ST.newId
    let path = tmp </> T.unpack i
    BS.writeFile path bts
    pure path
