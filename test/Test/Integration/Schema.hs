
module Test.Integration.Schema where

import           Import

import qualified Data.HashMap.Strict as HM
import           JSONSchema.Types    (Schema (..))

import qualified Station.Types       as ST

-- * Schemas

invalidSchema :: Schema
invalidSchema = Schema (HM.singleton "type" (Bool True))

noneSchema :: Schema
noneSchema = Schema (HM.singleton "not" (Object mempty))

anySchema :: Schema
anySchema = Schema mempty

minimalSchema :: Schema
minimalSchema = Schema (HM.singleton "type" (String "null"))

linkSchema :: Schema
linkSchema = Schema $ HM.fromList
    [ ("type", String "object")
    , ("required", toJSON [String "foo"])
    , ("properties", object
        [ "foo" .= object
            [ "type" .= String "object"
            , "linkTo" .= object
                [ "type" .= String "array" ]
            ]
        ]
      )
    ]

-- * Values

emptyArray :: Value
emptyArray = Array mempty

emptyObject :: Value
emptyObject = Object mempty

mkLink :: ST.Link ST.VersionHash -> Value
mkLink a = object [ "foo" .= a ]
