
module Station.Schema
    ( module Station.Schema
    , Schema(..)
    ) where

import           Import

import qualified Data.List.NonEmpty              as NE
import           Data.Profunctor                 (Profunctor(..))
import           JSONSchema.Types                (Schema(..), Spec(..))
import qualified JSONSchema.Types                as JST
import qualified JSONSchema.Validator.Draft4     as D4

import           Station.Schema.Failure
import qualified Station.Schema.Validator.HashOf as HO
import qualified Station.Schema.Validator.LinkTo as LT
import qualified Station.Schema.Validator.Ref    as REF
import qualified Station.Types                   as ST

-- * API

validate
    :: (ST.Hash -> Maybe ByteString)
    -> Schema
    -> Value
    -> Either InvalidData ()
validate getBytes s v =
    case NE.nonEmpty (validate' getBytes (REF.Scope s) s v) of
        Nothing       -> Right ()
        Just failures -> Left InvalidData
            { _invalidSchema   = s
            , _invalidInstance = v
            , _invalidFailures = failures
            }

-- | For use within this module.
--
-- The @REF.Scope Schema@ argument is used to resolve internal references
-- such as @"#/foo"@. It needs to be separate from the schema we're currently
-- resolving in case the latter is a subschema embedded in the original
-- document.
validate'
    :: (ST.Hash -> Maybe ByteString)
    -> REF.Scope Schema
    -> Schema
    -> Value
    -> [ValidatorFailure]
validate' = fmap JST.validate . d4Modified

-- * Spec

-- | TODO: Add loop detection.
d4Modified
    :: (ST.Hash -> Maybe ByteString)
    -> REF.Scope Schema
    -> Spec Schema ValidatorFailure
d4Modified getContent scope =
    Spec
        [ dimap f FailureMultipleOf D4.multipleOfValidator
        , dimap f FailureMaximum D4.maximumValidator
        , dimap f FailureMinimum D4.minimumValidator

        , dimap f FailureMaxLength D4.maxLengthValidator
        , dimap f FailureMinLength D4.minLengthValidator
        , dimap f FailurePattern D4.patternValidator

        , dimap f FailureMaxItems D4.maxItemsValidator
        , dimap f FailureMinItems D4.minItemsValidator
        , dimap f FailureUniqueItems D4.uniqueItemsValidator
        , dimap
            (fromMaybe D4.emptyItems . f)
            (\err -> case err of
                         D4.IRInvalidItems e      -> FailureItems e
                         D4.IRInvalidAdditional e -> FailureAdditionalItems e)
            (D4.itemsRelatedValidator recurse)

        , dimap f FailureMaxProperties D4.maxPropertiesValidator
        , dimap f FailureMinProperties D4.minPropertiesValidator
        , dimap f FailureRequired D4.requiredValidator
        , dimap f FailureDependencies (D4.dependenciesValidator recurse)
        , dimap
            (fromMaybe D4.emptyProperties . f)
            FailurePropertiesRelated
            (D4.propertiesRelatedValidator recurse)

        , dimap f FailureEnum D4.enumValidator
        , dimap f FailureType D4.typeValidator
        , dimap f FailureAllOf (D4.allOfValidator recurse)
        , dimap f FailureAnyOf (D4.anyOfValidator recurse)
        , dimap f FailureOneOf (D4.oneOfValidator recurse)
        , dimap f FailureNot (D4.notValidator recurse)

        , dimap
            f
            FailureRef
            (REF.refValidator getContent scope recurseWithNewScope)
        , dimap f FailureHashOf (HO.hashOfValidator getContent recurse)
        , dimap f FailureLinkTo (LT.linkToValidator getContent recurse)
        ]
  where
    f :: FromJSON a => Schema -> Maybe a
    f = fromJSONMaybe . Object . _unSchema

    recurse :: Schema -> Value -> [ValidatorFailure]
    recurse = validate' getContent scope

    recurseWithNewScope
        :: REF.Scope Schema
        -> Schema
        -> Value
        -> [ValidatorFailure]
    recurseWithNewScope = validate' getContent
