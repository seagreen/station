
module Station.Schema.Failure where

import           Import

import           JSONSchema.Types                (Schema)
import qualified JSONSchema.Validator.Draft4     as VAL

import qualified Station.Schema.Validator.HashOf as CUSTOM
import qualified Station.Schema.Validator.LinkTo as CUSTOM
import qualified Station.Schema.Validator.Ref    as CUSTOM

data Invalid
    = InvalidEncoding ByteString Text
    | InvalidContent InvalidData
    deriving (Eq, Show)

-- | Used to report an entire instance being invalidated, as opposed
-- to the failure of a single validator.
data InvalidData = InvalidData
    { _invalidSchema   :: Schema
    , _invalidInstance :: Value
    , _invalidFailures :: NonEmpty ValidatorFailure
    } deriving (Eq, Show)

data ValidatorFailure
    = FailureMultipleOf VAL.MultipleOfInvalid
    | FailureMaximum    VAL.MaximumInvalid
    | FailureMinimum    VAL.MinimumInvalid

    | FailureMaxLength VAL.MaxLengthInvalid
    | FailureMinLength VAL.MinLengthInvalid
    | FailurePattern   VAL.PatternInvalid

    | FailureMaxItems        VAL.MaxItemsInvalid
    | FailureMinItems        VAL.MinItemsInvalid
    | FailureUniqueItems     VAL.UniqueItemsInvalid
    | FailureItems           (VAL.ItemsInvalid ValidatorFailure)
    | FailureAdditionalItems (VAL.AdditionalItemsInvalid ValidatorFailure)

    | FailureMaxProperties     VAL.MaxPropertiesInvalid
    | FailureMinProperties     VAL.MinPropertiesInvalid
    | FailureRequired          VAL.RequiredInvalid
    | FailureDependencies      (VAL.DependenciesInvalid ValidatorFailure)
    | FailurePropertiesRelated (VAL.PropertiesRelatedInvalid ValidatorFailure)

    | FailureEnum  VAL.EnumInvalid
    | FailureType  VAL.TypeValidatorInvalid
    | FailureAllOf (VAL.AllOfInvalid ValidatorFailure)
    | FailureAnyOf (VAL.AnyOfInvalid ValidatorFailure)
    | FailureOneOf (VAL.OneOfInvalid ValidatorFailure)
    | FailureNot   VAL.NotValidatorInvalid

    | FailureRef    (CUSTOM.RefInvalid ValidatorFailure)
    | FailureHashOf (CUSTOM.HashOfInvalid ValidatorFailure)
    | FailureLinkTo (CUSTOM.LinkToInvalid ValidatorFailure)
    deriving (Eq, Show)
