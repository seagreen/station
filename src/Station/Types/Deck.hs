{-# LANGUAGE TemplateHaskell #-}

module Station.Types.Deck where

import           Import

import           Lens.Micro.TH

import           Station.Types.Card
import           Station.Types.VersionContext

data Deck = Deck
    { _deckBytes    :: HashMap Hash ByteString
    -- ^ Maps directly to the '_pathHashes' directory.
    , _deckVersions :: HashMap VersionLocation (Set VersionHash)
    -- ^ Maps to the '_pathVersions' directory.
    , _deckIds      :: HashMap Id (NonEmpty VersionInfo)
    -- ^ Purely derived.
    --
    -- If the value of the HashMap has more than one element than there's
    -- an 'Id' conflict.
    } deriving Show

-- * Lenses

makeLenses ''Deck
