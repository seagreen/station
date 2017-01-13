
module Station.Implementation where

import           Import

import qualified Station.Implementation.PlainFilesystem as PF
import           Station.Types                          (Implementation(..))
import qualified Station.Types                          as ST

plainFilesystem :: PF.Paths -> Implementation IO
plainFilesystem paths = Implementation
    { _imWriteBytes    = PF.writeBytes    paths
    , _imWriteVersion  = PF.writeVersion  paths
    , _imBuildBytes    = PF.buildBytes    (PF._pathHashes paths)
    , _imBuildVersions = PF.buildVersions paths
    , _imNewId         = ST.newId
    , _imGetTAI        = ST.getTAI
    }
