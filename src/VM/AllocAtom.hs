module VM.AllocAtom where
-- import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Data.Set as S
-- import Data.List
-- import Data.Bifunctor 
-- import Data.Tuple.Extra
import Compiler.Compiler hiding (Envs)
import Util.Util (
--  mapEitherList,
--  monadicMapAccumL,
  monadicFoldl
  )
import GHC.Base
import VM.Heap
import VM.Envs (
  Envs (..),
  nullEnvs,
  updateIncommingLinks,
  addMatchedLocalAddrs,
  -- updateMatchedLocalAddrs,
  addLocalLink2Addr,
  -- updateLocalLink2Addr,
  updateFreeLink2Addr,
  addFreeLink2Addr,
  updateFreeAddr2Indeg  
  )
