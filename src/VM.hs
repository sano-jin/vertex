module VM where
import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Data.Bifunctor 
import Compiler hiding (Envs)
import Heap

-- | State is consists of triple
-- - The muximum address of the heap,
-- - the heap,
-- - the rules.
type State = (Addr, Heap, [Rule]) 

-- | Environment for the links, which is consists of the tuple :
-- - A map from the local links in the rule to the local links in a matching process
-- - A map from the free links in the rule to the free links in a matching process.
-- - 
type Envs = (M.Map Addr Addr, M.Map String Addr)

nullEnvs :: Envs
nullEnvs = (M.empty, M.empty)

type ThrowsRuntimeError = Either RuntimeError
instance Show RuntimeError where show = showRTError

data RuntimeError = DanglingPointer String


showRTError (DanglingPointer err) = err

matchProcVal :: ProcVal -> Envs -> Heap -> Maybe (Envs, Heap)
matchProcVal (LocalAliasVal indeg addr pointerVal) envs heap
  = Just (envs, heap)
matchProcVal (FreeAliasVal pointerName pointerVal) envs heap
  = Just (envs, heap)

matchRule :: Rule -> Heap -> Maybe (Envs, Heap)
matchRule (Rule lhs rhs freeTailLinks rhsRules) heap
  = Just (nullEnvs, heap)
