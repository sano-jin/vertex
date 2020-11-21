module VM where
import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Data.Bifunctor 
import Compiler hiding (Envs)
import Heap

-- | State is consists of triple
-- - The muximum address of the addresses in the heap,
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

matchProcVal :: Envs -> Heap -> ProcVal -> Maybe (Envs, Heap)
matchProcVal envs heap (LocalAliasVal indeg addr (AtomVal atomName links)) 
  = Just (envs, heap)
matchProcVal _ _ (LocalAliasVal _ _ _)
  = error "not normalized"
matchProcVal envs heap (FreeAliasVal pointerName pointerVal) 
  = Just (envs, heap)

matchRule :: Rule -> Heap -> Maybe (Envs, Heap)
matchRule (Rule lhs rhs freeTailLinks rhsRules) heap
  = Just (nullEnvs, heap)

