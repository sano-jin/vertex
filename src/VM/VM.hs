
module VM.VM where
import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Data.Bifunctor 
import Compiler.Compiler hiding (Envs)
import VM.Heap 

type State = (Addr, Heap, [Rule]) 
-- ^ State is consists of triple
-- - The muximum address of the addresses in the heap,
-- - the heap,
-- - the rules.


type ThrowsRuntimeError = Either RuntimeError
instance Show RuntimeError where show = showRTError

data RuntimeError = DanglingPointer String

showRTError (DanglingPointer err) = err

matchRule :: Rule -> Heap -> Maybe (Envs, Heap)
matchRule (Rule lhs rhs freeTailLinks rhsRules) heap
  = Just (nullEnvs, heap)

