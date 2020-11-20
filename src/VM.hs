module VM where
import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Data.Bifunctor 
import Compiler 


data Node = NAtom String [Addr]                       -- NAtom SymbolAtomName [Pointers]
          | NInd Addr                                 -- Alias to Addr

-- | Heap is consists of the tuple
-- - The muximum address of the heap
-- - and the map from addresses to the tuples of the indegree and the node
type Heap = (Addr, M.Map Addr (Indeg, Node)) 

-- | Environment for the links, which is consists of the tuple :
-- - A map from the local links in the rule to the local links in a matching process
-- - A map from the free links in the rule to the free links in a matching process.
type Envs = (M.Map Addr Addr, M.Map String Addr)


type ThrowsRTError = Either RuntimeError
instance Show RuntimeError where show = showRTError

data RuntimeError = DanglingPointer String

showRTError (DanglingPointer err) = err


