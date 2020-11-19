module VM where
import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Data.Bifunctor 
import Compiler 

data Node = NAtom String [Addr]                       -- NAtom SymbolAtomName [Pointers]
          | NInd Addr                                 -- Alias to Addr
          | NRule [ProcVal] [ProcVal] (S.Set String)  -- Rule

type Heap = (Addr, [(Addr, (Indeg, Node))])  -- [(Address, Indegree, Atom)]

type ThrowsRTError = Either RuntimeError
instance Show RuntimeError where show = showRTError

data RuntimeError = DanglingPointer String

showRTError (DanglingPointer err) = err


