module Normalize where
import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Data.Tuple.Extra
import Syntax
import Compiler

data Pointer = FreeLink Addr          -- X
             | LocalLink Addr           -- X
             | Atom String [Pointer]    -- p(X1,...,Xm)
             deriving(Eq, Ord, Show)

data Proc = LocalAlias Indeg Addr Pointer          -- \X.X -> p(X1,...,Xm)
           | FreeAlias Addr Pointer               -- X -> p(X1,...,Xm)
           deriving(Eq, Ord, Show)

incrNLocalIndeg :: Addr -> Envs -> Int -> Envs
incrNLocalIndeg addr envs n 
  = updateLocalMapAddrIndeg (M.adjust (+ n) addr) envs


normalizeAlias :: Envs -> ProcVal -> (Envs, Maybe Proc)
normalizeAlias envs (LocalAliasVal indeg addr (LocalLinkVal pointingAddr))
  = (envs, Nothing)



