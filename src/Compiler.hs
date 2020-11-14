module Compiler where
import System.Environment
import Data.IORef
import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Parser (
  readExpr,
  showBlock,
  ParseError,
  SourcePos
  )
import Syntax

type Addr = Int
type Indeg = Int
data Node = NAtom String [Addr]  -- NAtom SymbolAtomName [Pointers]
          | NInd Addr            -- Alias to Addr

type Heap = [(Addr, Indeg, Node)]  -- [(Address, Indegree, Atom)]

data PointerVal = PointerVal Addr                   -- Addr
                | AtomVal String [PointerVal]       -- p(X1,...,Xm)
                deriving(Eq, Ord, Show)

data ProcVal = LocalAliasVal Int Addr PointerVal    -- \X.Addr -> p(X1,...,Xm)
             | FreeAliasVal Int Addr PointerVal     -- X.Addr -> p(X1,...,Xm)
             | RuleVal [ProcVal] [ProcVal]          -- P :- P
             deriving(Eq, Ord, Show)

type RuleSet = [(ProcVal, ProcVal)]

-- PointerName -> (Address, Indegree, hasHead, isLocal)
type HasHead = Bool
type IsLocal = Bool
type Env = [(String, (Addr, Indeg, HasHead, IsLocal))]

-- seed -> (newAddr, newSeed)
newAddr :: Addr -> (Addr, Addr)    
newAddr oldAddr = (oldAddr, oldAddr + 1)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

fst4 :: (a, b, c, d) -> a
fst4 (a, _, _, _) = a

lookupEnv_ :: String -> Env -> Maybe Addr
lookupEnv_ pointerName env
  = liftM fst4 $ lookup pointerName env

mapEnv ::
  String -> ((Addr, Indeg, HasHead, IsLocal) -> (Addr, Indeg, HasHead, IsLocal)) -> Env -> Maybe Env
mapEnv pointerName f (h@(key, attr):t)
  = if pointerName == key then return $ (key, f attr):t
    else liftM (h:) $ mapEnv pointerName f t
mapEnv _ _ [] = Nothing
  
-- procLit -> oldHeap -> oldEnv -> oldRuleSet -> oldAddrSeed
-- -> (newHeap, newEnv, newRuleSet, newAddr)
compileProcLit :: ProcLit -> Heap -> Env -> RuleSet -> Addr -> (Heap, Env, RuleSet, Addr)
compileProcLit (AliasLit (Just pointerName) pointingTo) oldHeap oldEnv oldRuleSet oldAddrSeed
  = case mapEnv
         (\(addr, indeg, hasHead, isLocal) -> (addr, indeg, True, isLocal))
         pointerName
         oldEnv
    of
      Nothing -> let newEnv      = oldEnv ++ [(oldAddrSeed, 0, True, False)]
                     newHeap     =
                       let compilePointingTo pointingTo oldHeap
                       (oldAddrSeed, 0, )
        in
                   (oldHeap, newEnv, oldRuleSet, newAddrSeed)
      Just addr -> (oldHeap, oldEnv, oldRuleSet, oldAddrSeed)
        
       
readExpr :: String -> String
readExpr input = case Parser.readExpr input of
    Left err -> "No match : " ++ show err
    Right val -> "Found : " ++ Parser.showBlock val

