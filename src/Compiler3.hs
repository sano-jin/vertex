module Compiler where
import System.Environment
import Data.IORef
import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Data.Either
import Data.Bifunctor 
import qualified Parser (
  readExpr,
  showBlock,
  ParseError,
  SourcePos
  )
import Syntax

type Addr = Int
type Indeg = Int
data Node = NAtom String [Addr]         -- NAtom SymbolAtomName [Pointers]
          | NInd Addr                   -- Alias to Addr
          | NRule [ProcVal] [ProcVal]   -- Rule

type Heap = [(Addr, Indeg, Node)]  -- [(Address, Indegree, Atom)]


data PointerVal = FreePointerVal String
                | LocalPointerVal Addr     -- X
                | AtomVal String [PointerLit]    -- p(X1,...,Xm)
                deriving(Eq, Ord, Show)


data ProcVal = LocalAliasVal Indeg Addr PointerVal   -- \X.X -> p(X1,...,Xm)
             | FreeAliasVal String PointerVal        -- X -> p(X1,...,Xm)
             | RuleVal [ProcVal] [ProcVal]           -- P :- P
             deriving(Eq, Ord, Show)

type RuleSet = [(ProcVal, ProcVal)]


type ThrowsError = Either CompileError
type IOThrowsError = ExceptT CompileError IO
instance Show CompileError where show = showError

data CompileError = IsNotSerial String
                  | IsNotFunctional String
                  | RuleOnLHS ProcLit
                  | Parser Parser.ParseError

-- show
showError :: CompileError -> String
showError (IsNotSerial name )
  = "pointer '" ++ name ++ "' is not serial.\n"
showError (IsNotFunctional name)
  = "pointer '" ++ name ++ "' is not functional.\n"
showError (RuleOnLHS rule)
  = "Rule on LHS in " ++ show rule
showError (Parser parseError)
  = "Parse error at " ++ show parseError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val


-- PointerName -> (Indegree, hasHead)
type HasHead = Bool
type IsLocal = Bool
type EnvList = [(String, (Addr, Indeg, HasHead))]
type EnvSet  = S.Set String

lookupAssocListWithErr :: Eq key => key -> [(key, value)] -> value
lookupAssocListWithErr key ((k, v):t)
  = if key == k then v
    else lookupAssocListWithErr key t
lookupAssocListWithErr _ [] = error "empty list"

updateAssocList :: Eq key => (value -> value) -> key -> [(key, value)] -> value
updateAssocList f key (h@(k, v):t)
  = if key == k then (k, f v) : t
    else h @ updateAssocList f key t
updateAssocListWithErr _ _ [] = []


tup3 :: (a, b, c) -> c
tup3 (_, _, c) = c

-- type Envs = (EnvList, EnvSet, EnvSet)

data Envs = Envs { localEnv :: EnvList
                 , localMapAddrIndeg :: M.Map Addr Indeg
                 , freeTailEnv :: EnvSet
                 , freeHeadEnv :: EnvSet
                 , addrSeed :: Int
                 } deriving (Show)

updateLocalEnv :: (EnvList -> EnvList) -> Envs -> Envs
updateLocalEnv f envs
  = envs { localEnv = f $ localEnv envs }

updateLocalMapAddrIndeg :: (M.Map Addr Indeg -> M.Map Addr Indeg) -> Envs -> Envs
updateLocalMapAddrIndeg f envs
  = envs { localMapAddrIndeg = f $ localMapAddrIndeg envs }

updateFreeTailEnv :: (EnvSet -> EnvSet) -> Envs -> Envs
updateFreeTailEnv f envs
  = envs { freeTailEnv = f $ freeTailEnv envs }

updateFreeHeadEnv :: (EnvSet -> EnvSet) -> Envs -> Envs
updateFreeHeadEnv f envs
  = envs { freeHeadEnv = f $ freeHeadEnv envs }

updateAddrSeed :: (Int -> Int) -> Envs -> Envs
updateAddrSeed f envs
  = envs { addrSeed = f $ addrSeed envs }

nullEnvs = Envs { localEnv = []
                , freeTailEnv = S.empty
                , freeHeadEnv = S.empty
                , addrSeed = 0
                }

incrAddrSeed :: Envs -> Envs
incrAddrSeed = updateAddrSeed (+ 1)

incrLocalIndeg :: Addr -> Envs -> Envs
incrLocalIndeg addr
  = 

mapAccumLM :: Monad m => (a -> b -> m (a, c)) -> a -> [b] -> m (a, [c])
mapAccumLM f a (b:bs) =
  do (a, c) <- f a b
     (a, cs) <- mapAccumLM f a bs
     return (a, c:cs)
  
isRuleVal (RuleVal _ _) = True
isRuleVal _             = False

compilePointingToLit :: PointerLit -> Envs -> (Envs, PointerVal)
compilePointingToLit (PointerLit pointerName) envs
  = case lookup $ localEnv envs of
      Nothing -> (envs, FreePoitnerVal poitnerName)
      Just (addr, _, _) -> (envs, 
compilePointingToLit pointingTo envs
  = (envs, FreePointerVal "X")

compileProcLit :: Envs -> ProcLit -> ThrowsError (Envs, [ProcVal])
compileProcLit envs (AliasLit (Just pointerName) pointingTo) 
  = case lookup pointerName $ localEnv envs of
      Nothing ->
        if S.member pointerName $ freeTailEnv envs
        then throwError $ IsNotFunctional pointerName
        else
          let (envs, pointingToVal)
                = compilePointingToLit pointingTo
                  $ updateFreeTailEnv (S.insert pointerName) envs
          in
            return $ (envs, [FreeAliasVal pointerName pointingToVal])
      Just (_, _, True) -> throwError $ IsNotFunctional pointerName
      Just (addr, _, False) ->
        let (envs, pointingToVal) = compilePointingToLit pointingTo envs in
          return (envs, [LocalAliasVal 0 addr pointingToVal])
compileProcLit envs (AliasLit Nothing pointingTo) 
  = let (envs, pointingToVal) = compilePointingToLit pointingTo envs in
      return $ (incrAddrSeed envs, [LocalAliasVal 0 (addrSeed envs) pointingToVal])

compileProcLit envs (RuleLit lhs rhs) 
  = do (lhsEnv, lhsProcs) <- compileProcLits nullEnvs lhs
       if any isRuleVal lhsProcs
         then throwError $ RuleOnLHS (RuleLit lhs rhs)
         else do (rhsEnv, rhsProcs) <- compileProcLits nullEnvs rhs
                 return $ (envs, [RuleVal lhsProcs rhsProcs])
  
compileProcLit envs (CreationLit pointerName procs)
  = let addr = addrSeed envs
        envs =
          incrAddrSeed
          $ updateLocalMapAddrIndeg (M.insert addr 0)
          $ updateLocalEnv ((pointerName, (addr, 0, False)) :) envs
    in
      do (envs, procVals) <- compileProcLits envs procs
         if not $ tup3 $ lookupAssocListWithErr pointerName $ localEnv envs
           then throwError $ IsNotSerial pointerName
           else return (updateLocalEnv (drop 1) envs, procVals)


setIndeg :: Envs -> ProcVal -> ProcVal
setIndeg envs (LocalAliasVal _ addr pointingTo)
  = LocalAliasVal (localMapAddrIndeg envs M.! addr) addr pointingTo
setIndeg envs (RuleVal lhs rhs)
  = RuleVal (setIndegs envs lhs) (setIndegs envs rhs)
setIndeg envs procVals = procVals

setIndegs :: Envs -> [ProcVal] -> [ProcVal]
setIndegs envs procVals
  = map (setIndeg envs) procVals
  


  
compileProcLits :: Envs -> [ProcLit] -> ThrowsError (Envs, [ProcVal])
compileProcLits envs 
  = (second (second concat) . mapAccumLM compileProcLit envs)



