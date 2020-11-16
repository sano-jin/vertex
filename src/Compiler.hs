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
  ParseError,
  SourcePos
  )
import Syntax

type Addr = Int
type Indeg = Int
data Node = NAtom String [Addr]                       -- NAtom SymbolAtomName [Pointers]
          | NInd Addr                                 -- Alias to Addr
          | NRule [ProcVal] [ProcVal] (S.Set String)  -- Rule

type Heap = (Addr, [(Addr, (Indeg, Node))])  -- [(Address, Indegree, Atom)]


data PointerVal = FreePointerVal String
                | LocalPointerVal Addr           -- X
                | AtomVal String [PointerVal]    -- p(X1,...,Xm)
                deriving(Eq, Ord, Show)


data ProcVal = LocalAliasVal Indeg Addr PointerVal          -- \X.X -> p(X1,...,Xm)
             | FreeAliasVal String PointerVal               -- X -> p(X1,...,Xm)
             | RuleVal [ProcVal] [ProcVal] (S.Set String)   -- P :- P
             deriving(Eq, Ord, Show)

type RuleSet = [(ProcVal, ProcVal)]


type ThrowsError = Either CompileError
type IOThrowsError = ExceptT CompileError IO
instance Show CompileError where show = showError

data CompileError = IsNotSerial String
                  | IsNotFunctional String
                  | RuleOnLHS ProcLit
                  | NewFreePointersOnRHS (S.Set String) ProcLit
                  | NotRedirectedPointers (S.Set String) ProcLit
                  | Parser Parser.ParseError

--
showSet :: S.Set String -> String
showSet stringSet = "{" ++ intercalate ", " (S.toList stringSet) ++ "}"

showProcVals :: [ProcVal] -> String
showProcVals = intercalate ", " . map showProcVal

showProcVal :: ProcVal -> String
showProcVal (LocalAliasVal indeg addr pointerVal)
  = "indeg : " ++ show indeg ++ " & #" ++ show addr ++ " -> " ++ showPointerVal pointerVal
showProcVal (FreeAliasVal pointerName pointerVal)
  = pointerName ++ " -> " ++ showPointerVal pointerVal
showProcVal (RuleVal lhs rhs freeTailPointers)
  = "(" ++ showProcVals lhs ++ " :- " ++ showProcVals rhs ++ ") with freeTailPointers "
    ++ showSet freeTailPointers

showPointerVal :: PointerVal -> String
showPointerVal (FreePointerVal pointerName) = pointerName
showPointerVal (LocalPointerVal addr) = "#" ++ show addr
showPointerVal (AtomVal atomName pointers)
  = atomName ++ "(" ++ intercalate ", " (map showPointerVal pointers) ++ ")"

-- show errors
showError :: CompileError -> String
showError (IsNotSerial name )
  = "pointer '" ++ name ++ "' is not serial.\n"
showError (IsNotFunctional name)
  = "pointer '" ++ name ++ "' is not functional.\n"
showError (RuleOnLHS rule)
  = "Rule on LHS in " ++ show rule
showError (NewFreePointersOnRHS pointers rule)
  = "New free pointers " ++ showSet pointers
    ++ " appeard on RHS of " ++ show rule
showError (NotRedirectedPointers pointers rule)
  = "Not redirected free tail pointers " ++ showSet pointers
    ++ " appeard on RHS of " ++ show rule
showError (Parser parseError)
  = "Parse error at " ++ show parseError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val


-- PointerName -> (Indegree, hasHead)
type HasHead = Bool
type EnvList = [(String, (Addr, HasHead))]
type EnvSet  = S.Set String

lookupAssocListWithErr :: Eq key => key -> [(key, value)] -> value
lookupAssocListWithErr key ((k, v):t)
  = if key == k then v
    else lookupAssocListWithErr key t
lookupAssocListWithErr _ [] = error "empty list"

updateAssocList :: Eq key => (value -> value) -> key -> [(key, value)] -> [(key, value)] 
updateAssocList f key ((h@(k, v)):t)
  = if key == k then (k, f v) : t
    else h : updateAssocList f key t
updateAssocList _ _ [] = []

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
                , localMapAddrIndeg = M.empty
                , freeTailEnv = S.empty
                , freeHeadEnv = S.empty
                , addrSeed = 0
                }

incrAddrSeed :: Envs -> Envs
incrAddrSeed = updateAddrSeed (+ 1)

incrLocalIndeg :: Addr -> Envs -> Envs
incrLocalIndeg addr envs
  = updateLocalMapAddrIndeg (M.adjust (+ 1) addr) envs

mapAccumLM :: Monad m => (a -> b -> m (a, c)) -> a -> [b] -> m (a, [c])
mapAccumLM f a (b:bs) =
  do (a, c) <- f a b
     (a, cs) <- mapAccumLM f a bs
     return (a, c:cs)
  
isRuleVal (RuleVal _ _ _) = True
isRuleVal _               = False

compilePointingToLit :: Envs -> PointerLit -> (Envs, PointerVal)
compilePointingToLit envs (PointerLit pointerName) 
  = case lookup pointerName $ localEnv envs of
      Nothing -> (envs, FreePointerVal pointerName)
      Just (addr, _) -> (incrLocalIndeg addr envs, LocalPointerVal addr)
compilePointingToLit envs (AtomLit atomName pointers) 
  = second (AtomVal atomName) $ mapAccumL compilePointingToLit envs pointers 
 
compileProcLit :: Envs -> ProcLit -> ThrowsError (Envs, [ProcVal])
compileProcLit envs (AliasLit (Just pointerName) pointingTo) 
  = case lookup pointerName $ localEnv envs of
      Nothing ->
        if S.member pointerName $ freeTailEnv envs
        then throwError $ IsNotFunctional pointerName
        else
          let (envs, pointingToVal)
                = compilePointingToLit 
                  (updateFreeTailEnv (S.insert pointerName) envs)
                  pointingTo
          in
            return $ (envs, [FreeAliasVal pointerName pointingToVal])
      Just (_, True) -> throwError $ IsNotFunctional pointerName
      Just (addr, False) ->
        let (envs, pointingToVal) = compilePointingToLit envs pointingTo in
          return (envs, [LocalAliasVal 0 addr pointingToVal])
compileProcLit envs (AliasLit Nothing pointingTo) 
  = let (envs, pointingToVal) = compilePointingToLit envs pointingTo in
      return $ (incrAddrSeed envs, [LocalAliasVal 0 (addrSeed envs) pointingToVal])

compileProcLit envs (RuleLit lhs rhs) 
  = do (lhsEnvs, lhsProcs) <- compileProcLits nullEnvs lhs
       if any isRuleVal lhsProcs
         then throwError $ RuleOnLHS $ RuleLit lhs rhs
         else
         do (rhsEnvs, rhsProcs) <- compileProcLits nullEnvs rhs
            let freeTailPointersOnLHS    = freeTailEnv lhsEnvs
                freeTailPointersOnRHS    = freeTailEnv rhsEnvs
                freeHeadPointersOnLHS    = freeHeadEnv lhsEnvs S.\\ freeTailPointersOnLHS
                freeHeadPointersOnRHS    = freeHeadEnv rhsEnvs S.\\ freeTailPointersOnRHS
                newTailFreePointersOnRHS = freeTailPointersOnLHS S.\\ freeTailPointersOnRHS
                newHeadFreePointersOnRHS = freeHeadPointersOnLHS S.\\ freeHeadPointersOnRHS
                newFreePointersOnRHS     = S.union newTailFreePointersOnRHS newHeadFreePointersOnRHS
                notRedirectedPointers    = freeTailPointersOnRHS S.\\ freeTailPointersOnLHS
              in
              if S.null newFreePointersOnRHS
              then throwError $ NewFreePointersOnRHS newFreePointersOnRHS $ RuleLit lhs rhs
              else if S.null notRedirectedPointers
                then throwError $ NotRedirectedPointers notRedirectedPointers $ RuleLit lhs rhs
                else return $ (envs, [RuleVal lhsProcs rhsProcs freeTailPointersOnLHS])
  
compileProcLit envs (CreationLit pointerName procs)
  = let addr = addrSeed envs
        envs =
          incrAddrSeed
          $ updateLocalMapAddrIndeg (M.insert addr 0)
          $ updateLocalEnv ((pointerName, (addr, False)) :) envs
    in
      do (envs, procVals) <- compileProcLits envs procs
         let (_, hasHead) = snd $ head $ localEnv envs in
           if not hasHead && localMapAddrIndeg envs M.! addr /= 0  
           then throwError $ IsNotSerial pointerName
           else return (updateLocalEnv (drop 1) envs, procVals)

setIndeg :: Envs -> ProcVal -> ProcVal
setIndeg envs (LocalAliasVal _ addr pointingTo)
  = LocalAliasVal (localMapAddrIndeg envs M.! addr) addr pointingTo
setIndeg envs (RuleVal lhs rhs freeTailPointers)
  = RuleVal (setIndegs envs lhs) (setIndegs envs rhs) freeTailPointers
setIndeg envs procVals = procVals

setIndegs :: Envs -> [ProcVal] -> [ProcVal]
setIndegs envs procVals
  = map (setIndeg envs) procVals
    
compileProcLits :: Envs -> [ProcLit] -> ThrowsError (Envs, [ProcVal])
compileProcLits envs 
  = liftM (second concat) . mapAccumLM compileProcLit envs

compileProcs :: Envs -> [ProcLit] -> ThrowsError (Addr, [ProcVal])
compileProcs envs procLits
  = do (envs, procVals) <- compileProcLits envs procLits
       let procVals = setIndegs envs procVals in
         return $ (addrSeed envs, procVals)


