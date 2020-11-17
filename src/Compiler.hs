module Compiler where
import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Data.Bifunctor 
import qualified Parser (
  readExpr,
  ParseError
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
                  | FreePointersOnTopLevel (S.Set String)
                  | ParseError Parser.ParseError

--
showSet :: S.Set String -> String
showSet stringSet = "{" ++ intercalate ", " (S.toList stringSet) ++ "}"

showProcVals :: [ProcVal] -> String
showProcVals = intercalate ", " . map showProcVal

showProcVal :: ProcVal -> String
showProcVal (LocalAliasVal indeg addr pointerVal)
  = show indeg ++ " &" ++ show addr ++ " -> " ++ showPointerVal pointerVal
showProcVal (FreeAliasVal pointerName pointerVal)
  = pointerName ++ " -> " ++ showPointerVal pointerVal
showProcVal (RuleVal lhs rhs freeTailPointers)
  = "(" ++ showProcVals lhs ++ " :- " ++ showProcVals rhs ++ ") with freeTailPointer(s) "
    ++ showSet freeTailPointers

showPointerVal :: PointerVal -> String
showPointerVal (FreePointerVal pointerName) = pointerName
showPointerVal (LocalPointerVal addr) = "&" ++ show addr
showPointerVal (AtomVal atomName pointers)
  = if null pointers then atomName
    else atomName ++ "(" ++ intercalate ", " (map showPointerVal pointers) ++ ")"

-- show errors
showError :: CompileError -> String
showError (IsNotSerial name )
  = "pointer '" ++ name ++ "' is not serial.\n"
showError (IsNotFunctional name)
  = "pointer '" ++ name ++ "' is not functional.\n"
showError (RuleOnLHS rule)
  = "Rule on LHS in " ++ showProc rule
showError (NewFreePointersOnRHS pointers rule)
  = "New free pointer(s) " ++ showSet pointers
    ++ " appeard on RHS of " ++ showProc rule
showError (NotRedirectedPointers pointers rule)
  = "Not redirected free tail pointer(s) " ++ showSet pointers
    ++ " appeard on RHS of " ++ showProc rule
showError (FreePointersOnTopLevel pointers)
  = "Free pointer(s) " ++ showSet pointers
    ++ " appeard on the top level process"
showError (ParseError parseError)
  = "Parse error at " ++ show parseError

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue _ = error "cannot extract value"


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

nullEnvs :: Envs
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
  do (a', c) <- f a b
     (a'', cs) <- mapAccumLM f a' bs
     return (a'', c:cs)
mapAccumLM _ a [] = return (a, [])

isRuleVal :: ProcVal -> Bool
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
          let (envs', pointingToVal)
                = compilePointingToLit 
                  (updateFreeTailEnv (S.insert pointerName) envs)
                  pointingTo
          in
            return $ (envs', [FreeAliasVal pointerName pointingToVal])
      Just (_, True) -> throwError $ IsNotFunctional pointerName
      Just (addr, False) ->
        let
          envs' = updateLocalEnv (updateAssocList (second $ const True) pointerName) envs
          (envs'', pointingToVal) = compilePointingToLit envs' pointingTo
        in
          return (envs'', [LocalAliasVal 0 addr pointingToVal])
compileProcLit envs (AliasLit Nothing pointingTo) 
  = let addr = addrSeed envs
        envs' =
          incrAddrSeed
          $ updateLocalMapAddrIndeg (M.insert addr 0) envs
        (envs'', pointingToVal) = compilePointingToLit envs' pointingTo in
      return $ (envs'', [LocalAliasVal 0 addr pointingToVal])

compileProcLit envs (RuleLit lhs rhs) 
  = do (lhsEnvs, lhsProcs) <- compileProcLits nullEnvs lhs
       if any isRuleVal lhsProcs
         then throwError $ RuleOnLHS $ RuleLit lhs rhs
         else
         do (rhsEnvs, rhsProcs) <- compileProcLits nullEnvs rhs
            let lhsProcs'                = setIndegs lhsEnvs lhsProcs
                rhsProcs'                = setIndegs rhsEnvs rhsProcs
                freeTailPointersOnLHS    = freeTailEnv lhsEnvs
                freeTailPointersOnRHS    = freeTailEnv rhsEnvs
                freeHeadPointersOnLHS    = freeHeadEnv lhsEnvs S.\\ freeTailPointersOnLHS
                freeHeadPointersOnRHS    = freeHeadEnv rhsEnvs S.\\ freeTailPointersOnRHS
                newTailFreePointersOnRHS = freeTailPointersOnRHS S.\\ freeTailPointersOnLHS
                newHeadFreePointersOnRHS = freeHeadPointersOnRHS S.\\ freeHeadPointersOnLHS
                newFreePointersOnRHS     = S.union newTailFreePointersOnRHS newHeadFreePointersOnRHS
                notRedirectedPointers    = freeTailPointersOnLHS S.\\ freeTailPointersOnRHS
              in
              if not $ S.null newFreePointersOnRHS
              then throwError $ NewFreePointersOnRHS newFreePointersOnRHS $ RuleLit lhs rhs
              else if not $ S.null notRedirectedPointers
                then throwError $ NotRedirectedPointers notRedirectedPointers $ RuleLit lhs rhs
                else return $ (envs, [RuleVal lhsProcs' rhsProcs' freeTailPointersOnLHS])
  
compileProcLit envs (CreationLit pointerName procs)
  = let addr = addrSeed envs
        envs' =
          incrAddrSeed
          $ updateLocalMapAddrIndeg (M.insert addr 0)
          $ updateLocalEnv ((pointerName, (addr, False)) :) envs
    in
      do (envs'', procVals) <- compileProcLits envs' procs
         let (_, hasHead) = snd $ head $ localEnv envs'' in
           if not hasHead && localMapAddrIndeg envs'' M.! addr /= 0  
           then throwError $ IsNotSerial pointerName
           else return (updateLocalEnv (drop 1) envs'', procVals)

setIndeg :: Envs -> ProcVal -> ProcVal
setIndeg envs (LocalAliasVal _ addr pointingTo)
  = LocalAliasVal (localMapAddrIndeg envs M.! addr) addr pointingTo
setIndeg _ procVals = procVals

setIndegs :: Envs -> [ProcVal] -> [ProcVal]
setIndegs envs procVals
  = map (setIndeg envs) procVals
    
compileProcLits :: Envs -> [ProcLit] -> ThrowsError (Envs, [ProcVal])
compileProcLits envs 
  = liftM (second concat) . mapAccumLM compileProcLit envs

{--|
compileProcs :: [ProcLit] -> ThrowsError (Addr, [ProcVal])
compileProcs procLits
  = do (envs, procVals) <- compileProcLits nullEnvs procLits
       let procVals' = setIndegs envs procVals in
         return $ (addrSeed envs, procVals')
|--}

compile :: String -> ThrowsError [ProcVal]
compile input
  = case Parser.readExpr input of
      Left err -> throwError $ ParseError err
      Right procLits -> 
        do (envs, procVals) <- compileProcLits nullEnvs procLits
           let freePointers = S.union (freeTailEnv envs) (freeHeadEnv envs)
               procVals' = setIndegs envs procVals in
             if not $ S.null freePointers
             then throwError $ FreePointersOnTopLevel freePointers 
             else return procVals'


