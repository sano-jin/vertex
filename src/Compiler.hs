module Compiler where
import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
-- import Data.Bifunctor 
import Data.Tuple.Extra hiding (both)
import qualified Parser (
  readExpr,
  ParseError
  )
import Syntax

type Addr = Int
type Indeg = Int

data PointerVal = FreePointerVal String          -- X
                | LocalPointerVal Addr           -- X
                | AtomVal String [PointerVal]    -- p(X1,...,Xm)
                deriving(Eq, Ord, Show)


data ProcVal = LocalAliasVal Indeg Addr PointerVal          -- \X.X -> p(X1,...,Xm)
             | FreeAliasVal String PointerVal               -- X -> p(X1,...,Xm)
             deriving(Eq, Ord, Show)

-- | some functions for showing processes
showSet :: S.Set String -> String
showSet stringSet = "{" ++ intercalate ", " (S.toList stringSet) ++ "}"

showProcVals :: [ProcVal] -> String
showProcVals = intercalate ", " . map showProcVal

showProcVal :: ProcVal -> String
showProcVal (LocalAliasVal indeg addr pointerVal)
  = show indeg ++ " &" ++ show addr ++ " -> " ++ showPointerVal pointerVal
showProcVal (FreeAliasVal pointerName pointerVal)
  = pointerName ++ " -> " ++ showPointerVal pointerVal

showPointerVal :: PointerVal -> String
showPointerVal (FreePointerVal pointerName) = pointerName
showPointerVal (LocalPointerVal addr) = "&" ++ show addr
showPointerVal (AtomVal atomName pointers)
  = if null pointers then atomName
    else atomName ++ "(" ++ intercalate ", " (map showPointerVal pointers) ++ ")"


-- | A rule is specified with ...
-- - left-hand-side atoms to match,
-- - right-hand-side atoms to generate,
-- - a set of names of free head pointers
-- - right-hand-side rules to generate.
data Rule = Rule [ProcVal] [ProcVal] (S.Set String) [Rule]

showRule :: Rule -> String
showRule (Rule lhs rhs freeTailPointers rules)
  = "(" ++ showProcVals lhs ++ " :- " ++ showProcVals rhs
    ++ " ./*" ++ showSet freeTailPointers ++ "*/. "
    ++ showRules rules
    ++ ")"

showRules :: [Rule] -> String
showRules = intercalate ", " . map showRule

-- | Processes are specified with atoms and rules
type Procs = ([ProcVal], [Rule])

showProcs :: Procs -> String
showProcs (procVals, rules)
  = let dot = if null procVals || null rules then "" else ". " in
      showProcVals procVals ++ dot ++ showRules rules

-- | a type for handling results and errors
type ThrowsError = Either CompileError
-- type IOThrowsError = ExceptT CompileError IO
instance Show CompileError where show = showError

-- | type for denoting compile errors
data CompileError = IsNotSerial String
                  | IsNotFunctional String
                  | RuleOnLHS ProcLit
                  | NewFreePointersOnRHS (S.Set String) ProcLit
                  | NotRedirectedPointers (S.Set String) ProcLit
                  | FreePointersOnTopLevel (S.Set String)
                  | ParseError Parser.ParseError

-- | functions for showing errors
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


type HasHead = Bool
-- | A type for the environment of local pointers
type EnvList = [(String, (Addr, HasHead))]
-- | A type for the environment of free pointers
type EnvSet  = S.Set String

-- | An non-defensive version of List.lookup 
lookupAssocListWithErr :: Eq key => key -> [(key, value)] -> value
lookupAssocListWithErr key ((k, v):t)
  = if key == k then v
    else lookupAssocListWithErr key t
lookupAssocListWithErr _ [] = error "empty list"

-- | A helper function for updating a list of tuples
updateAssocList :: Eq key => (value -> value) -> key -> [(key, value)] -> [(key, value)] 
updateAssocList f key ((h@(k, v)):t)
  = if key == k then (k, f v) : t
    else h : updateAssocList f key t
updateAssocList _ _ [] = []

-- | A type for the Envirnment of pointes
data Envs = Envs { localEnv :: EnvList
                 , localMapAddrIndeg :: M.Map Addr Indeg 
                 , freeTailEnv :: EnvSet
                 , freeHeadEnv :: EnvSet
                 , addrSeed :: Int
                 } deriving (Show)

-- | Some helper functions for the pointer environment
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
                , addrSeed = 0  -- | the number of the local pointers appeared in the process
                }

incrAddrSeed :: Envs -> Envs
incrAddrSeed = updateAddrSeed (+ 1)

incrLocalIndeg :: Addr -> Envs -> Envs
incrLocalIndeg addr envs
  = updateLocalMapAddrIndeg (M.adjust (+ 1) addr) envs

-- | A monadic version of List.mapAccumL
mapAccumLM :: Monad m => (a -> b -> m (a, c)) -> a -> [b] -> m (a, [c])
mapAccumLM f a (b:bs) =
  do (a', c) <- f a b
     (a'', cs) <- mapAccumLM f a' bs
     return (a'', c:cs)
mapAccumLM _ a [] = return (a, [])

-- | Check if the pointers are the local pointer or not.
-- If it is a local pointer, then increse its indegree in the environment
compilePointingToLit :: Envs -> PointerLit -> (Envs, PointerVal)
compilePointingToLit envs (PointerLit pointerName) 
  = case lookup pointerName $ localEnv envs of
      Nothing -> (envs, FreePointerVal pointerName)
      Just (addr, _) -> (incrLocalIndeg addr envs, LocalPointerVal addr)
compilePointingToLit envs (AtomLit atomName pointers) 
  = second (AtomVal atomName) $ mapAccumL compilePointingToLit envs pointers 

-- | Check if the incoming pointer is the local pointer or not.
-- Also, check the "functional condition",
-- which specifies that the head of the same link should not have appeared in the process
-- Notice the indegree of the local pointer are not set correctly for this time.
-- Here, initially, we just set it to be 0.
-- It will be correctly set after checking all the process appears on left/right hand-side of the rules or at the top-level process.
compileProcLit :: Envs -> ProcLit -> ThrowsError (Envs, Procs)
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
            return $ (envs', ([FreeAliasVal pointerName pointingToVal], []))
      Just (_, True) -> throwError $ IsNotFunctional pointerName
      Just (addr, False) ->
        let
          envs' = updateLocalEnv (updateAssocList (second $ const True) pointerName) envs
          (envs'', pointingToVal) = compilePointingToLit envs' pointingTo
        in
          return (envs'', ([LocalAliasVal 0 addr pointingToVal], []))
compileProcLit envs (AliasLit Nothing pointingTo) 
  = let addr = addrSeed envs
        envs' =
          incrAddrSeed
          $ updateLocalMapAddrIndeg (M.insert addr 0) envs
        (envs'', pointingToVal) = compilePointingToLit envs' pointingTo in
      return $ (envs'', ([LocalAliasVal 0 addr pointingToVal], []))

-- | A Rule `(P :- Q)` has several conditions.
-- - There should be no rules on `Q`,
--   - otherwise thrors the "RuleOnLHS" error.
-- - `fl(P)` must be a superset of `fl(Q)`,
--   - otherwise thrors the "NewFreePointersOnRHS" error.
-- - For any free tail link `X` in P,
--   there must be a free tail link `X` that has the same name in `Q`,
--   - otherwise thrors the "NotRedirectedPointers" error.
-- Also, this sets the indeg of all the local links appears in the processes on the left/right hand-sides
compileProcLit envs (RuleLit lhs rhs) 
  = do (lhsEnvs, (lhsProcs, lhsRules)) <- compileProcLits nullEnvs lhs
       if not $ null lhsRules
         then throwError $ RuleOnLHS $ RuleLit lhs rhs
         else
         do (rhsEnvs, (rhsProcs, rhsRules)) <- compileProcLits nullEnvs rhs
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
                else return $ (envs, ([], [Rule lhsProcs' rhsProcs' freeTailPointersOnLHS rhsRules]))

-- | handles the link creation
-- firstly add the new local link to the environment,
-- check the child processes,
-- then check whether the _head_ of the link appears or not if the indeg is bigger than zero.
-- If there is no _head_, throws the "IsNotSerial" error.
-- Drops the newly created link from the environment before returning the result
compileProcLit envs (CreationLit pointerName procs)
  = let addr = addrSeed envs
        envs' =
          incrAddrSeed
          $ updateLocalMapAddrIndeg (M.insert addr 0)
          $ updateLocalEnv ((pointerName, (addr, False)) :) envs
    in
      do (envs'', procsAndRules) <- compileProcLits envs' procs
         let (_, hasHead) = snd $ head $ localEnv envs'' in
           if not hasHead && localMapAddrIndeg envs'' M.! addr /= 0  
           then throwError $ IsNotSerial pointerName
           else return (updateLocalEnv (drop 1) envs'', procsAndRules)

-- | set indeg of the local links
setIndeg :: Envs -> ProcVal -> ProcVal
setIndeg envs (LocalAliasVal _ addr pointingTo)
  = LocalAliasVal (localMapAddrIndeg envs M.! addr) addr pointingTo
setIndeg _ procVals = procVals

setIndegs :: Envs -> [ProcVal] -> [ProcVal]
setIndegs envs procVals
  = map (setIndeg envs) procVals

zipConcatBoth :: [([a], [b])] -> ([a], [b])
zipConcatBoth = first concat . second concat . unzip

-- | check the processes
compileProcLits :: Envs -> [ProcLit] -> ThrowsError (Envs, Procs)
compileProcLits envs 
  = liftM (second zipConcatBoth) . mapAccumLM compileProcLit envs

-- | check the top-level process
compile :: String -> ThrowsError Procs
compile input
  = case Parser.readExpr input of
      Left err -> throwError $ ParseError err
      Right procLits -> 
        do (envs, (procVals, rules)) <- compileProcLits nullEnvs procLits
           let freePointers = S.union (freeTailEnv envs) (freeHeadEnv envs)
               procVals' = setIndegs envs procVals in
             if not $ S.null freePointers
             then throwError $ FreePointersOnTopLevel freePointers 
             else return (procVals', rules)


