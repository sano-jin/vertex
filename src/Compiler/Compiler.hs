{-# LANGUAGE Safe #-}
module Compiler.Compiler (
  Addr,
  Indeg,
  AtomName,
  LinkVal (..),
  ProcVal (..),
  Procs,
  Rule (..),
  showProcs,
  showProcVals,
  showRules,
  compile,
  ThrowsCompileError,
  CompileError (IsNotSerialAfterNormalization),
  Envs,
  updateLocalMapAddrIndeg
  ) where
import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import safe Data.Tuple.Extra
import qualified Compiler.Parser as Parser (
  readExpr,
  ParseError
  ) 
import Compiler.Syntax (
  showProc,
  LinkLit (..),
  ProcLit (..)
  )
import Util.Util (
  monadicMapAccumL
  )


type Addr = Int
type Indeg = Int
type AtomName = String

data LinkVal = FreeLinkVal String
               -- ^ X
             | LocalLinkVal Addr
               -- ^ X
             | AtomVal String [LinkVal]
             -- ^ p(X1,...,Xm)
             deriving(Eq)

data ProcVal = LocalAliasVal Indeg Addr LinkVal
               -- ^ \X.X -> p(X1,...,Xm)
             | FreeAliasVal AtomName LinkVal
               -- ^ X -> p(X1,...,Xm)
             deriving(Eq)

-- | some functions for showing processes
showSet :: S.Set String -> String
showSet stringSet = "{" ++ intercalate ", " (S.toList stringSet) ++ "}"

showProcVals :: [ProcVal] -> String
showProcVals = intercalate ", " . map showProcVal

showProcVal :: ProcVal -> String
showProcVal (LocalAliasVal indeg addr linkVal)
  = show indeg ++ " &" ++ show addr ++ " -> " ++ showLinkVal linkVal
showProcVal (FreeAliasVal linkName linkVal)
  = linkName ++ " -> " ++ showLinkVal linkVal

showLinkVal :: LinkVal -> String
showLinkVal (FreeLinkVal linkName) = linkName
showLinkVal (LocalLinkVal addr) = "&" ++ show addr
showLinkVal (AtomVal atomName links)
  = if null links then atomName
    else atomName ++ "(" ++ intercalate ", " (map showLinkVal links) ++ ")"


data Rule = Rule [ProcVal] [ProcVal] (S.Set String) [Rule]
-- ^ A rule is specified with ...
-- - left-hand-side atoms to match,
-- - right-hand-side atoms to generate,
-- - a set of names of free head links
-- - right-hand-side rules to generate.

showRule :: Rule -> String
showRule (Rule lhs rhs freeTailLinks rules)
  = "(" ++ showProcVals lhs ++ " :- " ++ showProcVals rhs
    ++ " ./*" ++ showSet freeTailLinks ++ "*/. "
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

type ThrowsCompileError = Either CompileError
-- ^ a type for handling results and errors

-- type IOThrowsError = ExceptT CompileError IO
instance Show CompileError where show = showCompileError

-- | type for denoting compile errors
data CompileError = IsNotSerial String
                  | IsNotFunctional String
                  | RuleOnLHS ProcLit
                  | NewFreeLinksOnRHS (S.Set String) ProcLit
                  | NotRedirectedLinks (S.Set String) ProcLit
                  | FreeLinksOnTopLevel (S.Set String)
                  | ParseError Parser.ParseError
                  | IsNotSerialAfterNormalization [(S.Set Addr, ProcVal)]

-- | functions for showing errors
showCompileError :: CompileError -> String
showCompileError (IsNotSerial name )
  = "link '" ++ name ++ "' is not serial"
showCompileError (IsNotFunctional name)
  = "link '" ++ name ++ "' is not functional"
showCompileError (RuleOnLHS rule)
  = "Rule on LHS in " ++ showProc rule
showCompileError (NewFreeLinksOnRHS links rule)
  = "New free link(s) " ++ showSet links
    ++ " appeard on RHS of " ++ showProc rule
showCompileError (NotRedirectedLinks links rule)
  = "Not redirected free tail link(s) " ++ showSet links
    ++ " appeard on RHS of " ++ showProc rule
showCompileError (FreeLinksOnTopLevel links)
  = "Free link(s) " ++ showSet links
    ++ " appeard on the top level process"
showCompileError (ParseError parseError)
  = "Parse error at " ++ show parseError
showCompileError (IsNotSerialAfterNormalization errors)
  = intercalate "\n" $ map showIsNotSerialAfterNormalizationError errors
    where showIsNotSerialAfterNormalizationError (addrs, procVal)
            = "local link(s) '" ++ showSet (S.map show addrs)
              ++ "' in '" ++ showProcVal procVal ++ "' is not serial"

type HasHead = Bool
type EnvList = [(String, (Addr, HasHead))]
-- ^ A type for the environment of local links

type EnvSet  = S.Set String
-- ^ A type for the environment of free links

-- | A helper function for updating a list of tuples
updateAssocList :: Eq key => (value -> value) -> key -> [(key, value)] -> [(key, value)] 
updateAssocList f key ((h@(k, v)):t)
  = if key == k then (k, f v) : t
    else h : updateAssocList f key t
updateAssocList _ _ [] = []

-- | A type for the Envirnment of pointes
data Envs = Envs { localEnv :: EnvList
                   -- ^ A mapping (list of tuple) from the local link names
                   -- to the tuples of the given address
                   -- and the boolean denotes whether its head appeared or not
                 , localMapAddrIndeg :: M.Map Addr Indeg 
                   -- ^ A map from the local link addresses to their indegrees
                 , freeTailEnv :: EnvSet
                   -- ^ A set of free tail link names
                 , freeHeadEnv :: EnvSet
                   -- ^ A set of free head link names
                 , addrSeed :: Int
                   -- ^ the number of the local links appeared in the process
                 } deriving (Show)

-- | Some helper functions for the link environment
updateLocalEnv :: (EnvList -> EnvList) -> Envs -> Envs
updateLocalEnv f envs
  = envs { localEnv = f $ localEnv envs }

updateLocalMapAddrIndeg :: (M.Map Addr Indeg -> M.Map Addr Indeg) -> Envs -> Envs
updateLocalMapAddrIndeg f envs
  = envs { localMapAddrIndeg = f $ localMapAddrIndeg envs }

updateFreeTailEnv :: (EnvSet -> EnvSet) -> Envs -> Envs
updateFreeTailEnv f envs
  = envs { freeTailEnv = f $ freeTailEnv envs }

{--|
updateFreeHeadEnv :: (EnvSet -> EnvSet) -> Envs -> Envs
updateFreeHeadEnv f envs
  = envs { freeHeadEnv = f $ freeHeadEnv envs }
|--}

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

-- | Check if the links are the local link or not.
-- If it is a local link, then increse its indegree in the environment
compilePointingToLit :: Envs -> LinkLit -> (Envs, LinkVal)
compilePointingToLit envs (LinkLit linkName) 
  = case lookup linkName $ localEnv envs of
      Nothing -> (envs, FreeLinkVal linkName)
      Just (addr, _) -> (incrLocalIndeg addr envs, LocalLinkVal addr)
compilePointingToLit envs (AtomLit atomName links) 
  = second (AtomVal atomName) $ mapAccumL compilePointingToLit envs links 

-- | Check if the incoming link is the local link or not.
-- Also, check the "functional condition",
-- which specifies that the head of the same link should not have appeared in the process
-- Notice the indegree of the local link are not set correctly for this time.
-- Here, initially, we just set it to be 0.
-- It will be correctly set after checking all the process appears on left/right hand-side of the rules or at the top-level process.
compileProcLit :: Envs -> ProcLit -> ThrowsCompileError (Envs, Procs)
compileProcLit envs (AliasLit (Just linkName) pointingTo) 
  = case lookup linkName $ localEnv envs of
      Nothing ->
        if S.member linkName $ freeTailEnv envs
        then throwError $ IsNotFunctional linkName
        else
          let (envs', pointingToVal)
                = compilePointingToLit 
                  (updateFreeTailEnv (S.insert linkName) envs)
                  pointingTo
          in
            return $ (envs', ([FreeAliasVal linkName pointingToVal], []))
      Just (_, True) -> throwError $ IsNotFunctional linkName
      Just (addr, False) ->
        let
          envs' = updateLocalEnv (updateAssocList (second $ const True) linkName) envs
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
--   - otherwise thrors the "NewFreeLinksOnRHS" error.
-- - For any free tail link `X` in P,
--   there must be a free tail link `X` that has the same name in `Q`,
--   - otherwise thrors the "NotRedirectedLinks" error.
-- Also, this sets the indeg of all the local links appears in the processes on the left/right hand-sides
compileProcLit envs (RuleLit lhs rhs) 
  = do (lhsEnvs, (lhsProcs, lhsRules)) <- compileProcLits nullEnvs lhs
       if not $ null lhsRules
         then throwError $ RuleOnLHS $ RuleLit lhs rhs
         else
         do (rhsEnvs, (rhsProcs, rhsRules)) <- compileProcLits nullEnvs rhs
            let lhsProcs'                = setIndegs lhsEnvs lhsProcs
                rhsProcs'                = setIndegs rhsEnvs rhsProcs
                freeTailLinksOnLHS    = freeTailEnv lhsEnvs
                freeTailLinksOnRHS    = freeTailEnv rhsEnvs
                freeHeadLinksOnLHS    = freeHeadEnv lhsEnvs S.\\ freeTailLinksOnLHS
                freeHeadLinksOnRHS    = freeHeadEnv rhsEnvs S.\\ freeTailLinksOnRHS
                newTailFreeLinksOnRHS = freeTailLinksOnRHS S.\\ freeTailLinksOnLHS
                newHeadFreeLinksOnRHS = freeHeadLinksOnRHS S.\\ freeHeadLinksOnLHS
                newFreeLinksOnRHS     = S.union newTailFreeLinksOnRHS newHeadFreeLinksOnRHS
                notRedirectedLinks    = freeTailLinksOnLHS S.\\ freeTailLinksOnRHS
              in
              if not $ S.null newFreeLinksOnRHS
              then throwError $ NewFreeLinksOnRHS newFreeLinksOnRHS $ RuleLit lhs rhs
              else if not $ S.null notRedirectedLinks
                then throwError $ NotRedirectedLinks notRedirectedLinks $ RuleLit lhs rhs
                else return $ (envs, ([], [Rule lhsProcs' rhsProcs' freeTailLinksOnLHS rhsRules]))

-- | handles the link creation
-- firstly add the new local link to the environment,
-- check the child processes,
-- then check whether the _head_ of the link appears or not if the indeg is bigger than zero.
-- If there is no _head_, throws the "IsNotSerial" error.
-- Drops the newly created link from the environment before returning the result
compileProcLit envs (CreationLit linkName procs)
  = let addr = addrSeed envs
        envs' =
          incrAddrSeed
          $ updateLocalMapAddrIndeg (M.insert addr 0)
          $ updateLocalEnv ((linkName, (addr, False)) :) envs
    in
      do (_envs, procsAndRules) <- compileProcLits envs' procs
         let (_, hasHead) = snd $ head $ localEnv _envs in
           if not hasHead && localMapAddrIndeg _envs M.! addr /= 0  
           then throwError $ IsNotSerial linkName
           else return (updateLocalEnv (drop 1) _envs, procsAndRules)

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
compileProcLits :: Envs -> [ProcLit] -> ThrowsCompileError (Envs, Procs)
compileProcLits envs 
  = liftM (second zipConcatBoth) . monadicMapAccumL compileProcLit envs

-- | check the top-level process
compile :: String -> ThrowsCompileError Procs
compile input
  = case Parser.readExpr input of
      Left err -> throwError $ ParseError err
      Right procLits -> 
        do (envs, (procVals, rules)) <- compileProcLits nullEnvs procLits
           let -- freeLinks = S.union (freeTailEnv envs) (freeHeadEnv envs)
               procVals' = setIndegs envs procVals in
             -- if not $ S.null freeLinks
             -- then throwError $ FreeLinksOnTopLevel freeLinks 
             -- else
             return (procVals', rules)

