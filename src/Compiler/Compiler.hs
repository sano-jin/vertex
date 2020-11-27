{-# LANGUAGE Safe #-}

{-|
Module      : Compiler
Description : A compiler for the vertex language
Copyright   : (c) sano, 2020
License     : MIT
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Compiler.Compiler (
  compile,
  ThrowsCompileError,
  CompileError (IsNotSerialAfterNormalization),
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
import Compiler.Process
import Compiler.Envs
import Data.Bifunctor (bimap)

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
              ++ "' in '" ++ show procVal ++ "' is not serial"

-- | A helper function for updating a list of tuples
updateAssocList :: Eq key => (value -> value) -> key -> [(key, value)] -> [(key, value)] 
updateAssocList f key (h@(k, v):t)
  = if key == k then (k, f v) : t
    else h : updateAssocList f key t
updateAssocList _ _ [] = []


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
            return (envs', ([FreeAliasVal linkName pointingToVal], []))
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
      return (envs'', ([LocalAliasVal 0 addr pointingToVal], []))

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
                else return (envs, ([], [Rule lhsProcs' rhsProcs' rhsRules]))

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
setIndegs envs 
  = map (setIndeg envs) 

zipConcatBoth :: [([a], [b])] -> ([a], [b])
zipConcatBoth = bimap concat concat . unzip

-- | check the processes
compileProcLits :: Envs -> [ProcLit] -> ThrowsCompileError (Envs, Procs)
compileProcLits envs 
  = fmap (second zipConcatBoth) . monadicMapAccumL compileProcLit envs

-- | check the top-level process
compile :: String -> ThrowsCompileError Procs
compile input
  = case Parser.readExpr input of
      Left err -> throwError $ ParseError err
      Right procLits -> 
        do (envs, (procVals, rules)) <- compileProcLits nullEnvs procLits
           let freeLinks = S.union (freeTailEnv envs) (freeHeadEnv envs)
               procVals' = setIndegs envs procVals in
             if not $ S.null freeLinks
             then throwError $ FreeLinksOnTopLevel freeLinks 
             else
               return (procVals', rules)

