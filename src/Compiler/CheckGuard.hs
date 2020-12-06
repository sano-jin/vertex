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

module Compiler.CheckGuard
  ( checkRules
  ) where
import           Compiler.Process
import           Compiler.Syntax                ( Type     (..)
--                                                , DataAtom (..)
                                                )
import           Control.Monad.Except           hiding ( guard )
-- import           Data.Bifunctor                 ( bimap )
-- import           Data.List
import qualified Data.Set                      as S
-- import           Data.Tuple.Extra
import           Compiler.Compiler
import           Control.Monad hiding ( guard )
import           Control.Applicative


-- | Check the injectivity and collect process contexts on the LHS of the rule. 
checkInjectivityLinkVal :: S.Set String -> LinkVal -> ThrowsCompileError (S.Set String)
checkInjectivityLinkVal pCtxNames linkVal@(ProcessContextVal name _) 
  = if S.member name pCtxNames
    then throwError $ NotInjectiveProcessContext linkVal
    else return $ S.insert name pCtxNames 
checkInjectivityLinkVal pCtxNames (AtomVal _ links)
  = foldM checkInjectivityLinkVal pCtxNames links
checkInjectivityLinkVal pCtxNames _ = return pCtxNames

checkInjectivityProcVal :: S.Set String -> ProcVal -> ThrowsCompileError (S.Set String)
checkInjectivityProcVal pCtxNames (LocalAliasVal _ _ linkVal)
  = checkInjectivityLinkVal pCtxNames linkVal
checkInjectivityProcVal pCtxNames (FreeAliasVal _ linkVal)
  = checkInjectivityLinkVal pCtxNames linkVal

checkInjectivityProcVals :: [ProcVal] -> ThrowsCompileError (S.Set String)
checkInjectivityProcVals = foldM checkInjectivityProcVal S.empty 




-- | Check the boundness and the op on the guard of the rule
isGuardOp :: String -> Bool
isGuardOp op
  = S.member op $ S.fromList [ "+"
                             , "-"
                             , "*"
                             , "/"
                             , "="
                             , "/="
                             , "<="
                             , ">="
                             , "<"
                             , ">"
                             ]

extractNameOfPCtx :: LinkVal -> ThrowsCompileError String
extractNameOfPCtx (ProcessContextVal name Nothing) = return name
extractNameOfPCtx linkVal = throwError $ UnexpectedOpOnGuard linkVal

checkOpLinkVal :: S.Set String -> LinkVal -> ThrowsCompileError (S.Set String)
checkOpLinkVal pCtxNames pCtx@(ProcessContextVal name Nothing)
  = if S.member name pCtxNames then return pCtxNames
    else throwError $ UnboundProcessContext pCtx
checkOpLinkVal pCtxNames linkVal@(AtomVal atomName links@[_, _])
  = if isGuardOp atomName then foldM checkOpLinkVal pCtxNames links
    else throwError $ UnexpectedOpOnGuard linkVal
checkOpLinkVal pCtxNames (DataVal _) = return pCtxNames
checkOpLinkVal _ linkVal = throwError $ UnexpectedOpOnGuard linkVal

checkOpProcVal :: S.Set String -> ProcVal -> ThrowsCompileError (S.Set String)
checkOpProcVal pCtxNames (LocalAliasVal 0 _ linkVal@(AtomVal atomName links@[l, r]))
  = if isGuardOp atomName then foldM checkOpLinkVal pCtxNames links
    else if atomName == ":="
         then liftA2 S.insert (extractNameOfPCtx l) $ checkOpLinkVal pCtxNames r
         else throwError $ UnexpectedOpOnGuard linkVal
checkOpProcVal pCtxNames (LocalAliasVal _ _ linkVal)
  = throwError $ UnexpectedOpOnGuard linkVal
checkOpProcVal _ procVal
  = error
  $  "No link should appear in the guard \""
  ++ show procVal
  ++ "\" , which should have already checked in the compilation process. "

checkOpProcVals :: [ProcVal] -> S.Set String -> ThrowsCompileError (S.Set String)
checkOpProcVals procVals pCtxNames = foldM checkOpProcVal pCtxNames procVals


-- | Check the boundness and the type constraints on the RHS of the rule
collectTypesLinkVal :: S.Set String -> LinkVal -> ThrowsCompileError ()
collectTypesLinkVal pCtxNames pCtx@(ProcessContextVal _ (Just type_))
  = throwError $ TypeConstraintsOnRHS pCtx
collectTypesLinkVal pCtxNames pCtx@(ProcessContextVal name Nothing)
  = if S.member name pCtxNames then return ()
    else throwError $ UnboundProcessContext pCtx
collectTypesLinkVal pCtxNames (AtomVal _ links)
  = mapM_ (collectTypesLinkVal pCtxNames) links
collectTypesLinkVal pCtxNames _ = return ()

collectTypesProcVal :: S.Set String -> ProcVal -> ThrowsCompileError ()
collectTypesProcVal pCtxNames (LocalAliasVal _ _ linkVal)
  = collectTypesLinkVal pCtxNames linkVal
collectTypesProcVal pCtxNames (FreeAliasVal _ linkVal)
  = collectTypesLinkVal pCtxNames linkVal

collectTypesProcVals :: [ProcVal] -> S.Set String-> ThrowsCompileError ()
collectTypesProcVals procVals pCtxNames
  = mapM_ (collectTypesProcVal pCtxNames) procVals



checkRule :: Rule -> ThrowsCompileError ()
checkRule rule@(Rule _ lhs guard rhs rhsRules)
 = checkInjectivityProcVals lhs
   >>= checkOpProcVals guard
   >>= collectTypesProcVals rhs
   >> mapM_ checkRule rhsRules

checkRules :: Procs -> ThrowsCompileError Procs
checkRules procs@(_, rules)
  = procs <$ mapM_ checkRule rules



{--|

collectPCtxsLinkVal :: LinkVal -> S.Set String
collectPCtxsLinkVal (ProcessContextVal name _) = S.singleton name
collectPCtxsLinkVal (AtomVal _ links) = S.unions $ map collectPCtxsLinkVal links
collectPCtxsLinkVal _ = S.empty

collectPCtxsProcVal :: ProcVal -> S.Set String
collectPCtxsProcVal (LocalAliasVal _ _ linkVal) = collectPCtxsLinkVal linkVal
collectPCtxsProcVal (FreeAliasVal _ linkVal) = collectPCtxsLinkVal linkVal

collectPCtxsProcVals :: [ProcVal] -> S.Set String
collectPCtxsProcVals procVals = S.unions $ map collectPCtxsProcVal procVals



|--}

