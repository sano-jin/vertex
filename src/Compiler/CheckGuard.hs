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
-- import           Control.Monad hiding ( guard )


collectTypesLinkVal :: LinkVal -> S.Set Type
collectTypesLinkVal (ProcessContextVal _ (Just type_)) = S.singleton type_
collectTypesLinkVal (AtomVal _ links) = S.unions $ map collectTypesLinkVal links
collectTypesLinkVal _ = S.empty

collectTypesProcVal :: ProcVal -> S.Set Type
collectTypesProcVal (LocalAliasVal _ _ linkVal) = collectTypesLinkVal linkVal
collectTypesProcVal (FreeAliasVal _ linkVal) = collectTypesLinkVal linkVal

collectTypesProcVals :: [ProcVal] -> S.Set Type
collectTypesProcVals procVals = S.unions $ map collectTypesProcVal procVals



checkInjectivityLinkVal :: S.Set String -> LinkVal -> Either LinkVal (S.Set String)
checkInjectivityLinkVal pCtxNames linkVal@(ProcessContextVal name _) 
  = if S.member name pCtxNames then throwError linkVal
    else return $ S.insert name pCtxNames 
checkInjectivityLinkVal pCtxNames (AtomVal _ links)
  = foldM checkInjectivityLinkVal pCtxNames links
checkInjectivityLinkVal pCtxNames _ = return pCtxNames

checkInjectivityProcVal :: S.Set String -> ProcVal -> Either LinkVal (S.Set String)
checkInjectivityProcVal pCtxNames (LocalAliasVal _ _ linkVal)
  = checkInjectivityLinkVal pCtxNames linkVal
checkInjectivityProcVal pCtxNames (FreeAliasVal _ linkVal)
  = checkInjectivityLinkVal pCtxNames linkVal

checkInjectivityProcVals :: [ProcVal] -> Either LinkVal (S.Set String)
checkInjectivityProcVals procVals = foldM checkInjectivityProcVal S.empty procVals



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


checkOpLinkVal :: LinkVal -> Either LinkVal ()
checkOpLinkVal (ProcessContextVal _ _) = return ()
checkOpLinkVal linkVal@(AtomVal atomName links@[_, _])
  = (if isGuardOp atomName then return ()
     else throwError linkVal)
    >> mapM_ checkOpLinkVal links
checkOpLinkVal (DataVal _) = return ()
checkOpLinkVal linkVal = throwError linkVal

checkOpProcVal :: ProcVal -> Either LinkVal ()
checkOpProcVal (LocalAliasVal 0 _ linkVal@(AtomVal atomName links@[_, _]))
  = (if isGuardOp atomName || atomName == ":=" then return ()
     else throwError linkVal)
    >> mapM_ checkOpLinkVal links
checkOpProcVal (LocalAliasVal _ _ linkVal) = throwError linkVal
checkOpProcVal procVal
  = error
  $  "No link should appear in the guard \""
  ++ show procVal
  ++ "\" , which should have already checked in the compilation process. "

checkOpProcVals :: [ProcVal] -> Either LinkVal ()
checkOpProcVals procVals = mapM_ checkOpProcVal procVals

checkRule :: Rule -> ThrowsCompileError Rule
checkRule rule@(Rule _ lhs guard rhs rhsRules)
 = case checkInjectivityProcVals lhs of
     Left notInjectiveLinkVal ->
       throwError $ NotInjectiveProcessContext notInjectiveLinkVal rule
     Right _ ->
       let typeConstraintsOnRHS = collectTypesProcVals rhs in
       if not $ S.null typeConstraintsOnRHS 
       then throwError $ TypeConstraintsOnRHS typeConstraintsOnRHS rule
       else case checkOpProcVals guard of
              Left unexpectedOp ->
                throwError $ UnexpectedOpOnGuard unexpectedOp rule
              Right _ -> rule <$ mapM_ checkRule rhsRules
         

checkRules :: Procs -> ThrowsCompileError Procs
checkRules (procVals, rules)
  = (procVals, rules) <$ mapM_ checkRule rules
