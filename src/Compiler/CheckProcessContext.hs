{-|
Module      : Compiler.CheckProcessContext
Description : A compiler for the vertex language
Copyright   : (c) sano, 2020
License     : MIT
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Compiler.CheckProcessContext
  ( checkRules
  ) where
import           Compiler.Process
import           Compiler.Syntax                ( Type(..)
                                                , DataAtom(..)
                                                )
import           Control.Monad.Except    hiding ( guard )
-- import           Data.Bifunctor                 ( bimap )
-- import           Data.List
import qualified Data.Set                      as S
import           Data.Tuple.Extra
import qualified Data.Map.Strict               as M
import           Compiler.Compiler
import           Control.Monad           hiding ( guard )
import           Control.Applicative
import           Data.Maybe

-- | A type for the Envirnment of process contexts
type PCtxEnv = M.Map String (Maybe Type)
               -- ^ A mapping from the names of the process contexts
               -- to their type 


-- | Check the injectivity and collect process contexts on the LHS of the rule. 
checkInjectivityLinkVal :: PCtxEnv -> LinkVal -> ThrowsCompileError PCtxEnv
checkInjectivityLinkVal pCtxEnv linkVal@(ProcessContextVal name maybeType) =
  if M.member name pCtxEnv
    then throwError $ NotInjectiveProcessContext linkVal
    else return $ M.insert name maybeType pCtxEnv
checkInjectivityLinkVal pCtxEnv (AtomVal _ links) =
  foldM checkInjectivityLinkVal pCtxEnv links
checkInjectivityLinkVal pCtxEnv _ = return pCtxEnv

checkInjectivityProcVal :: PCtxEnv -> ProcVal -> ThrowsCompileError PCtxEnv
checkInjectivityProcVal pCtxEnv (LocalAliasVal _ _ linkVal) =
  checkInjectivityLinkVal pCtxEnv linkVal
checkInjectivityProcVal pCtxEnv (FreeAliasVal _ linkVal) =
  checkInjectivityLinkVal pCtxEnv linkVal

checkInjectivityProcVals :: [ProcVal] -> ThrowsCompileError PCtxEnv
checkInjectivityProcVals = foldM checkInjectivityProcVal M.empty




-- | Check the boundness and the op on the guard of the rule
isPolymorphicOp :: String -> Bool
isPolymorphicOp op = S.member op $ S.fromList ["=", "/="]

-- | Check the boundness and the op on the guard of the rule
isIntOp :: String -> Bool
isIntOp op =
  S.member op $ S.fromList ["+", "-", "*", "/", "<=", ">=", "<", ">"]


isGuardOp :: String -> Bool
isGuardOp op = isIntOp op || isPolymorphicOp op

extractPCtxName :: LinkVal -> ThrowsCompileError String
extractPCtxName (ProcessContextVal name Nothing) = return name
extractPCtxName linkVal@(ProcessContextVal _ (Just _)) =
  throwError $ UnexpectedTypeConstraint linkVal
  -- ^ Type constraint on the left hand side of the assignment
extractPCtxName linkVal =
  error $ "unexpected " ++ show linkVal ++ ". Should not have collectly parsed."


checkOpLinkVal
  :: PCtxEnv -> LinkVal -> ThrowsCompileError (PCtxEnv, Maybe Type)
checkOpLinkVal pCtxEnv pCtx@(ProcessContextVal name maybeType) =
  case M.lookup name pCtxEnv of
    Just maybeType' -> do
      unifiedType <- unifyType pCtx maybeType maybeType'
      return (M.insert name unifiedType pCtxEnv, unifiedType)
    Nothing -> throwError $ UnboundProcessContext pCtx
checkOpLinkVal pCtxEnv atomVal@(AtomVal atomName links@[l, r]) =
  checkBinaryOp atomVal pCtxEnv l r =<< if isIntOp atomName
    then return $ Just TypeInt
    else throwError $ UnexpectedOpOnGuard atomVal
checkOpLinkVal pCtxEnv (DataVal (IntAtom _)) = return (pCtxEnv, Just TypeInt)
checkOpLinkVal pCtxEnv (DataVal (StringAtom _)) =
  return (pCtxEnv, Just TypeString)
checkOpLinkVal _ linkVal = throwError $ UnexpectedOpOnGuard linkVal


unifyType
  :: LinkVal -> Maybe Type -> Maybe Type -> ThrowsCompileError (Maybe Type)
unifyType linkVal lMaybeType rMaybeType = case (lMaybeType, rMaybeType) of
  (_         , Nothing   ) -> return lMaybeType
  (Nothing   , _         ) -> return rMaybeType
  (Just lType, Just rType) -> case (lType, rType) of
    (TypeUnary , _         ) -> return $ Just rType
    (_         , TypeUnary ) -> return $ Just lType
    (TypeString, TypeString) -> return $ Just TypeString
    (TypeInt   , TypeInt   ) -> return $ Just TypeInt
    _                        -> throwError $ UnexpectedTypeConstraint linkVal

unifyEnvs :: LinkVal -> PCtxEnv -> PCtxEnv -> ThrowsCompileError PCtxEnv
unifyEnvs linkVal leftPCtxEnv rightPCtxEnv = foldM addConstraint leftPCtxEnv
  $ M.toList rightPCtxEnv
 where
  addConstraint pCtxEnv (pCtxName, maybeType) =
    case M.lookup pCtxName pCtxEnv of
      Nothing -> return $ M.insert pCtxName maybeType pCtxEnv
      Just maybeType' ->
        flip (M.insert pCtxName) pCtxEnv
          <$> unifyType linkVal maybeType maybeType'

checkBinaryOp
  :: LinkVal
  -> PCtxEnv
  -> LinkVal
  -> LinkVal
  -> Maybe Type
  -> ThrowsCompileError (PCtxEnv, Maybe Type)
checkBinaryOp linkVal pCtxEnv left right inferedType = do
  (leftPCtxEnv , leftMaybeType ) <- checkOpLinkVal pCtxEnv left
  (rightPCtxEnv, rightMaybeType) <- checkOpLinkVal pCtxEnv right
  unifiedPCtxEnv                 <- unifyEnvs linkVal leftPCtxEnv rightPCtxEnv
  unifiedType                    <-
    unifyType linkVal inferedType
      =<< unifyType linkVal leftMaybeType rightMaybeType
  return (unifiedPCtxEnv, unifiedType)

checkOpProcVal :: PCtxEnv -> ProcVal -> ThrowsCompileError PCtxEnv
checkOpProcVal pCtxEnv (LocalAliasVal 0 _ linkVal@(AtomVal atomName links@[l, r]))
  = if atomName == ":="
    then do
      leftPCtxName <- extractPCtxName l
      if M.member leftPCtxName pCtxEnv
        then throwError $ ShadowingProcessContext linkVal
        else
          uncurry (M.insert leftPCtxName) . swap
          <$> checkOpLinkVal pCtxEnv r
    else
      fst
        <$> (checkBinaryOp linkVal pCtxEnv l r
             =<< if isPolymorphicOp atomName
              then return Nothing
              else if isIntOp atomName
                then return $ Just TypeInt
                else throwError $ UnexpectedOpOnGuard linkVal
            )
        
checkOpProcVal pCtxEnv (LocalAliasVal _ _ linkVal) =
  throwError $ UnexpectedOpOnGuard linkVal
checkOpProcVal _ procVal =
  error
    $  "No link should appear in the guard \""
    ++ show procVal
    ++ "\" , which should have already checked in the compilation process. "

checkOpProcVals :: [ProcVal] -> PCtxEnv -> ThrowsCompileError PCtxEnv
checkOpProcVals procVals pCtxEnv = foldM checkOpProcVal pCtxEnv procVals


-- | Check the free occurence and the type constraints on the RHS of the rule
collectTypesLinkVal :: PCtxEnv -> LinkVal -> ThrowsCompileError PCtxEnv
collectTypesLinkVal pCtxEnv pCtx@(ProcessContextVal _ (Just type_)) =
  throwError $ UnexpectedTypeConstraint pCtx
collectTypesLinkVal pCtxEnv pCtx@(ProcessContextVal name Nothing) =
  if M.member name pCtxEnv
    then return pCtxEnv
    else throwError $ UnboundProcessContext pCtx
collectTypesLinkVal pCtxEnv (AtomVal _ links) =
  foldM collectTypesLinkVal pCtxEnv links
collectTypesLinkVal pCtxEnv _ = return pCtxEnv

collectTypesProcVal :: PCtxEnv -> ProcVal -> ThrowsCompileError PCtxEnv
collectTypesProcVal pCtxEnv (LocalAliasVal _ _ linkVal) =
  collectTypesLinkVal pCtxEnv linkVal
collectTypesProcVal pCtxEnv (FreeAliasVal _ linkVal) =
  collectTypesLinkVal pCtxEnv linkVal

collectTypesProcVals :: [ProcVal] -> PCtxEnv -> ThrowsCompileError PCtxEnv
collectTypesProcVals procVals pCtxEnv =
  foldM collectTypesProcVal pCtxEnv procVals

checkMultipleUtypedProcessContexts :: PCtxEnv -> ThrowsCompileError PCtxEnv
checkMultipleUtypedProcessContexts pCtxEnv =
  let untypedProcessContexts =
        S.fromList $ mapMaybe maybeProcessContextVal $ M.toList pCtxEnv
  in  if S.size untypedProcessContexts > 1
        then throwError $ MultipleUtypedProcessContexts untypedProcessContexts
        else return pCtxEnv
 where
  maybeProcessContextVal (_, Just _) = Nothing
  maybeProcessContextVal (name, Nothing) =
    Just $ ProcessContextVal name Nothing

checkRule :: Rule -> ThrowsCompileError ()
checkRule rule@(Rule _ lhs guard rhs rhsRules) =
  checkInjectivityProcVals lhs
    >>= checkOpProcVals guard
    >>= checkMultipleUtypedProcessContexts
    >>= collectTypesProcVals rhs
    >>  mapM_ checkRule rhsRules

checkRules :: Procs -> ThrowsCompileError Procs
checkRules procs@(_, rules) = procs <$ mapM_ checkRule rules

