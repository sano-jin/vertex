{-|
Module      : Compiler.TypeCheck
Description : A compiler for the vertex language
Copyright   : (c) sano, 2020
License     : MIT
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Compiler.TypeCheck
  ( typeCheck
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
data Ty = TyInt
        | TyString
        | TyUnary
        | TyVar String
        deriving(Eq, Ord)

-- | A type environment
type PCtxEnv = M.Map String Ty
               -- ^ A mapping from the names of the process contexts
               -- to their type 

maybeType2Ty :: String -> Maybe Type -> Ty
maybeType2Ty name Nothing        = TyVar name
maybeType2Ty _ (Just TypeInt   ) = TyInt
maybeType2Ty _ (Just TypeString) = TyString
maybeType2Ty _ (Just TypeUnary ) = TyUnary

-- | Check the injectivity and collect process contexts on the LHS of the rule. 
checkInjectivityLinkVal :: PCtxEnv -> LinkVal -> ThrowsCompileError PCtxEnv
checkInjectivityLinkVal pCtxEnv linkVal@(ProcessContextVal name maybeType) =
  if M.member name pCtxEnv
    then throwError $ NotInjectiveProcessContext linkVal
    else return $ M.insert name (maybeType2Ty name maybeType) pCtxEnv
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
  :: PCtxEnv -> LinkVal -> ThrowsCompileError (PCtxEnv, Ty)
checkOpLinkVal pCtxEnv pCtx@(ProcessContextVal name maybeType) =
  case M.lookup name pCtxEnv of
    Just ty -> do
      (newPCtxEnv, unifiedType) <-
        unifyType pCtx pCtxEnv (maybeType2Ty name maybeType) ty
      return (M.insert name unifiedType newPCtxEnv, unifiedType)
    Nothing -> throwError $ UnboundProcessContext pCtx
checkOpLinkVal pCtxEnv atomVal@(AtomVal atomName links@[l, r]) =
  checkBinaryOp atomVal pCtxEnv l r
  =<< if isIntOp atomName
    then return TyInt
    else throwError $ UnexpectedOpOnGuard atomVal
checkOpLinkVal pCtxEnv (DataVal (IntAtom _)) = return (pCtxEnv, TyInt)
checkOpLinkVal pCtxEnv (DataVal (StringAtom _)) =
  return (pCtxEnv, TyString)
checkOpLinkVal _ linkVal = throwError $ UnexpectedOpOnGuard linkVal

substitute :: String -> Ty -> PCtxEnv -> PCtxEnv
substitute name ty pCtxEnv =
  M.map subst pCtxEnv
  where subst tyVar@(TyVar name2)
          = if name == name2 then ty
            else tyVar
        subst ty2 = ty2

unifyType ::
  LinkVal -> PCtxEnv -> Ty -> Ty ->
  ThrowsCompileError (PCtxEnv, Ty)
unifyType linkVal pCtxEnv ty1 ty2
  = if ty1 == ty2 then return (pCtxEnv, ty1)
    else case (ty1, ty2) of
           (TyVar name1, _) -> return (substitute name1 ty2 pCtxEnv, ty2)
           (_, TyVar name2) -> return (substitute name2 ty1 pCtxEnv, ty1)
           (TyUnary, _    ) -> return (pCtxEnv, ty2)
           (_, TyUnary    ) -> return (pCtxEnv, ty1)
           _                -> throwError $ UnexpectedTypeConstraint linkVal

unifyEnvs :: LinkVal -> PCtxEnv -> PCtxEnv -> ThrowsCompileError PCtxEnv
unifyEnvs linkVal leftPCtxEnv rightPCtxEnv = foldM addConstraint leftPCtxEnv
  $ M.toList rightPCtxEnv
 where
  addConstraint pCtxEnv (pCtxName, ty1) =
    case M.lookup pCtxName pCtxEnv of
      Nothing -> return $ M.insert pCtxName ty1 pCtxEnv
      Just ty2 ->
        do (newPCtxEnv, newTy) <-
             unifyType linkVal pCtxEnv ty1 ty2
           return $ M.insert pCtxName newTy newPCtxEnv

checkBinaryOp
  :: LinkVal
  -> PCtxEnv
  -> LinkVal
  -> LinkVal
  -> Ty
  -> ThrowsCompileError (PCtxEnv, Ty)
checkBinaryOp linkVal pCtxEnv left right inferedType = do
  (leftPCtxEnv , leftTy ) <- checkOpLinkVal pCtxEnv left
  (rightPCtxEnv, rightTy) <- checkOpLinkVal pCtxEnv right
  unifiedPCtxEnv          <- unifyEnvs linkVal leftPCtxEnv rightPCtxEnv
  (newPCtxEnv, newTy)     <- unifyType linkVal unifiedPCtxEnv leftTy rightTy
  unifyType linkVal newPCtxEnv newTy inferedType
  
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
              then return TyUnary
              else if isIntOp atomName
                then return TyInt
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
  maybeProcessContextVal (name, TyVar _)
    = Just $ ProcessContextVal name Nothing
  maybeProcessContextVal _ = Nothing


ty2MaybeType :: Ty -> Maybe Type
ty2MaybeType TyInt     = Just TypeInt
ty2MaybeType TyString  = Just TypeString
ty2MaybeType TyUnary   = Just TypeUnary
ty2MaybeType (TyVar _) = Nothing

-- | Set the type of the process contexts to the infered type
--   based on the given type environment.  
setInferedTypeLinkVal :: PCtxEnv -> LinkVal -> LinkVal
setInferedTypeLinkVal pCtxEnv (ProcessContextVal name _)
  = ProcessContextVal name $ ty2MaybeType $ pCtxEnv M.! name
setInferedTypeLinkVal pCtxEnv (AtomVal atomName links)
  = AtomVal atomName $ map (setInferedTypeLinkVal pCtxEnv) links
setInferedTypeLinkVal _ linkOrData = linkOrData

setInferedTypeProcVal :: PCtxEnv -> ProcVal -> ProcVal
setInferedTypeProcVal pCtxEnv (LocalAliasVal indeg fromAddr linkVal)
  = LocalAliasVal indeg fromAddr $ setInferedTypeLinkVal pCtxEnv linkVal
setInferedTypeProcVal pCtxEnv (FreeAliasVal linkName linkVal)
  = FreeAliasVal linkName $ setInferedTypeLinkVal pCtxEnv linkVal

setInferedTypeProcVals :: PCtxEnv -> [ProcVal] -> [ProcVal]
setInferedTypeProcVals pCtxEnv procVals
  = map (setInferedTypeProcVal pCtxEnv) procVals

checkRule :: Rule -> ThrowsCompileError Rule
checkRule (Rule maybeName lhs guard rhs rhsRules) =
  do pCtxEnv <- checkInjectivityProcVals lhs
                >>= checkOpProcVals guard
                >>= checkMultipleUtypedProcessContexts
                >>= collectTypesProcVals rhs
     newRhsRules <- mapM checkRule rhsRules
     return $
       Rule
       maybeName
       (setInferedTypeProcVals pCtxEnv lhs)
       guard
       rhs
       newRhsRules

typeCheck :: Procs -> ThrowsCompileError Procs
typeCheck (procVals, rules)
  = liftA2 (,)
    (procVals <$ collectTypesProcVals procVals M.empty)
    $ mapM checkRule rules

