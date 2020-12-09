{-|
Module      : Compiler.TypeCheck
Description : A type checkker for the vertex language
Copyright   : (c) sano, 2020
License     : MIT
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

This module check the types of the process contexts.
The checkker infers the type of them and
test it whether the type could match with the context.

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
import qualified Data.Set                      as S
import           Data.Tuple.Extra
import qualified Data.Map.Strict               as M
import           Compiler.Compiler
import           Control.Applicative
import           Data.Maybe

-- | A type for the process contexts.
--   May better to replace the "Type" with this entirely.
data Ty = TyInt
        | TyString
        | TyUnary Int
        | TyVar Int
        -- ^ Type variable.
        --   Corresponds with the Nothing (untyped).
        deriving(Eq, Ord)

-- | A type environment
data TyEnv = TyEnv (M.Map String Ty) Int
               -- ^ A mapping from the names of the process contexts
               --   to their type
               --   and the seed for the type variable of the unary type 
               --   and the untyped. 

tMember :: String -> TyEnv -> Bool
tMember name (TyEnv mapping _) = M.member name mapping

tInsert :: String -> Ty -> TyEnv -> TyEnv
tInsert name ty (TyEnv mapping seed)
  = TyEnv (M.insert name ty mapping) seed

tEmpty :: TyEnv
tEmpty = TyEnv M.empty 0

tLookup :: String -> TyEnv -> Maybe Ty
tLookup name (TyEnv mapping _)
  = M.lookup name mapping

tUnsafeLookup :: String -> TyEnv -> Ty
tUnsafeLookup name (TyEnv mapping _) = mapping M.! name 

tToList :: TyEnv -> [(String, Ty)]
tToList (TyEnv mapping _) = M.toList mapping

tMap :: (Ty -> Ty) -> TyEnv -> TyEnv
tMap f (TyEnv mapping seed) = TyEnv (M.map f mapping) seed

-- | Convert the (Maybe) "Type", defined in the "Syntax.hs", to the "Ty"
--   using the type environment and increment the seed of the type variable.
maybeType2Ty :: TyEnv -> Maybe Type -> (Ty, TyEnv)
maybeType2Ty (TyEnv mapping i) Nothing          = (TyVar i, TyEnv mapping $ i + 1)
maybeType2Ty pCtxEnv (Just TypeInt   )            = (TyInt, pCtxEnv)
maybeType2Ty pCtxEnv (Just TypeString)            = (TyString, pCtxEnv) 
maybeType2Ty (TyEnv mapping i) (Just TypeUnary) = (TyUnary i, TyEnv mapping $ i + 1)

-- | A helper function to apply a function to the linkVal of the given procVal.
applyToLinkVal :: (LinkVal -> a) -> ProcVal -> a
applyToLinkVal f (LocalAliasVal _ _ linkVal) = f linkVal
applyToLinkVal f (FreeAliasVal  _   linkVal) = f linkVal


-- | Check the injectivity and collect process contexts on the LHS of the rule. 
checkInjectivityLinkVal :: TyEnv -> LinkVal -> ThrowsCompileError TyEnv
checkInjectivityLinkVal tyEnv linkVal@(ProcessContextVal name maybeType) =
  if tMember name tyEnv
    then throwError $ NotInjectiveProcessContext linkVal
    else return . uncurry (tInsert name) $ maybeType2Ty tyEnv maybeType
checkInjectivityLinkVal tyEnv (AtomVal _ links) =
  foldM checkInjectivityLinkVal tyEnv links
checkInjectivityLinkVal tyEnv _ = return tyEnv

checkInjectivityProcVals :: [ProcVal] -> ThrowsCompileError TyEnv
checkInjectivityProcVals = foldM (applyToLinkVal . checkInjectivityLinkVal) tEmpty




-- | Check the boundness and the op on the guard of the rule
isPolymorphicOp :: String -> Bool
isPolymorphicOp op = S.member op $ S.fromList ["=", "/="]

-- | Check the boundness and the op on the guard of the rule
isIntArithOp :: String -> Bool
isIntArithOp op =
  S.member op $ S.fromList ["+", "-", "*", "/"]

isIntRelOp :: String -> Bool
isIntRelOp op =
  S.member op $ S.fromList ["<=", ">=", "<", ">"]


-- | Extract the name of the process context.
--   Only used to gain the name of the left hand-side process context'name
--   of the assignment (:=).
extractPCtxName :: LinkVal -> ThrowsCompileError String
extractPCtxName (ProcessContextVal name Nothing) = return name
extractPCtxName linkVal@(ProcessContextVal _ (Just _)) =
  throwError $ UnexpectedTypeConstraint linkVal
  -- ^ Type constraint on the left hand side of the assignment
extractPCtxName linkVal =
  error $ "unexpected " ++ show linkVal ++ ". Should not have collectly parsed."


-- | Check at the guard
checkOpLinkVal
  :: TyEnv -> LinkVal -> ThrowsCompileError (TyEnv, Ty)
checkOpLinkVal tyEnv pCtx@(ProcessContextVal name maybeType) =
  case tLookup name tyEnv of
    Just ty -> do
      (newTyEnv, unifiedType) <-
        uncurry (unifyType pCtx) (swap $ maybeType2Ty tyEnv maybeType) ty
      return (tInsert name unifiedType newTyEnv, unifiedType)
    Nothing -> throwError $ UnboundProcessContext pCtx
checkOpLinkVal tyEnv atomVal@(AtomVal atomName [l, r]) =
  checkBinaryOp atomVal tyEnv l r
  =<< if isIntArithOp atomName
      then return TyInt
      else throwError $ UnexpectedOpOnGuard atomVal
checkOpLinkVal tyEnv (DataVal (IntAtom _)) = return (tyEnv, TyInt)
checkOpLinkVal tyEnv (DataVal (StringAtom _)) =
  return (tyEnv, TyString)
checkOpLinkVal _ linkVal = throwError $ UnexpectedOpOnGuard linkVal

substituteVar :: Int -> Ty -> TyEnv -> TyEnv
substituteVar name ty = tMap subst 
  where subst tyVar@(TyVar name2)
          = if name == name2 then ty
            else tyVar
        subst ty2 = ty2

substituteUnary :: Int -> Ty -> TyEnv -> TyEnv
substituteUnary name ty = tMap subst 
  where subst tyUnary@(TyUnary name2)
          = if name == name2 then ty
            else tyUnary
        subst ty2 = ty2

unifyType ::
  LinkVal -> TyEnv -> Ty -> Ty ->
  ThrowsCompileError (TyEnv, Ty)
unifyType linkVal tyEnv ty1 ty2
  = if ty1 == ty2 then return (tyEnv, ty1)
    else case (ty1, ty2) of
           (TyVar name1  , _) -> return (substituteVar name1 ty2 tyEnv, ty2)
           (_  , TyVar name2) -> return (substituteVar name2 ty1 tyEnv, ty1)
           (TyUnary name1, _) -> return (substituteUnary name1 ty2 tyEnv, ty2)
           (_, TyUnary name2) -> return (substituteUnary name2 ty1 tyEnv, ty1)
           _                  -> throwError $ UnexpectedTypeConstraint linkVal

unifyEnvs :: LinkVal -> TyEnv -> TyEnv -> ThrowsCompileError TyEnv
unifyEnvs linkVal leftTyEnv rightTyEnv = foldM addConstraint leftTyEnv
  $ tToList rightTyEnv
 where
  addConstraint tyEnv (pCtxName, ty1) =
    case tLookup pCtxName tyEnv of
      Nothing -> return $ tInsert pCtxName ty1 tyEnv
      Just ty2 ->
        do (newTyEnv, newTy) <-
             unifyType linkVal tyEnv ty1 ty2
           return $ tInsert pCtxName newTy newTyEnv

checkBinaryOp
  :: LinkVal
  -> TyEnv
  -> LinkVal
  -> LinkVal
  -> Ty
  -> ThrowsCompileError (TyEnv, Ty)
checkBinaryOp linkVal tyEnv left right inferedType = do
  (leftTyEnv , leftTy ) <- checkOpLinkVal tyEnv left
  (rightTyEnv, rightTy) <- checkOpLinkVal tyEnv right
  unifiedTyEnv          <- unifyEnvs linkVal leftTyEnv rightTyEnv
  (newTyEnv, newTy)     <- unifyType linkVal unifiedTyEnv leftTy rightTy
  unifyType linkVal newTyEnv newTy inferedType

-- | Check on the guard.
checkOpProcVal :: TyEnv -> ProcVal -> ThrowsCompileError TyEnv
checkOpProcVal tyEnv (LocalAliasVal 0 _ linkVal@(AtomVal atomName [l, r]))
  = if atomName == ":="
    then do
      leftPCtxName <- extractPCtxName l
      if tMember leftPCtxName tyEnv
        then throwError $ ShadowingProcessContext linkVal
        else
          uncurry (tInsert leftPCtxName) . swap
          <$> checkOpLinkVal tyEnv r
    else
      do (inferedTy, newTyEnv) <-
            if isPolymorphicOp atomName 
            then return $ maybeType2Ty tyEnv $ Just TypeUnary
            else if isIntRelOp atomName
            then return $ maybeType2Ty tyEnv $ Just TypeInt
            else throwError $ UnexpectedOpOnGuard linkVal
         fst <$> checkBinaryOp linkVal newTyEnv l r inferedTy
        
checkOpProcVal tyEnv (LocalAliasVal 0 _ pCtx@(ProcessContextVal name maybeType@(Just _)))
  = case tLookup name tyEnv of
    Just ty -> do
      (newTyEnv, unifiedType) <-
        uncurry (unifyType pCtx) (swap $ maybeType2Ty tyEnv maybeType) ty
      return $ tInsert name unifiedType newTyEnv
    Nothing -> throwError $ UnboundProcessContext pCtx
checkOpProcVal _ (LocalAliasVal _ _ linkVal) =
  throwError $ UnexpectedOpOnGuard linkVal
checkOpProcVal _ procVal =
  error
    $  "No link should appear in the guard \""
    ++ show procVal
    ++ "\" , which should have already checked in the compilation process. "

checkOpProcVals :: [ProcVal] -> TyEnv -> ThrowsCompileError TyEnv
checkOpProcVals = flip $ foldM checkOpProcVal


isProcessContext :: ProcVal -> Bool
isProcessContext (LocalAliasVal _ _ (ProcessContextVal _ _)) = True
isProcessContext _ = False


-- | Check the free occurence and the type constraints on the RHS of the rule.
--   Currently the typing on the RHS of the rule is not supported.
collectTypesLinkVal :: TyEnv -> LinkVal -> ThrowsCompileError TyEnv
collectTypesLinkVal _ pCtx@(ProcessContextVal _ (Just _)) =
  throwError $ UnexpectedTypeConstraint pCtx
collectTypesLinkVal tyEnv pCtx@(ProcessContextVal name Nothing) =
  if tMember name tyEnv
    then return tyEnv
    else throwError $ UnboundProcessContext pCtx
collectTypesLinkVal tyEnv (AtomVal _ links) =
  foldM collectTypesLinkVal tyEnv links
collectTypesLinkVal tyEnv _ = return tyEnv

collectTypesProcVals :: [ProcVal] -> TyEnv -> ThrowsCompileError TyEnv
collectTypesProcVals = 
  flip $ foldM $ applyToLinkVal . collectTypesLinkVal


-- | Check that there is at least one untyped process context appears.
--   However, the untyped process context is currently not supported.
--   So, this check may should be more strict that doesn't allow the existence
--   of the (even one) untyped process context at all.
checkMultipleUtypedProcessContexts :: TyEnv -> ThrowsCompileError TyEnv
checkMultipleUtypedProcessContexts tyEnv =
  let untypedProcessContexts =
        mapMaybe maybeProcessContextVal $ tToList tyEnv
  in  if length untypedProcessContexts > 1
        then throwError $ MultipleUtypedProcessContexts untypedProcessContexts
        else return tyEnv
 where
  maybeProcessContextVal (name, TyVar _)
    = Just $ ProcessContextVal name Nothing
  maybeProcessContextVal _ = Nothing

-- | Convert "Ty" to the "Maybe Type"
ty2MaybeType :: Ty -> Maybe Type
ty2MaybeType TyInt       = Just TypeInt
ty2MaybeType TyString    = Just TypeString
ty2MaybeType (TyUnary _) = Just TypeUnary
ty2MaybeType (TyVar _)   = Nothing

-- | Set the type of the process contexts to the infered type
--   based on the given type environment.  
setInferedTypeLinkVal :: TyEnv -> LinkVal -> LinkVal
setInferedTypeLinkVal tyEnv (ProcessContextVal name _)
  = ProcessContextVal name $ ty2MaybeType $ tUnsafeLookup name tyEnv 
setInferedTypeLinkVal tyEnv (AtomVal atomName links)
  = AtomVal atomName $ map (setInferedTypeLinkVal tyEnv) links
setInferedTypeLinkVal _ linkOrData = linkOrData

setInferedTypeProcVal :: TyEnv -> ProcVal -> ProcVal
setInferedTypeProcVal tyEnv (LocalAliasVal indeg fromAddr linkVal)
  = LocalAliasVal indeg fromAddr $ setInferedTypeLinkVal tyEnv linkVal
setInferedTypeProcVal tyEnv (FreeAliasVal linkName linkVal)
  = FreeAliasVal linkName $ setInferedTypeLinkVal tyEnv linkVal

setInferedTypeProcVals :: TyEnv -> [ProcVal] -> [ProcVal]
setInferedTypeProcVals = map . setInferedTypeProcVal

checkRule :: Rule -> ThrowsCompileError Rule
checkRule (Rule maybeName lhs guard rhs rhsRules) =
  do tyEnv <- checkInjectivityProcVals lhs
                >>= checkOpProcVals guard
                >>= checkMultipleUtypedProcessContexts
                >>= collectTypesProcVals rhs
     newRhsRules <- mapM checkRule rhsRules
     return $
       Rule
       maybeName
       (setInferedTypeProcVals tyEnv lhs)
       (filter (not . isProcessContext) guard)
       rhs
       newRhsRules

typeCheck :: Procs -> ThrowsCompileError Procs
typeCheck (procVals, rules)
  = liftA2 (,)
    (procVals <$ collectTypesProcVals procVals tEmpty)
    $ mapM checkRule rules

