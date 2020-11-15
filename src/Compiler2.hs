module Compiler where
import System.Environment
import Data.IORef
import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Parser (
  readExpr,
  showBlock,
  ParseError,
  SourcePos
  )
import Syntax

type Addr = Int
type Indeg = Int
data Node = NAtom String [Addr]    -- NAtom SymbolAtomName [Pointers]
          | NInd Addr              -- Alias to Addr

type Heap = [(Addr, Indeg, Node)]  -- [(Address, Indegree, Atom)]


data PointerLit = FreePointerLit Indeg String
                | LocalPointerLit Indeg String  -- X
                | AtomLit String [PointerLit]       -- p(X1,...,Xm)
                deriving(Eq, Ord, Show)


data ProcVal = LocalAliasVal Indeg (Maybe String) PointerLit   -- \X.X -> p(X1,...,Xm)
             | FreeAliasVal  Indeg String PointerLit   -- X -> p(X1,...,Xm)
             | RuleVal [ProcVal] [ProcVal]             -- P :- P
             deriving(Eq, Ord, Show)

type RuleSet = [(ProcVal, ProcVal)]


type ThrowsError = Either CompileError
type IOThrowsError = ExceptT CompileError IO
instance Show CompileError where show = showError

data CompileError = IsNotSerial String 
                  | IsNotFunctional String
                  | RuleOnLHS ProcLit
                  | Parser Parser.ParseError


-- show
showError :: CompileError -> String
showError (IsNotSerial name ) =
  "pointer '" ++ name ++ "' is not serial.\n"
showError (IsNotFunctional name) =
  "pointer '" ++ name ++ "' is not functional.\n"
showError (Parser parseError) = "Parse error at " ++ show parseError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val


-- PointerName -> (Indegree, hasHead)
type HasHead = Bool
type IsLocal = Bool
type EnvList = [(String, (Indeg, HasHead))]
type EnvSet  = S.Set String


-- seed -> (newAddr, newSeed)
newAddr :: Addr -> (Addr, Addr)    
newAddr oldAddr = (oldAddr, oldAddr + 1)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c



fst4 :: (a, b, c, d) -> a
fst4 (a, _, _, _) = a

fst5 :: (a, b, c, d, e) -> a
fst5 (a, _, _, _, _) = a

lookupEnv_ :: String -> EnvList -> Maybe Indeg
lookupEnv_ pointerName env
  = liftM fst $ lookup pointerName env

lookupAssocListWithErr :: Eq key => key -> [(key, value)] -> value
lookupAssocListWithErr key ((k, v):t)
  = if key == k then v
    else lookupAssocListWithErr key t


updateAssocList :: Monad m => Eq key =>
  key -> (value -> m value) -> [(key, value)] -> m [(key, value)]
updateAssocList _ _ [] = return []
updateAssocList key f ((k, v):t) =
  if key == k then
    do fv <- f v
       return $ (k, fv):t
  else liftM ((k, v) :) $ updateAssocList key f t

mapSnd :: (b -> b) -> (a, b) -> (a, b)
mapSnd f (a, b) = (a, f b)

-- type Envs = (EnvList, EnvSet, EnvSet)

data Envs = Envs { localEnv :: EnvList
                 , freeTailEnv :: EnvSet
                 , freeHeadEnv :: EnvSet
                 } deriving (Show)

nullEnv = Envs { localEnv = []
              , freeTailEnv = M.empty
              , freeHeadEnv = M.empty
              }

compilePointingToLit ::
  PointerLit -> Envs -> (PointerVal, Envs)
compilePointingToLit pointingTo envs
  = envs

-- procLit -> oldHeap -> oldEnv -> oldRuleSet -> oldAddrSeed
-- -> (newHeap, newEnv, newRuleSet, newAddr)
compileProcLit ::
  ProcLit -> Envs -> RuleSet ->
  ThrowsError (ProcVal, (Envs, RuleSet))
compileProcLit (AliasLit (Just pointerName) pointingTo) envs oldRuleSet
  = case lookup pointerName $ localEnv envs of
      Nothing ->
        if M.member pointerName $ freeTailEnv envs
        then throwError $ IsNotFunctional pointerName
        else
          let envs = compilePointingToLit pointingTo
                     $ if M.member pointerName $ freeTailEnv envs then envs
                       else
                          envs { freeTailEnv = M.insert pointerName 0 $ freeTailEnv envs }
              indeg = freeTailEnv envs M.! pointerName
          in
            return $ (LocalAliasVal indeg (Just pointerName) pointingTo, (envs, oldRuleSet))
      Just (_, True) -> throwError $ IsNotFunctional pointerName
      Just (_, False) ->
        let envs = compilePointingToLit pointingTo envs
            indeg = fst $ lookupAssocListWithErr pointerName $ localEnv envs
        in
          return $ (LocalAliasVal indeg (Just pointerName) pointingTo, (envs, oldRuleSet))
compileProcLit (AliasLit Nothing pointingTo) envs oldRuleSet
  = let envs = compilePointingToLit pointingTo envs in
      return $ (LocalAliasVal 0 Nothing pointingTo, (envs, oldRuleSet))

      {--|
compileProcLit (RuleLit lhs rhs) _ freeTailEnv freeHeadEnv oldRuleSet
  = do (lhsProc, (lhsEnv, lhsRuleSet)) <- compileProcLit lhs nullEnv
       (rhsProc, (rhsEnv, rhsRuleSet)) <- compileProcLit rhs nullEnv
       if not $ null lhsRuleSet
         then throwError $ IsNotFunctional pointerName 
         else 

           
  lhsProcEnv =  []
        rhsProcEnv = compileProcLit rhs nullEnv []
        

  case lookup pointerName localEnv of
      Nothing ->
        if member pointerName freeTailEnv
        then throwError $ IsNotFunctional pointerName
        else
          let freeTailEnv
                = insert pointerName 0 freeTailEnv
          in 
            compilePointingTo pointingTo localEnv freeTailEnv freeHeadEnv oldRuleSet
      Just (_, True) -> throwError $ IsNotFunctional pointerName
      Just (_, False) ->
        compilePointingTo pointingTo localEnv freeTailEnv freeHeadEnv oldRuleSet

|--}

{--|
-- procLit -> oldHeap -> oldEnv -> oldRuleSet -> oldAddrSeed
-- -> (newHeap, newEnv, newRuleSet, newAddr)
compileProcLit :: ProcLit -> Env -> Env -> RuleSet -> ThrowsError ((Env, Env), RuleSet)
compileProcLit (AliasLit (Just pointerName) pointingTo) localEnv freeEnv oldRuleSet
  = let lookedUp True = throwError $ IsNotFunctional
        lookedUp False =
          let localEnv
                = updateAssocList (mapSnd $ const True) localEnv
          in
            liftM (flip (,) oldRuleSet) $ compilePointingTo pointingTo localEnv freeEnv
    in
      liftM (lookedUp . snd) $ lookup pointerName localEnv
|--}


readExpr :: String -> String
readExpr input = case Parser.readExpr input of
    Left err -> "No match : " ++ show err
    Right val -> "Found : " ++ Parser.showBlock val

