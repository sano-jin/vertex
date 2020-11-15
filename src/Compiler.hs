module Compiler where
import System.Environment
import Data.IORef
import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import qualified Parser (
  readExpr,
  showBlock,
  ParseError,
  SourcePos
  )
import Syntax

type Addr = Int
type Indeg = Int
data Node = NAtom String [Addr]         -- NAtom SymbolAtomName [Pointers]
          | NInd Addr                   -- Alias to Addr
          | NRule [ProcVal] [ProcVal]   -- Rule

type Heap = [(Addr, Indeg, Node)]  -- [(Address, Indegree, Atom)]


data PointerVal = FreePointerVal String
                | LocalPointerVal Indeg Addr     -- X
                | AtomVal String [PointerLit]    -- p(X1,...,Xm)
                deriving(Eq, Ord, Show)


data ProcVal = LocalAliasVal Indeg Addr PointerVal   -- \X.X -> p(X1,...,Xm)
             | FreeAliasVal String PointerVal        -- X -> p(X1,...,Xm)
             | RuleVal [ProcVal] [ProcVal]           -- P :- P
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
type EnvList = [(String, (Addr, Indeg, HasHead))]
type EnvSet  = S.Set String

lookupAssocListWithErr :: Eq key => key -> [(key, value)] -> value
lookupAssocListWithErr key ((k, v):t)
  = if key == k then v
    else lookupAssocListWithErr key t

mapSnd :: (b -> b) -> (a, b) -> (a, b)
mapSnd f (a, b) = (a, f b)

-- type Envs = (EnvList, EnvSet, EnvSet)

data Envs = Envs { localEnv :: EnvList
                 , freeTailEnv :: EnvSet
                 , freeHeadEnv :: EnvSet
                 , addrSeed :: Int
                 } deriving (Show)

nullEnv = Envs { localEnv = []
              , freeTailEnv = S.empty
              , freeHeadEnv = S.empty
              , addrSeed = 0
              }

incrAddrSeed envs = envs { addrSeed = addrSeed envs + 1 }

mapAccumLM :: Monad m => (a -> b -> m (a, c)) -> a -> [b] -> m (a, [c])
mapAccumLM f a (b:bs) =
  do (a, c) <- f a b
     (a, cs) <- mapAccumLM f a bs
     return (a, c:cs)
  


compilePointingToLit ::
  PointerLit -> Envs -> (Envs, PointerVal)
compilePointingToLit pointingTo envs
  = (envs, FreePointerVal "X")

compileProcLit :: Envs -> ProcLit -> ThrowsError (Envs, [ProcVal])
compileProcLit envs (AliasLit (Just pointerName) pointingTo) 
  = case lookup pointerName $ localEnv envs of
      Nothing ->
        if S.member pointerName $ freeTailEnv envs
        then throwError $ IsNotFunctional pointerName
        else
          let (envs, pointingToVal)
                = compilePointingToLit pointingTo
                  $ envs { freeTailEnv = S.insert pointerName $ freeTailEnv envs }
          in
            return $ (envs, [FreeAliasVal pointerName pointingToVal])
      Just (_, _, True) -> throwError $ IsNotFunctional pointerName
      Just (addr, _, False) ->
        let (envs, pointingToVal) = compilePointingToLit pointingTo envs in
          return (envs, [LocalAliasVal 0 addr pointingToVal])
compileProcLit envs (AliasLit Nothing pointingTo) 
  = let (envs, pointingToVal) = compilePointingToLit pointingTo envs in
      return $ (incrAddrSeed envs, [LocalAliasVal 0 (addrSeed envs) pointingToVal])

compileProcLit envs (RuleLit lhs rhs) 
  = do (lhsEnv, lhsProc) <- mapAccumLM compileProcLit nullEnv lhs
       (rhsEnv, rhsProc) <- mapAccumLM compileProcLit nullEnv rhs
       return $ (envs, [RuleVal (concat lhsProc) (concat rhsProc)])


       {--|
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

