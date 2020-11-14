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

data PointerVal = PointerVal Addr                     -- Addr
                | AtomVal String [PointerVal]         -- p(X1,...,Xm)
                deriving(Eq, Ord, Show)

data ProcVal = LocalAliasVal Addr PointerVal    -- \X.Addr -> p(X1,...,Xm)
             | FreeAliasVal  Addr PointerVal    -- X.Addr -> p(X1,...,Xm)
             | RuleVal [ProcVal] [ProcVal]            -- P :- P
             deriving(Eq, Ord, Show)

type RuleSet = [(ProcVal, ProcVal)]


type ThrowsError = Either CompileError
type IOThrowsError = ExceptT CompileError IO
instance Show CompileError where show = showError


data CompileError = IsNotSerial String 
                  | IsNotFunctional String 
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
type Env = [(String, (Indeg, HasHead))]


-- seed -> (newAddr, newSeed)
newAddr :: Addr -> (Addr, Addr)    
newAddr oldAddr = (oldAddr, oldAddr + 1)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

fst4 :: (a, b, c, d) -> a
fst4 (a, _, _, _) = a

lookupEnv_ :: String -> Env -> Maybe Indeg
lookupEnv_ pointerName env
  = liftM fst $ lookup pointerName env

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
            liftM ((,) oldRuleSet) $ compilePointingTo pointingTo localEnv freeEnv
    in
      liftM (lookedUp . snd) $ lookup pointerName localEnv
    |--}  
       
readExpr :: String -> String
readExpr input = case Parser.readExpr input of
    Left err -> "No match : " ++ show err
    Right val -> "Found : " ++ Parser.showBlock val

