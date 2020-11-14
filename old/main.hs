module Main where
import System.Environment
import Data.IORef
import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Parser (
  readExpr,
  showBlock,
  PointerLit (..),
  ProcLit (..),
  ParseError,
  SourcePos
  )

type Env = IORef (M.Map String (IORef (Int, Maybe Parser.PointerLit)))

data CompileError = IsNotSerial String Parser.SourcePos
                  | IsNotFunctional String Parser.SourcePos Parser.SourcePos
                  | RedundantAliasing String String
                  | Free2FreeAliasingOnLHS String String
                  | Parser Parser.ParseError

-- show
showError :: CompileError -> String
showError (IsNotSerial name pos) =
  "pointer '" ++ name ++ "' at " ++ show pos ++ " is not serial.\n"
showError (IsNotFunctional name pos1 pos2) =
  "pointer '" ++ name ++ "' at " ++ show pos1 ++ " is not functional.\n"
  ++ "It is already mapped to an atom at " ++ show pos2
showError (RedundantAliasing x y) = "aliasing from '" ++ x ++ "' to '" ++ y ++ "' is redundant"
showError (Free2FreeAliasingOnLHS x y) = "aliasing from free tail pointer '"
                                         ++ x ++ "' to free head pointer'" ++ y ++ "'"
showError (Parser parseError) = "Parse error at " ++ show parseError
instance Show CompileError where show = showError

type ThrowsError = Either CompileError
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val


nullEnv :: IO Env
nullEnv = newIORef M.empty

nthMaybe :: [a] -> Int -> a
nthMaybe (x:xs) n
  | n == 0    = Just x
  | n > 0     = nthMaybe xs
  | otherwise = Nothing
nthMaybe [] _ = Nothing

mapAtNth :: Int -> (Maybe a -> a) -> [a] -> [a]
mapAtNth n f l@(x:xs)
  | n == 0    = (f $ Just x):xs
  | n > 0     = x:(mapAtNth (n - 1) f xs)
  | otherwise = l
mapAtNth n f []
  | n == 0    = [f Nothing]
  | n > 0     = (f Nothing):(mapAtNth (n - 1) f [])
  | otherwise = []
  

data ProcVal = AliasVal Int (Maybe (Parser.SourcePos, Int, String)) PointerVal 
             | RuleVal Molecule Molecule
             | MoleculeVal Molecule

data PointerVal = PointerVal Parser.SourcePos Int String
                | Path [(Int)]
                | AtomVal String [PointerVal] (S.Set (Parser.SourcePos, Int, String))

data Molecule =
  Molecule [ProcVal] [M.Map String ([Int], Parser.SourcePos)] (S.Set (Parser.SourcePos, Int, String))
  
{-
data PointerLit = PointerLit SourcePos Int String
                | AtomLit String [PointerLit]
                deriving(Eq, Ord, Show)

data ProcLit = AliasLit (Maybe (SourcePos, Int, String)) PointerLit
             | RuleLit [ProcLit] [ProcLit]
             | MoleculeLit [ProcLit]
             deriving(Eq, Ord, Show)
-}

get_pointer_set :: PointerVal -> S.Set (Parser.SourcePos, Int, String)
get_pointer_set (AtomVal name args pointer_set) = pointer_set
get_pointer_set (PointerVal pos index name) = S.singleton (pos, index, name)
get_pointer_set (Path _) = S.empty -- never reaches here

analizePointer :: Parser.PointerLit -> PointerVal
analizePointer (Parser.PointerLit pos index name) = PointerVal pos index name
analizePointer (Parser.AtomLit name args) = AtomVal name args' pointer_set
  where args' = map analizePointer args
        pointer_set = S.unions $ map get_pointer_set args'

insert :: String -> ([Int], Parser.SourcePos) -> M.Map String [(Int, Parser.Position)] -> ThrowError (M.Map String [(Int, Parser.Position)])
insert name (path, pos1) m = case M.lookup key of
    Just (_, pos2) -> throwError $ IsNotFunctional name pos1 pos2
    Nothing -> return $ M.insert name (path, pos1) m

insertNth :: Int -> String -> ([Int], SourcePos) -> ThrowError [M.Map String ([Int], Parser.SourcePos)]
insertNth index name (path, pos) = do
  
  liftM mapAtNth index (insert name (path, pos)) M.empty


get_pointer_map :: [Int] -> ProcVal -> ThrowError [M.Map String ([Int], Parser.SourcePos)]
get_pointer_map path (AliasVal _ (Just (_, index, name)) _) =
  
  if nthMaybe member 
  get_pointer_map _    (AliasVal _ _ _) = \_ -> Nothing
get_pointer_map _    (MoleculeVal (Molecule _ pointer_map _)) =
  \(i, n) -> if i > 0 then pointer_map (i - 1, n) else Nothing
get_pointer_map _    (RuleVal _ _) = \_ -> Nothing

-- analizeProc :: [Int] -> Parser.ProcLit -> ThrowError ProcVal
-- analizeProc path (Parser.AliasLit from to) = return $ AliasVal 0 from to
-- analizeProc path (Parser.MoleculeLit molecule) = return $  AliasVal 0 from to



      

readExpr :: String -> String
readExpr input = case Parser.readExpr input of
    Left err -> "No match : " ++ show err
    Right val -> "Found : " ++ Parser.showBlock val



main :: IO()
main = do
  args <- getArgs
  putStrLn $ readExpr $ head args 
