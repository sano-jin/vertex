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

data CompileError = IsNotSerial String
                  | IsNotFunctional String
                  | RedundantAliasing String String
                  | Free2FreeAliasingOnLHS String String
                  | Parser Parser.ParseError

-- show
showError :: CompileError -> String
showError (IsNotSerial pointer) = "pointer '" ++ pointer ++ "' is not serial"
showError (IsNotFunctional pointer) = "pointer '" ++ pointer ++ "' is not functional"
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

data ProcVal = AliasVal Int (Maybe (Parser.SourcePos, Int, String)) PointerVal 
             | RuleVal Molecule Molecule
             | MoleculeVal Molecule
             deriving(Eq, Ord, Show)

data PointerVal = PointerVal Parser.SourcePos Int String
                | Path [Int]
                | AtomVal String [PointerVal] (S.Set (Parser.SourcePos, Int, String)) Int
                deriving(Eq, Ord, Show)

data Molecule = Molecule [ProcVal] (M.Map (Int, String) [Int]) (S.Set (Parser.SourcePos, Int, String)) Int
              deriving(Eq, Ord, Show)




      

readExpr :: String -> String
readExpr input = case Parser.readExpr input of
    Left err -> "No match : " ++ show err
    Right val -> "Found : " ++ Parser.showBlock val



main :: IO()
main = do
  args <- getArgs
  putStrLn $ readExpr $ head args 
