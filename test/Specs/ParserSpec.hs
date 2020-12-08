module ParserSpec where
-- | A Module for testing
--   give as `stack test --test-arguments "sample.dhl"`

import           System.Environment
import           Compiler.Parser
import           Compiler.Syntax

main :: IO ()
main = do [f] <- getArgs
          s   <- readFile f
          case readExpr s of
            Left err -> putStrLn $ show err
            Right procLits -> putStrLn $ showBlock procLits

