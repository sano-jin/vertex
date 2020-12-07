module CompilerSpec where
-- | A Module for testing
--   give as `stack test --test-arguments "sample.dhl"`
--   stack ghc Syntax.hs Process.hs Compiler.hs Envs.hs Parser.hs CheckGuard.hs Normalize.hs SpecCompiler.hs -- -Wall

import           System.Environment
import           Compiler.Compiler
import           Compiler.Normalize
import           Compiler.CheckGuard
import           Compiler.Process

main :: IO ()
main = do [f] <- getArgs
          s   <- readFile f
          case checkRules =<< normalize =<< compile s of
            Left err -> putStrLn $ show err
            Right procs ->
              putStrLn $ showProcs procs





