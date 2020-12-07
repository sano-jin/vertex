module Repl
  ( readAndRun
  ) where
import           Compiler.Compiler              ( compile )
import           Compiler.CheckProcessContext
import           Compiler.Normalize             ( normalize )
import           VM.VM                          ( State(..)
                                                , initializeHeap
                                                , reduce
                                                )


-- | runtime
run :: (State -> String) -> Int -> State -> IO ()
run state2String stepN oldState = case reduce oldState of
  Just (newState, rule) ->
    putStrLn
        (  show stepN
        ++ ": "
        ++ show rule
        ++ " ~> \n"
        ++ state2String newState
        ++ "\n"
        )
      >> run state2String (stepN + 1) newState
  Nothing -> return ()

readAndRun :: (State -> String) -> String -> IO ()
readAndRun state2String input = case checkRules =<< normalize =<< compile input of
  Left err -> putStrLn ("Error : " ++ show err)
  Right (procVals, rules) ->
    let initialState = State (initializeHeap procVals) rules
    in  putStrLn ("0: \n" ++ state2String initialState ++ "\n")
          >> run state2String 1 initialState




