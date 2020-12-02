module Repl where
-- import System.Environment
import           Compiler.Compiler              ( compile )
import           Compiler.Normalize             ( normalize )
import           Compiler.Process               ( Rule )
import           Data.List
import           Data.Maybe
import           System.IO
import           VM.VM                          ( State(..)
                                                , initializeHeap
                                                , isStateEq
                                                , reduce
                                                , reduceND
                                                )
-- import Util.Util
import           Control.Monad
import           Data.Tuple.Extra


-- | runtime
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

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
readAndRun state2String input = case normalize =<< compile input of
  Left err -> putStrLn ("Error : " ++ show err)
  Right (procVals, rules) ->
    let initialState = State (initializeHeap procVals) rules
    in  putStrLn ("0: \n" ++ state2String initialState ++ "\n")
          >> run state2String 1 initialState

readAndRunND :: (State -> String) -> String -> IO ()
readAndRunND state2String input = case normalize =<< compile input of
  Left err -> putStrLn ("Error : " ++ show err)
  Right (procVals, rules) ->
    let initialState = State (initializeHeap procVals) rules
    in  putStrLn ("0: " ++ state2String initialState)
          >>  runND state2String 0 (initialPath initialState) initialState
          >>= putStrLn
          .   showEnds state2String


data Path = Path Int [(Int, State)] [((Int, Int), Rule)]
-- ^ The arguments are
-- the number of the states,
-- the list of the state id and the states
-- and the list of the tuples : [(
-- (the id of the before state, the id of the consequent state)
-- , the rule that was applied to reduce
-- )]

-- | The initial path
initialPath :: State -> Path
initialPath state = Path 1 [(0, state)] []


addState2Path
  :: Int -> Path -> (State, Rule) -> (Path, Maybe (Int, (State, Rule)))
addState2Path oldStateId (Path stateN states transitions) (newState, appliedRule)
  = case find (isStateEq newState . snd) states of
    Nothing ->
      ( Path (stateN + 1)
             ((stateN, newState) : states)
             (((oldStateId, stateN), appliedRule) : transitions)
      , Just (stateN, (newState, appliedRule))
      )
    Just (matchedStateId, _) ->
      ( Path stateN
             states
             (((oldStateId, matchedStateId), appliedRule) : transitions)
      , Nothing
      )

{--|
printTransitionsND :: (State -> String) -> Int -> (State, Rule) -> IO ()
printTransitionsND state2String stateN (newState, rule)
  = putStrLn
    $ show stateN ++ ": "
      ++ show rule ++ " ~> \n"
      ++ state2String newState ++ "\n"
|--}

showEnds :: (State -> String) -> Path -> String
showEnds state2String (Path stateN states transitions) =
  "\nEnded with state number: "
    ++ show stateN
    ++ ".\n"
    ++ "\nstates:\n"
    ++ unlines
         (reverse $ map
           (\(stateId, state) -> show stateId ++ ": " ++ state2String state)
           states
         )
    ++ "\ntransitions: "
    ++ show (length transitions)
    ++ "\n"
    ++ unlines
         ((reverse . map
            (\((prev, next), rule) ->
              show prev
                ++ " ~> "
                ++ show next
                ++ " with a rule \""
                ++ show rule
                ++ "\"."
            )
          )
           transitions
         )


runND :: (State -> String) -> Int -> Path -> State -> IO Path
runND state2String oldStateID oldPath oldState =
  let (path, transitions) = second catMaybes
        $ mapAccumL (addState2Path oldStateID) oldPath (reduceND oldState)
  in  mapM_
          ( putStrLn
          . (\(stateId, (state, _)) ->
              show stateId ++ ": " ++ state2String state
            )
          )
          transitions
        >> if null transitions
             then return path
             else
               let runND' path (oldStateId, (state, _)) =
                     runND state2String oldStateId path state
               in  foldM runND' path transitions




{--|

readAndRunND:: (State -> String) -> String -> IO ()
readAndRunND state2String input
  = case normalize =<< compile input of
    Left err -> putStrLn ("Error : " ++ show err)
    Right (procVals, rules) ->
      let initialState = State (initializeHeap procVals) rules in
        putStrLn ("0: \n" ++ state2String initialState ++ "\n")
        >> run state2String 1 initialState
|--}


{--|
main :: IO ()
main = do
  args <- getArgs
  putStrLn $ readExpr $ head args
|--}
