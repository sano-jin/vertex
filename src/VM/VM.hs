module VM.VM (
  State (..),
  reduce,
  initializeHeap,
  showStateForDebugging,
--   showTransition,
  ) where
import Compiler.Process
import VM.Heap 
import VM.FindAtom (
  findAtoms
  ) 
import VM.PushAtom (
  push
  )
import VM.Envs
import GHC.Base


data State = State Heap [Rule]
-- ^ State is consists of tuple, the heap and the list of rules.

instance Show State where show = showState


-- | Shows the state
-- Pritty print the heap and the rules
showState :: State -> String
showState (State heap rules)
  = show heap ++ showRules rules
  
-- | Shows the state
-- Print all the addresses and the nodes in the heap.
showStateForDebugging :: State -> String
showStateForDebugging (State heap rules)
  = "The heap status:\n"
    ++ showHeapForDebugging 4 heap
    ++ "Pritty printed heap:\n"
    ++ replicate 4 ' ' ++ show heap
    ++ "\nRules:\n"
    ++ showRulesForDebugging 4 rules
  
-- | execute rule and returns the new heap, the newly created rules and the applied rule
execRule :: Heap -> Rule -> Maybe (Heap, [Rule], Rule)
execRule heap rule@(Rule lhs rhs rhsRules) 
  = do envs <- findAtoms lhs heap
       return (push heap rhs envs, rhsRules, rule)

applyTillFail :: (a -> Maybe b) -> [a] -> Maybe b
applyTillFail f (h:t)
  = f h <|> applyTillFail f t
applyTillFail _ [] = Nothing

-- | runs the program and returns the next state
reduce :: State -> Maybe (State, Rule)
reduce (State heap rules)
  = do (newHeap, newlyCreatedRules, appliedRule) <- applyTillFail (execRule heap) rules
       return (State (normalizeHeap newHeap) (rules ++ newlyCreatedRules), appliedRule)

initializeHeap :: [ProcVal] -> Heap
initializeHeap procVals
  = push initialHeap procVals nullEnvs

{--| 
showTransition :: Maybe (State, Rule) -> String
showTransition (Just (state, rule))
  = show state ++ "\n with a rule " ++ showRule rule
showTransition Nothing
 = "halted"  
|--}
