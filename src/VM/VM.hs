module VM.VM
  ( State(..)
  , reduce
  , initializeHeap
  , showStateForDebugging
  , isStateEq
  , reduceND
  , state2DGraph
--  , showTransition
  ) where
import           Compiler.Process
import           Data.Maybe
import           GHC.Base
import           VM.Envs
import           VM.FindAtom                    ( findAtoms )
import           VM.Heap
import           VM.PushAtom                    ( push )
import           Vis.DGraph
import qualified Data.Map.Strict               as M

data State = State Heap [Rule]
-- ^ State is consists of tuple, the heap and the list of rules.

instance Show State where
  show = showState

state2DGraph :: Floating s => State -> M.Map Int (DNode String s)
state2DGraph (State heap _) = heap2DGraph heap

-- | Shows the state.
--   Pritty print the heap and the rules.
showState :: State -> String
showState (State heap rules) = show heap ++ showRules rules

-- | Shows the state.
--   Print all the addresses and the nodes in the heap.
showStateForDebugging :: State -> String
showStateForDebugging (State heap rules) =
  "The heap status:\n"
    ++ showHeapForDebugging 4 heap
    ++ "Pritty printed heap:\n"
    ++ replicate 4 ' '
    ++ show heap
    ++ "\nRules:\n"
    ++ showRulesForDebugging 4 rules

-- | Execute rule and returns the new heap, the newly created rules and the applied rule.
execRule :: Heap -> Rule -> Maybe (Heap, [Rule], Rule)
execRule heap rule@(Rule lhs rhs rhsRules) = do
  envs <- findAtoms lhs heap
  return (push heap rhs envs, rhsRules, rule)


applyTillFail :: (a -> Maybe b) -> [a] -> Maybe b
applyTillFail f (h : t) = f h <|> applyTillFail f t
applyTillFail _ []      = Nothing


-- | Runs the program and returns the next state.
reduce :: State -> Maybe (State, Rule)
reduce (State heap rules) = do
  (newHeap, newlyCreatedRules, appliedRule) <- applyTillFail (execRule heap)
                                                             rules
  return
    (State (normalizeHeap newHeap) (rules ++ newlyCreatedRules), appliedRule)


-- | For the non-deterministic execution.
--   Currently, this does not check the equivalence of the rules.
--   This is surely NOT efficient at all.
isStateEq :: State -> State -> Bool
isStateEq (State heap1 _) (State heap2 _) = if hSize heap1 == hSize heap2
  then isJust $ findAtoms (heap2ProcVals heap1) heap2
  else False

-- | Runs the program and returns the all possible next states.
--   This is for the non-deterministic execution.
reduceND :: State -> [(State, Rule)]
reduceND (State oldHeap rules) =
  map
      (\(newHeap, rhsRules, appliedRule) ->
        (State (normalizeHeap newHeap) (rules ++ rhsRules), appliedRule)
      )
    . mapMaybe (execRule oldHeap)
    $ rules


initializeHeap :: [ProcVal] -> Heap
initializeHeap procVals = push initialHeap procVals nullEnvs


{--|
showTransition :: Maybe (State, Rule) -> String
showTransition (Just (state, rule))
  = show state ++ "\n with a rule " ++ showRule rule
showTransition Nothing
 = "halted"
|--}

{--|
-- | adds rules to the state
addRules2State :: [Rule] -> State -> State
addRules2State rules2Add (State heap rules)
  = State heap (rules ++ rules2Add)
|--}

