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
import           Data.Tuple.Extra
import           GHC.Base
import           VM.Envs
import           VM.FindAtom                    ( findAtoms )
import           VM.Guard                       ( updateEnvsWithGuard )
import           VM.Heap
import           VM.PushAtom                    ( push )
import           Vis.DGraph                     ( DGraph )
import           Data.List


data State = State Heap [Rule]
-- ^ State is consists of tuple, the heap and the list of rules.

instance Show State where
  show = showState

state2DGraph :: Floating s => State -> DGraph String s
state2DGraph (State heap _) = heap2DGraph heap

-- | Shows the state.
--   Pritty print the heap and the rules.
showState :: State -> String
showState (State heap _) = show heap
  -- ++ showRules rules

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

-- | Execute rule and returns the new heap,
--   the newly created rules and the applied rule.
execRule :: Heap -> Rule -> [(Heap, [Rule], Rule)]
execRule heap rule@(Rule _ lhs guard rhs rhsRules) =
  let envsList = findAtoms lhs heap
      newEnvsList = mapMaybe (updateEnvsWithGuard guard) envsList 
  in
    map
    (\newEnvs -> (push heap rhs newEnvs, rhsRules, rule))
    newEnvsList


applyTillFail :: (a -> Maybe b) -> [a] -> Maybe b
applyTillFail f (h : t) = f h <|> applyTillFail f t
applyTillFail _ []      = Nothing


-- | Runs the program and returns the next state.
reduce :: State -> [(State, Rule)]
reduce (State heap rules) =
  map
  (\(newHeap, newlyCreatedRules, appliedRule)
    -> (State (normalizeHeap newHeap) (rules ++ newlyCreatedRules), appliedRule)
  )
  $ concatMap (execRule heap) rules


-- | For the non-deterministic execution.
--   Currently, this does not check the equivalence of the rules.
--   This is surely NOT efficient at all.
isStateEq :: State -> State -> Bool
isStateEq (State heap1 _) (State heap2 _) =
  (hSize heap1 == hSize heap2) && (not $ null $ findAtoms (heap2ProcVals heap1) heap2)

-- | Runs the program and returns the all possible next states.
--   This is for the non-deterministic execution.
reduceND :: State -> [(State, Rule)]
reduceND (State oldHeap rules) =
  nubBy (\(s1, _) (s2, _) -> isStateEq s1 s2)
  $ map
  (\(newHeap, rhsRules, appliedRule) ->
      (State (normalizeHeap newHeap) (rules ++ rhsRules), appliedRule)
  )
  $ concatMap (execRule oldHeap) rules


initializeHeap :: [ProcVal] -> Heap
initializeHeap procVals = push initialHeap procVals nullEnvs



