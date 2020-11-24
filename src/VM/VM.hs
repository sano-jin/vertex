
module VM.VM where
import Control.Monad.Except
-- import qualified Data.Map.Strict as M
-- import qualified Data.Set as S
-- import Data.List
import Data.Bifunctor 
import Compiler.Compiler hiding (Envs)
import VM.Heap 
import VM.FindAtom
import VM.PushAtom
import VM.Envs
import GHC.Base

data State = State Heap [Rule]
-- ^ State is consists of tuple
-- - the heap,
-- - the rules.

instance Show State where show = showState


-- | shows the state
showState :: State -> String
showState (State heap rules)
  = show heap ++ "\n" ++ showRules rules
  
-- | execute rule and returns the new heap, the newly created rules and the applied rule
execRule :: Heap -> Rule -> Maybe (Heap, [Rule], Rule)
execRule heap rule@(Rule lhs rhs _ rhsRules) 
  = do envs <- findAtoms lhs heap
       return (push heap rhs envs, rhsRules, rule)

applyTillFail :: (a -> Maybe b) -> [a] -> Maybe b
applyTillFail f (h:t)
  = f h <|> applyTillFail f t
applyTillFail f [] = Nothing

-- | runs the program and returns the next state
run :: State -> Maybe (State, Rule)
run (State heap rules)
  = do (newHeap, newlyCreatedRules, appliedRule) <- applyTillFail (execRule heap) rules
       return (State newHeap (rules ++ newlyCreatedRules), appliedRule)

initializeHeap :: [ProcVal] -> Heap
initializeHeap procVals
  = push initialHeap procVals nullEnvs
 
showTransition :: Maybe (State, Rule) -> String
showTransition (Just (state, rule))
  = show state ++ "\n with a rule " ++ showRule rule
showTransition Nothing
 = "halted"  
