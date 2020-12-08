module VM.Guard
  ( updateEnvsWithGuard
  ) where
import           Compiler.Process
import           Compiler.Syntax
-- import           Data.List
import qualified Data.Map.Strict               as M
import           VM.Envs                        ( Envs(..)
                                                , insertPCtxName2Node
                                                , lookupPCtxName2Node
                                                )
import           VM.Heap                        ( Node(..)
                                                , HAddr
                                                )

-- import           Data.Maybe                     ( catMaybes )
-- import           Data.Tuple.Extra               ( second )
import           Control.Monad

type LocalEnv = M.Map Addr HAddr
                 -- ^ A map from the addresses in the procVal and linkVal
                 --   to the addresses on the heap.

binopInt :: Envs -> (Integer -> Integer -> Integer) -> LinkVal -> LinkVal -> Node
binopInt envs f l r
  = case (eval envs l, eval envs r) of
      (NData (IntAtom i1), NData (IntAtom i2)) ->
        NData $ IntAtom $ f i1 i2
      _ -> error $ "unexpected value when evaluating "
           ++ show l ++ " and " ++ show r


eval :: Envs -> LinkVal -> Node
eval envs (AtomVal "+" [l, r]) = binopInt envs (+) l r 
eval envs (AtomVal "-" [l, r]) = binopInt envs (-) l r 
eval envs (AtomVal "*" [l, r]) = binopInt envs (*) l r 
eval envs (AtomVal "/" [l, r]) = binopInt envs div l r 
eval envs (DataVal dataAtom)   = NData dataAtom
eval envs (ProcessContextVal name _) = lookupPCtxName2Node name envs
eval _ linkVal = error $ "unexpected value when evaluating " ++ show linkVal

updateEnvs :: Envs -> ProcVal -> Maybe Envs
updateEnvs envs (LocalAliasVal 0 _ (AtomVal ":=" [(ProcessContextVal name _), r]))
  = Just $ insertPCtxName2Node name (eval envs r) envs
updateEnvs envs (LocalAliasVal 0 _ (AtomVal "/=" [l, r]))
  = if eval envs l /= eval envs r then Just envs
    else Nothing
updateEnvs envs (LocalAliasVal 0 _ (AtomVal "=" [l, r]))
  = if eval envs l == eval envs r then Just envs
    else Nothing
updateEnvs _ _
  = error "should have eliminated in the compilation phase"

updateEnvsWithGuard :: Envs -> [ProcVal] -> Maybe Envs
updateEnvsWithGuard envs procVals
  = foldM updateEnvs envs procVals
