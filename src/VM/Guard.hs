module VM.Guard
  ( updateEnvsWithGuard
  ) where
import           Compiler.Process
import           Compiler.Syntax
import           VM.Envs                        ( Envs(..)
                                                , insertPCtxName2Node
                                                , lookupPCtxName2Node
                                                )
import           VM.Heap                        ( Node(..)
                                                )
import           Control.Monad

binopInt :: Envs -> (Integer -> Integer -> a) -> LinkVal -> LinkVal -> a
binopInt envs f l r
  = case (eval envs l, eval envs r) of
      (NData (IntAtom i1), NData (IntAtom i2)) ->
        f i1 i2
      _ -> error $ "unexpected value when evaluating "
           ++ show l ++ " and " ++ show r

binopIntArith :: Envs -> (Integer -> Integer -> Integer) -> LinkVal -> LinkVal -> Node
binopIntArith envs f = binopInt envs (\l r -> NData $ IntAtom $ f l r)

eval :: Envs -> LinkVal -> Node
eval envs (AtomVal "+" [l, r]) = binopIntArith envs (+) l r 
eval envs (AtomVal "-" [l, r]) = binopIntArith envs (-) l r 
eval envs (AtomVal "*" [l, r]) = binopIntArith envs (*) l r 
eval envs (AtomVal "/" [l, r]) = binopIntArith envs div l r 
eval _    (DataVal dataAtom)   = NData dataAtom
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
updateEnvs envs (LocalAliasVal 0 _ (AtomVal "<" [l, r]))
  = if binopInt envs (<) l r then Just envs
    else Nothing
updateEnvs envs (LocalAliasVal 0 _ (AtomVal "<=" [l, r]))
  = if binopInt envs (<=) l r then Just envs
    else Nothing
updateEnvs envs (LocalAliasVal 0 _ (AtomVal ">" [l, r]))
  = if binopInt envs (>) l r then Just envs
    else Nothing
updateEnvs envs (LocalAliasVal 0 _ (AtomVal ">=" [l, r]))
  = if binopInt envs (>=) l r then Just envs
    else Nothing
updateEnvs _ _
  = error "should have eliminated in the compilation phase"

updateEnvsWithGuard :: [ProcVal] -> Envs -> Maybe Envs
updateEnvsWithGuard = flip $ foldM updateEnvs
