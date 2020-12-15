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
                                                , HAtomName(..)
                                                )
import           Control.Monad

binopInt :: Envs -> (Integer -> Integer -> a) -> LinkVal -> LinkVal -> a
binopInt envs f l r
  = case (eval envs l, eval envs r) of
      (NAtom (HInt i1) [], NAtom (HInt i2) []) ->
        f i1 i2
      _ -> error $ "unexpected value when evaluating "
           ++ show l ++ " and " ++ show r

binopIntArith :: Envs -> (Integer -> Integer -> Integer) -> LinkVal -> LinkVal -> Node
binopIntArith envs f = binopInt envs (\l r -> flip NAtom [] $ HInt $ f l r)

eval :: Envs -> LinkVal -> Node
eval envs (AtomVal (Symbol "'+'") [l, r]) = binopIntArith envs (+) l r 
eval envs (AtomVal (Symbol "'-'") [l, r]) = binopIntArith envs (-) l r 
eval envs (AtomVal (Symbol "'*'") [l, r]) = binopIntArith envs (*) l r 
eval envs (AtomVal (Symbol "'/'") [l, r]) = binopIntArith envs div l r 
eval envs (AtomVal (Symbol "mod") [l, r]) = binopIntArith envs mod l r 
eval _    (AtomVal (Int i     ) []) = NAtom (HInt i     ) []
eval _    (AtomVal (String str) []) = NAtom (HString str) []
eval envs (AtomVal (ProcessContext name _) _) = lookupPCtxName2Node name envs
eval _ linkVal = error $ "unexpected value when evaluating " ++ show linkVal


updateEnvs :: Envs -> ProcVal -> Maybe Envs
updateEnvs envs (LocalAliasVal 0 _ (AtomVal (Symbol "':='")
                                    [(AtomVal (ProcessContext name _) _), r]))
  = Just $ insertPCtxName2Node name (eval envs r) envs
updateEnvs envs (LocalAliasVal 0 _ (AtomVal (Symbol "'/='") [l, r]))
  = if eval envs l /= eval envs r then Just envs
    else Nothing
updateEnvs envs (LocalAliasVal 0 _ (AtomVal (Symbol "'='") [l, r]))
  = if eval envs l == eval envs r then Just envs
    else Nothing
updateEnvs envs (LocalAliasVal 0 _ (AtomVal (Symbol "'<'") [l, r]))
  = if binopInt envs (<) l r then Just envs
    else Nothing
updateEnvs envs (LocalAliasVal 0 _ (AtomVal (Symbol "'<='") [l, r]))
  = if binopInt envs (<=) l r then Just envs
    else Nothing
updateEnvs envs (LocalAliasVal 0 _ (AtomVal (Symbol "'>'") [l, r]))
  = if binopInt envs (>) l r then Just envs
    else Nothing
updateEnvs envs (LocalAliasVal 0 _ (AtomVal (Symbol "'>='") [l, r]))
  = if binopInt envs (>=) l r then Just envs
    else Nothing
updateEnvs _ _
  = error "should have eliminated in the compilation phase"

updateEnvsWithGuard :: [ProcVal] -> Envs -> Maybe Envs
updateEnvsWithGuard = flip $ foldM updateEnvs
