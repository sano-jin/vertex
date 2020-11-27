{-# LANGUAGE Safe #-}

module Compiler.Process (
  Addr,
  Indeg,
  AtomName,
  LinkVal (..),
  ProcVal (..),
  Procs,
  Rule (..),
  showProcs,
  showProcVals,
  showRules,
  showSet,
  showRulesForDebugging,
  ) where
import           Data.List
import qualified Data.Set  as S

type Addr = Int
type Indeg = Int
type AtomName = String

data LinkVal = FreeLinkVal String
               -- ^ X
             | LocalLinkVal Addr
               -- ^ X
             | AtomVal String [LinkVal]
               -- ^ p(X1,...,Xm)
             deriving(Eq)

data ProcVal = LocalAliasVal Indeg Addr LinkVal
               -- ^ \X. ... X -> p(X1,...,Xm)
             | FreeAliasVal AtomName LinkVal
               -- ^ X -> p(X1,...,Xm)
             deriving(Eq)

instance Show LinkVal where show = showLinkVal
instance Show ProcVal where show = showProcVal

-- | some functions for showing processes
showSet :: S.Set String -> String
showSet stringSet = "{" ++ intercalate ", " (S.toList stringSet) ++ "}"

showProcVals :: [ProcVal] -> String
showProcVals = intercalate ", " . map show

showProcVal :: ProcVal -> String
showProcVal (LocalAliasVal indeg addr linkVal)
  = let incommingLink =
          if indeg > 0 then "L" ++ show addr ++ " -> " else "" in
      incommingLink ++ show linkVal
showProcVal (FreeAliasVal linkName linkVal)
  = linkName ++ " -> " ++ show linkVal

showLinkVal :: LinkVal -> String
showLinkVal (FreeLinkVal linkName) = linkName
showLinkVal (LocalLinkVal addr) = "L" ++ show addr
showLinkVal (AtomVal atomName links)
  = if null links then atomName
    else atomName ++ "(" ++ intercalate ", " (map show links) ++ ")"


showProcValForDebugging :: ProcVal -> String
showProcValForDebugging (LocalAliasVal indeg addr linkVal)
  = if indeg > 0
    then "L" ++ show addr ++ " -> (" ++ show indeg ++ ", " ++ show linkVal ++ ")"
    else show linkVal
showProcValForDebugging procVal = showProcVal procVal

showProcValsForDebugging :: [ProcVal] -> String
showProcValsForDebugging = intercalate ", " . map showProcValForDebugging


data Rule = Rule [ProcVal] [ProcVal] [Rule]
-- ^ A rule is specified with ...
-- - left-hand-side atoms to match,
-- - right-hand-side atoms to generate,
-- - right-hand-side rules to generate.

instance Show Rule where show = showRule

showRule :: Rule -> String
showRule (Rule lhs rhs rules)
  = let sep = if null rhs || null rules then "" else ", " in
  showProcVals lhs ++ " :- "
  ++ showProcVals rhs ++ sep
  ++ showSubRules rules

showRuleForDebugging :: Rule -> String
showRuleForDebugging (Rule lhs rhs rules)
  = let sep = if null rhs || null rules then "" else ", " in
  showProcValsForDebugging lhs ++ " :- "
  ++ showProcValsForDebugging rhs ++ sep
  ++ showSubRulesForDebugging rules

paren :: String -> String
paren str = "(" ++ str ++ ")"

showRules :: [Rule] -> String
showRules
  = concatMap ((++ ". ") . show)

showSubRules :: [Rule] -> String
showSubRules
  = intercalate ", "
    . map (paren . show)


showRulesForDebugging :: Int -> [Rule] -> String
showRulesForDebugging indentLevel
  = intercalate "\n"
    . map ((replicate indentLevel ' ' ++)
            . (++ ".")
            . showRuleForDebugging)

showSubRulesForDebugging :: [Rule] -> String
showSubRulesForDebugging
  = intercalate ", "
    . map (paren . showRuleForDebugging)



type Procs = ([ProcVal], [Rule])
-- ^ Processes are specified with atoms and rules

showProcs :: Procs -> String
showProcs (procVals, rules)
  = let dot = if null procVals || null rules then "" else ". " in
      showProcVals procVals ++ dot ++ showRules rules

