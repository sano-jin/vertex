{-# LANGUAGE Safe #-}

module Compiler.Process
  ( Addr
  , Indeg
  , AtomName
  , LinkVal(..)
  , ProcVal(..)
  , Procs
  , Rule(..)
  , showProcs
  , showProcVals
  , showRules
  , showSet
  , showRulesForDebugging
  , incrAddr
  , fromInt
  ) where
import           Data.List
import qualified Data.Set                      as S
import           Compiler.Syntax                ( AtomName(..) )

newtype Addr = Addr Int
             deriving(Eq, Ord)
instance Show Addr where
  show (Addr addr) = "L" ++ show addr

incrAddr :: Addr -> Addr
incrAddr (Addr addr) = Addr $ addr + 1

fromInt :: Int -> Addr
fromInt = Addr

type Indeg = Int
-- type AtomName = String
-- ^ These types should be replaced with the "newtype"
--   to avoid confusion (mixing use).

data LinkVal = FreeLinkVal String
               -- ^ X
             | LocalLinkVal Addr
               -- ^ X
             | AtomVal AtomName [LinkVal]
               -- ^ p(X1,...,Xm)
             deriving(Eq, Ord)

instance Show LinkVal where
  show = showLinkVal

data ProcVal = LocalAliasVal Indeg Addr LinkVal
               -- ^ \X. ... X -> p(X1,...,Xm)
             | FreeAliasVal String LinkVal
               -- ^ X -> p(X1,...,Xm)
             deriving(Eq)

instance Show ProcVal where
  show = showProcVal

-- | some functions for showing processes
showSet :: S.Set String -> String
showSet stringSet = "{" ++ intercalate ", " (S.toList stringSet) ++ "}"

showProcVals :: [ProcVal] -> String
showProcVals = intercalate ", " . map show

showProcVal :: ProcVal -> String
showProcVal (LocalAliasVal indeg addr linkVal) =
  let incommingLink = if indeg > 0 then show addr ++ " -> " else ""
  in  incommingLink ++ show linkVal
showProcVal (FreeAliasVal linkName linkVal) =
  linkName ++ " -> " ++ show linkVal

showLinkVal :: LinkVal -> String
showLinkVal (FreeLinkVal  linkName ) = linkName
showLinkVal (LocalLinkVal addr     ) = show addr
showLinkVal (AtomVal (ProcessContext name maybeType) _) =
  "$" ++ name ++ maybe "" ((":" ++) . show) maybeType
showLinkVal (AtomVal atomName links) = if null links
  then show atomName
  else show atomName ++ "(" ++ intercalate ", " (map show links) ++ ")"


-- | Show indegree if the indegree is bigger than 0.
showProcValForDebugging :: ProcVal -> String
showProcValForDebugging (LocalAliasVal indeg addr linkVal) = if indeg > 0
  then show addr ++ " -> (" ++ show indeg ++ ", " ++ show linkVal ++ ")"
  else show linkVal
showProcValForDebugging procVal = showProcVal procVal

-- | Show the ProcVal more consisely with information of indegs.
showProcValsForDebugging :: [ProcVal] -> String
showProcValsForDebugging = intercalate ", " . map showProcValForDebugging


data Rule = Rule (Maybe String) [ProcVal] [ProcVal] [ProcVal] [Rule]
-- ^ A rule is specified with ...
--   name (maybe),
--   left-hand-side atoms to match,
--   a list of guards (maybe null),
--   right-hand-side atoms to generate,
--   and right-hand-side rules to generate.

instance Show Rule where
  show = showRule

showRule :: Rule -> String
showRule (Rule (Just name) _ _ _ _) = name
showRule (Rule Nothing lhs guard rhs rules) =
  let sep      = if null rhs || null rules then "" else ", "
      guardStr = if null guard then "" else showProcVals guard ++ " | "
  in  showProcVals lhs
      ++ " :- "
      ++ guardStr
      ++ showProcVals rhs
      ++ sep
      ++ showSubRules rules

showRuleForDebugging :: Rule -> String
showRuleForDebugging (Rule maybeName lhs guard rhs rules) =
  let
    sep  = if null rhs || null rules then "" else ", "
    name = maybe "" (++ " @@ ") maybeName
    guardStr =
      if null guard then "" else showProcValsForDebugging guard ++ " | "
  in
    name
    ++ showProcValsForDebugging lhs
    ++ " :- "
    ++ guardStr
    ++ showProcValsForDebugging rhs
    ++ sep
    ++ showSubRulesForDebugging rules

paren :: String -> String
paren str = "(" ++ str ++ ")"

showRules :: [Rule] -> String
showRules = concatMap ((++ ". ") . show)

showSubRules :: [Rule] -> String
showSubRules = intercalate ", " . map (paren . show)


showRulesForDebugging :: Int -> [Rule] -> String
showRulesForDebugging indentLevel = intercalate "\n"
  . map ((replicate indentLevel ' ' ++) . (++ ".") . showRuleForDebugging)

showSubRulesForDebugging :: [Rule] -> String
showSubRulesForDebugging =
  intercalate ", " . map (paren . showRuleForDebugging)

type Procs = ([ProcVal], [Rule])
-- ^ Processes are specified with atoms and rules

showProcs :: Procs -> String
showProcs (procVals, rules) =
  let dot = if null procVals || null rules then "" else ". "
  in  showProcVals procVals ++ dot ++ showRules rules
  
