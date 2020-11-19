module Syntax (
  showBlock,
  showProc,
  PointerLit (..),
  ProcLit (..)
  ) where
import Data.List

data PointerLit = PointerLit String                 -- X
                | AtomLit String [PointerLit]       -- p(X1,...,Xm)
                deriving(Eq, Ord, Show)

data ProcLit = AliasLit (Maybe String) PointerLit   -- X -> p(X1,...,Xm)
             | RuleLit [ProcLit] [ProcLit]          -- P :- P
             | CreationLit String [ProcLit]         -- \X.(P1,..,Pn)
             deriving(Eq, Ord, Show)

-- show
showBlock :: [ProcLit] -> String
showBlock = intercalate ". " . map showProc

showProc :: ProcLit -> String
showProc (AliasLit (Just p) to) =
  showPointer (PointerLit p) ++ " -> " ++ showPointer to
showProc (AliasLit Nothing to) = showPointer to
showProc (RuleLit lhs rhs) = showProcSet lhs ++ " :- " ++ showProcSet rhs
showProc (CreationLit pointer procs)
  = "\\" ++ pointer ++ "." ++ if length procs == 1 then showProcSet procs
                              else "(" ++ showProcSet procs ++ ")"                                
                                  
showProcSet :: [ProcLit] -> String
showProcSet = intercalate ", " . map showProc_
  where showProc_ r@(RuleLit _ _) = "(" ++ showProc r ++ ")"
        showProc_ others = showProc others

showPointerList :: [PointerLit] -> String
showPointerList [] = ""
showPointerList args = "(" ++ unwordsList args ++ ")"
  where unwordsList = intercalate ", " . map showPointer

showPointer :: PointerLit -> String
showPointer (PointerLit name) = name
showPointer (AtomLit name args) = name ++ showPointerList args

