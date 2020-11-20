{-|
Module      : Syntax
Description : A syntax for the vertex language
Copyright   : (c) sano, 2020
License     : MIT
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module Syntax (
  showBlock,
  showProc,
  PointerLit (..),
  ProcLit (..)
  ) where
import Data.List

-- | Links are denoted as the variable starting from the capital lettes
-- or an embedded atom if the indegree of the pointing atom is 1
data PointerLit = PointerLit String                 -- X
                | AtomLit String [PointerLit]       -- p(X1,...,Xm)
                deriving(Eq, Ord, Show)

-- | A process can be
-- an Atom (aliasing from link)
-- a rule
-- or a link creation.
data ProcLit = AliasLit (Maybe String) PointerLit   -- X -> p(X1,...,Xm)
             | RuleLit [ProcLit] [ProcLit]          -- P :- P
             | CreationLit String [ProcLit]         -- \X.(P1,..,Pn)
             deriving(Eq, Ord, Show)

-- | Show the top level processes
showBlock :: [ProcLit] -> String
showBlock = intercalate ". " . map showProc

-- | Show the procLit
showProc :: ProcLit -> String
showProc (AliasLit (Just p) to) =
  showPointer (PointerLit p) ++ " -> " ++ showPointer to
showProc (AliasLit Nothing to) = showPointer to
showProc (RuleLit lhs rhs) = showProcSet lhs ++ " :- " ++ showProcSet rhs
showProc (CreationLit pointer procs)
  = "\\" ++ pointer ++ "." ++ if length procs == 1 then showProcSet procs
                              else "(" ++ showProcSet procs ++ ")"                                

-- | Show the processes on the left/right hand-side of the rules                                   
showProcSet :: [ProcLit] -> String
showProcSet = intercalate ", " . map showProc_
  where showProc_ r@(RuleLit _ _) = "(" ++ showProc r ++ ")"
        showProc_ others = showProc others

-- | Show the list of links of the atom
showPointerList :: [PointerLit] -> String
showPointerList [] = ""
showPointerList args = "(" ++ unwordsList args ++ ")"
  where unwordsList = intercalate ", " . map showPointer

-- | Show the given link or the embedded atom
showPointer :: PointerLit -> String
showPointer (PointerLit name) = name
showPointer (AtomLit name args) = name ++ showPointerList args

