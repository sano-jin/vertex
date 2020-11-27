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
{-# LANGUAGE Safe #-}

module Compiler.Syntax
  ( showBlock
  , showProc
  , LinkLit(..)
  , ProcLit(..)
  ) where
import           Data.List


-- | Links are denoted as the variable starting from the capital lettes
-- or an embedded atom if the indegree of the pointing atom is 1
data LinkLit = LinkLit String                 -- X
             | AtomLit String [LinkLit]       -- p(X1,...,Xm)

-- | A process can be
-- an Atom (aliasing from link)
-- a rule
-- or a link creation.
data ProcLit = AliasLit (Maybe String) LinkLit   -- X -> p(X1,...,Xm)
             | RuleLit [ProcLit] [ProcLit]          -- P :- P
             | CreationLit String [ProcLit]         -- \X.(P1,..,Pn)

-- | Show the top level processes
showBlock :: [ProcLit] -> String
showBlock = intercalate ". " . map showProc

-- | Show the procLit
showProc :: ProcLit -> String
showProc (AliasLit (Just p) to) = showLink (LinkLit p) ++ " -> " ++ showLink to
showProc (AliasLit Nothing to) = showLink to
showProc (RuleLit lhs rhs) = showProcSet lhs ++ " :- " ++ showProcSet rhs
showProc (CreationLit link procs) = "\\" ++ link ++ "." ++ if length procs == 1
  then showProcSet procs
  else "(" ++ showProcSet procs ++ ")"

-- | Show the processes on the left/right hand-side of the rules
showProcSet :: [ProcLit] -> String
showProcSet = intercalate ", " . map showProc_
 where
  showProc_ r@(RuleLit _ _) = "(" ++ showProc r ++ ")"
  showProc_ others          = showProc others

-- | Show the list of links of the atom
showLinkList :: [LinkLit] -> String
showLinkList []   = ""
showLinkList args = "(" ++ unwordsList args ++ ")"
  where unwordsList = intercalate ", " . map showLink

-- | Show the given link or the embedded atom
showLink :: LinkLit -> String
showLink (LinkLit name     ) = name
showLink (AtomLit name args) = name ++ showLinkList args

