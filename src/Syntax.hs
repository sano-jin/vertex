module Syntax where

data PointerLit = PointerLit String                 -- X
                | AtomLit String [PointerLit]       -- p(X1,...,Xm)
                deriving(Eq, Ord, Show)

data ProcLit = AliasLit (Maybe String) PointerLit   -- X -> p(X1,...,Xm)
             | RuleLit [ProcLit] [ProcLit]          -- P :- P
             | CreationLit String [ProcLit]         -- \X.(P1,..,Pn)
             deriving(Eq, Ord, Show)


