module Heap where
-- import Control.Monad.Except
import qualified Data.Map.Strict as M
-- import qualified Data.Set as S
-- import Data.List
-- import Data.Bifunctor 
import Compiler 

type Arity = Int

data Node = NAtom AtomName [Addr]
            -- ^ NAtom SymbolAtomName [Pointers]
          | NInd Addr
            -- ^ Alias to Addr
          deriving(Eq)

-- | Heap is the map from addresses to the tuples of the indegree and the node
type Heap = M.Map Addr (Indeg, Node)

-- | Some functions for normalizing the heap
substituteAddr :: Addr -> Addr -> Addr -> Addr
substituteAddr fromAddr toAddr addr 
  = if addr == fromAddr then toAddr
    else addr

-- | Normalize addresses of the nodes to the given address 
normalizeNode :: Addr -> Addr -> Node -> Node
normalizeNode fromAddr toAddr (NAtom atomName links)
  = NAtom atomName $ map substitute links
  where substitute = substituteAddr fromAddr toAddr 
normalizeNode fromAddr toAddr (NInd addr)
  = NInd $ substituteAddr fromAddr toAddr addr

normalizeAddrNode :: (Addr, Indeg) -> Addr -> Addr -> (Indeg, Node) -> (Indeg, Node)
normalizeAddrNode (fromAddr, fromIndeg) toAddr addr (indeg, node)
  = let indeg' = 
          if toAddr == addr then indeg + fromIndeg - 1
          else indeg
    in
      (indeg', normalizeNode fromAddr toAddr node)
  
normalizeIfInd :: Addr -> (Indeg, Node) -> Heap -> Maybe Heap
normalizeIfInd fromAddr (indeg, NInd toAddr) oldHeap
  = Just $ M.mapWithKey (normalizeAddrNode (fromAddr, indeg) toAddr) oldHeap
normalizeIfInd _ _ _
  = Nothing

isNInd :: Node -> Bool
isNInd (NInd _) = True
isNInd _        = False

lookupVal :: Ord key => (val -> Bool) -> M.Map key val -> Maybe (key, val)
lookupVal f mapA2B
  = let list = M.toAscList mapA2B
        lookupVal' ((key, val):t)
          = if f val then Just (key, val)
            else lookupVal' t
        lookupVal' [] = Nothing
    in
      lookupVal' list

isTheLookingLocalAtom :: Indeg -> AtomName -> Arity -> (Indeg, Node) -> Bool
isTheLookingLocalAtom indeg atomName arity (indegOfNode, NAtom atomNameOfNode links)
  = indeg == indegOfNode
    && atomName == atomNameOfNode
    && arity == length links
isTheLookingLocalAtom _ _ _ _ = False

lookupLocalAtom :: Indeg -> AtomName -> Arity -> Heap -> Maybe (Addr, (Indeg, Node))
lookupLocalAtom indeg atomName arity heap
  = lookupVal (isTheLookingLocalAtom indeg atomName arity) heap

checkLocalAtom :: Indeg -> AtomName -> Arity -> Heap -> Addr -> Maybe (Addr, (Indeg, Node))
checkLocalAtom indeg atomName arity heap addr 
  = case M.lookup addr heap of
      Just tupIndegNode@(indegOfNode, (NAtom atomNameOfNode links))
        -> if isTheLookingLocalAtom indeg atomName arity tupIndegNode
           then Just (addr, tupIndegNode)
           else Nothing
      Just _ -> error "indirection traversing is not implemented yet"
      Nothing -> error "not serial"
      

normalizeHeap :: Heap -> Heap
normalizeHeap heap
  = case lookupVal (isNInd . snd) heap of
      Just (fromAddr, (indeg, NInd toAddr))
        -> normalizeHeap
           $ M.mapWithKey (normalizeAddrNode (fromAddr, indeg) toAddr)
           $ M.delete fromAddr heap
      Nothing -> heap
      _ -> error "should not reach here"
