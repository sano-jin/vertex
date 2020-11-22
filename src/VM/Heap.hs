module VM.Heap (
  Node (..),
  Heap,
  AtomList,
  normalizeHeap,
  ) where
import qualified Data.Map.Strict as M
import Compiler.Compiler hiding (Envs)

data Node = NAtom AtomName [Addr]
            -- ^ NAtom SymbolAtomName [Link]
          | NInd Addr
            -- ^ Indirect to Addr
          deriving(Eq)

type Heap = M.Map Addr (Indeg, Node)
            -- ^ Heap is the map from addresses to the tuples of the indegree and the node
type AtomList = AssocList Addr (Indeg, Node)
                -- ^ atomList can be obtained from `M.toAscList heap`
type AssocList key val = [(key, val)]

-- | Address substitution
substituteAddr :: Addr -> Addr -> Addr -> Addr
substituteAddr fromAddr toAddr addr 
  = if addr == fromAddr then toAddr
    else addr

-- | Substitute addresses of the nodes to the given address 
normalizeNode :: Addr -> Addr -> Node -> Node
normalizeNode fromAddr toAddr (NAtom atomName links)
  = NAtom atomName $ map substitute links
  where substitute = substituteAddr fromAddr toAddr 
normalizeNode fromAddr toAddr (NInd addr)
  = NInd $ substituteAddr fromAddr toAddr addr

-- | Substitute addresses of the nodes to the given address
normalizeAddrNode :: (Addr, Indeg) -> Addr -> Addr -> (Indeg, Node) -> (Indeg, Node)
normalizeAddrNode (fromAddr, fromIndeg) toAddr addr (indeg, node)
  = let indeg' = 
          if toAddr == addr then indeg + fromIndeg - 1
          else indeg
    in
      (indeg', normalizeNode fromAddr toAddr node)

isNInd :: Node -> Bool
isNInd (NInd _) = True
isNInd _        = False

-- | lookup a map for the certain value that satisfies the given function
m_lookupVal :: Ord key => (val -> Bool) -> M.Map key val -> Maybe (key, val)
m_lookupVal f mapA2B
  = let list = M.toAscList mapA2B
        lookupVal' ((key, val):t)
          = if f val then Just (key, val)
            else lookupVal' t
        lookupVal' [] = Nothing
    in
      lookupVal' list

-- | normalize heap
-- This should be the only exposed function.
-- Others should be hided
normalizeHeap :: Heap -> Heap
normalizeHeap heap
  = case m_lookupVal (isNInd . snd) heap of
      Just (fromAddr, (indeg, NInd toAddr))
        -> normalizeHeap
           $ M.mapWithKey (normalizeAddrNode (fromAddr, indeg) toAddr)
           $ M.delete fromAddr heap
      Nothing -> heap
      _ -> error "should not reach here"


