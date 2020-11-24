module VM.Heap (
  Node (..),
  Heap,
  AtomList,
  normalizeHeap,
  getIndeg,
  toAtomList,
  hLookup,
  initialHeap,
  hAlloc,  
  ) where
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Compiler.Compiler hiding (Envs)


data Node = NAtom AtomName [Addr]
            -- ^ NAtom SymbolAtomName [Link]
          | NInd Addr
            -- ^ Indirect to Addr
          deriving(Eq, Show)

type IndegNode = (Indeg, Node)
data Heap = Heap [Addr] (M.Map Addr IndegNode)
  -- ^ Heap is consists of ...
  -- - the list of free addresses
  -- - the map from addresses to the tuples of the indegree and the node

instance Show Heap where show = showHeap

showHeap :: Heap -> String
showHeap (Heap _ mapAddr2IndegNode)
  = let list = intercalate "\n"
          $ map show
          $ M.toAscList mapAddr2IndegNode
  in
      list

  
type AtomList = [Addr]
                
type AssocList key val = [(key, val)]

-- | initial heap
initialHeap :: Heap
initialHeap = Heap [1..] M.empty

-- | translate heap to Atom List (assoc list)
toAtomList :: Heap -> AtomList
toAtomList (Heap _ mapAddr2IndegNode)
  = map fst $ M.toList mapAddr2IndegNode

-- | lookup
hLookup :: Addr -> Heap -> IndegNode
hLookup addr (Heap _ mapAddr2IndegNode) 
  = mapAddr2IndegNode M.! addr

-- | delete
hDelete :: Addr -> Heap -> Heap
hDelete addr (Heap freeAddrs mapAddr2IndegNode) 
  = Heap (addr:freeAddrs) (M.delete addr mapAddr2IndegNode)

-- | Memory allocation
hAlloc :: Heap -> IndegNode -> (Heap, Addr)
hAlloc (Heap (addr:freeAddrs) mapAddr2IndegNode) resource
  = (Heap freeAddrs (M.insert addr resource mapAddr2IndegNode), addr)

-- | lookup for the certain value
hLookupVal :: (IndegNode -> Bool) -> Heap -> Maybe (Addr, IndegNode)
hLookupVal f (Heap _ mapAddr2IndegNode)
  = let list = M.toAscList mapAddr2IndegNode
        lookupVal' ((key, val):t)
          = if f val then Just (key, val)
            else lookupVal' t
        lookupVal' [] = Nothing
    in
      lookupVal' list

-- | A heap version Map.mapWithKey 
hMapWithAddr :: (Addr -> IndegNode -> IndegNode) -> Heap -> Heap
hMapWithAddr f (Heap freeAddrs mapAddr2IndegNode)
  = Heap freeAddrs $ M.mapWithKey f mapAddr2IndegNode

getIndeg :: Addr -> Heap -> Indeg
getIndeg addr (Heap _ mapAddr2IndegNode)
  = fst $ mapAddr2IndegNode M.! addr




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
  = case hLookupVal (isNInd . snd) heap of
      Just (fromAddr, (indeg, NInd toAddr))
        -> normalizeHeap
           $ hMapWithAddr (normalizeAddrNode (fromAddr, indeg) toAddr)
           $ hDelete fromAddr heap
      Nothing -> heap
      _ -> error "should not reach here"


