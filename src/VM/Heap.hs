{-# LANGUAGE Safe #-}

{-|
Module      : Heap
Description : A heap of the processes
Copyright   : (c) sano, 2020
License     : MIT
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

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
  hDelete,
  hReplace,
  setIndeg,
  incrIndeg,
  showHeapForDebugging,
  isHeapNull,
  ) where
import qualified Data.Map.Strict as M
import Data.List
import Compiler.Process 
import Data.Tuple.Extra

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

isHeapNull :: Heap -> Bool
isHeapNull (Heap _ mapAddr2Node) = null mapAddr2Node

showHeapNode :: Addr -> IndegNode -> String
showHeapNode addr (indeg, NAtom atomName links)
  = let incommingLink =
          if indeg > 0 then "L" ++ show addr ++ " -> " else ""
        args =
          if length links > 0
          then "(" ++ (intercalate ", " $ map (\link -> "L" ++ show link) links) ++ ")"
          else ""
    in
      incommingLink ++ atomName ++ args
showHeapNode addr (indeg, NInd link)
  = "L" ++ show addr ++ " -> L" ++ show link

-- | (This should) pritty print the heap.
-- Not yet implemented the prity printing. 
-- This just shows the node and the address of the nodes in the heap in order.
-- This function is a instance of the `show`
showHeap :: Heap -> String
showHeap (Heap _ mapAddr2IndegNode)
  = concat
    . map (++ ". ")
    . map (uncurry showHeapNode)
    . M.toAscList
    $ mapAddr2IndegNode

  
-- | Show heap with addresses.
-- This is (mainly) used for debugging.
showHeapForDebugging :: Int -> Heap -> String
showHeapForDebugging indentLevel (Heap _ mapAddr2IndegNode)
  = concat
    . map (++ "\n")
    . map (replicate indentLevel ' ' ++ )
    . ("/* Address -> (Indeg, Node) */" :)
    . map (\(addr, (indeg, node))
            -> "A" ++ show addr ++ " -> (" ++ show indeg ++ ", " ++ show node ++ ")")
    . M.toAscList
    $ mapAddr2IndegNode


  
type AtomList = [Addr]
-- ^ AtomList is a list of addresses (pointers) 
                
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

setIndeg :: Addr -> Indeg -> Heap -> Heap
setIndeg addr indeg (Heap freeAddrs mapAddr2IndegNode) 
 = Heap freeAddrs
   $ M.adjust (first $ const indeg) addr mapAddr2IndegNode

incrIndeg :: Addr -> Heap -> Heap
incrIndeg addr (Heap freeAddrs mapAddr2IndegNode) 
 = Heap freeAddrs
   $ M.adjust (first (+ 1)) addr mapAddr2IndegNode


-- | Memory allocation
hAlloc :: Heap -> IndegNode -> (Heap, Addr)
hAlloc (Heap (addr:freeAddrs) mapAddr2IndegNode) resource
  = (Heap freeAddrs (M.insert addr resource mapAddr2IndegNode), addr)
hAlloc (Heap [] _) _ = error "run out of the free addresses"


-- | Replace resource at the given address with a certain resource
hReplace :: Addr -> IndegNode -> Heap -> Heap
hReplace addr resource (Heap freeAddrs mapAddr2IndegNode) 
  = Heap freeAddrs (M.insert addr resource mapAddr2IndegNode)


-- | lookup for the certain value
hLookupVal :: (IndegNode -> Bool) -> Heap -> Maybe (Addr, IndegNode)
hLookupVal f (Heap _ mapAddr2IndegNode)
  = let lookupVal' ((key, val):t)
          = if f val then Just (key, val)
            else lookupVal' t
        lookupVal' [] = Nothing
    in
      lookupVal' $ M.toAscList mapAddr2IndegNode

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

-- | normalize heap
-- This should be the only exposed function.
-- Others should be hided
normalizeHeap :: Heap -> Heap
normalizeHeap heap
  = case hLookupVal (isNInd . snd) heap of
      Just (fromAddr, (indeg, NInd toAddr))
        -> normalizeHeap
           . hMapWithAddr (normalizeAddrNode (fromAddr, indeg) toAddr)
           . hDelete fromAddr
           $ heap
      Nothing -> heap
      _ -> error "should not reach here"

-- | A heap for testing.
-- Should be eliminated after testing.
initTestHeap :: Heap
initTestHeap =
  let assocList
        = [ (1, NAtom "a" [3, 1])   -- 1
          , (0, NAtom "b" [3]   )   -- 2
          , (2, NAtom "c" []    )   -- 3
          , (0, NAtom "d" []    )   -- 4          
          , (0, NAtom "e" [6, 6])   -- 5
          , (2, NAtom "f" []    )   -- 6
          , (0, NAtom "g" [8]   )   -- 7
          , (1, NAtom "h" []    )   -- 8
          , (1, NAtom "i" [9]    )  -- 9
          ]
  in
    fst $ mapAccumL hAlloc initialHeap assocList

