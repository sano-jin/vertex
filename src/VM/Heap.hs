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

module VM.Heap
  ( Node(..)
  , Heap
  , AtomList
  , HAddr
  , normalizeHeap
  , getIndeg
  , toAtomList
  , hLookup
  , initialHeap
  , hAlloc
  , hDelete
  , hReplace
  , setIndeg
  , incrIndeg
  , showHeapForDebugging
  , heap2ProcVals
  , heap2DGraph
  , hSize
  , initTestHeap -- Should be eliminated
  ) where
import           Compiler.Process
import           Data.List
import qualified Data.Map.Strict               as M
import           Data.Tuple.Extra
import           Vis.DGraph                     ( DGraph
                                                , map2DGraph
                                                )

heap2DGraph :: Floating s => Heap -> DGraph String s
heap2DGraph (Heap _ mapHAddr2IndegNode) =
  map2DGraph $ M.map (translateNode . snd) $ M.mapKeys hAddr2Int
                                                       mapHAddr2IndegNode
 where
  translateNode (NAtom atomName links) = (atomName, map hAddr2Int links)
  translateNode (NInd link           ) = ("->", [hAddr2Int link])
  translateNode (NInt i              ) = (show i, [])


data HAddr = HAddr Int
  deriving (Eq, Ord)
-- ^ The address on the heap.
instance Show HAddr where
  show (HAddr addr) = show addr


hAddr2Int :: HAddr -> Int
hAddr2Int (HAddr addr) = addr

data Node = NAtom AtomName [HAddr]
            -- ^ NAtom SymbolAtomName [Link]
          | NInd HAddr
            -- ^ Indirect to Addr
          | NInt Integer
          deriving(Eq, Show)

type IndegNode = (Indeg, Node)
data Heap = Heap [HAddr] (M.Map HAddr IndegNode)
  -- ^ Heap is consists of ...
  -- - the list of free addresses
  -- - the map from addresses to the tuples of the indegree and the node

instance Show Heap where
  show = showHeap

heapNode2ProcVal :: HAddr -> (Indeg, Node) -> ProcVal
heapNode2ProcVal (HAddr addr) (indeg, NAtom atomName links) =
  LocalAliasVal indeg addr $ AtomVal atomName $ map LocalLinkVal $ map
    hAddr2Int
    links
heapNode2ProcVal (HAddr addr) (indeg, NInd link) =
  LocalAliasVal indeg addr $ LocalLinkVal $ hAddr2Int link
heapNode2ProcVal (HAddr addr) (indeg, NInt i) =
  LocalAliasVal indeg addr $ IntVal i

heap2ProcVals :: Heap -> [ProcVal]
heap2ProcVals (Heap _ mapHAddr2IndegNode) =
  map (uncurry heapNode2ProcVal) $ M.toAscList mapHAddr2IndegNode

hSize :: Heap -> Int
hSize (Heap _ mapHAddr2IndegNode) = M.size mapHAddr2IndegNode



showLinkedHeapNode :: Heap -> HAddr -> (Heap, String)
showLinkedHeapNode oldHeap hAddr =
  case hSafeLookup hAddr oldHeap of
    Nothing -> (oldHeap, "L" ++ show hAddr)
    Just (1, NAtom atomName links) ->
      let (newHeap, args) = mapAccumL showLinkedHeapNode (hDelete hAddr oldHeap) links
          stringArgs =
            if null args then ""
            else "(" ++ intercalate ", " args ++ ")"
          in
        (newHeap, atomName ++ stringArgs)
    Just (1, NInd link) ->
      showLinkedHeapNode (hDelete hAddr oldHeap) link
    Just (1, NInt i) ->
      (hDelete hAddr oldHeap, show i)
    Just _ -> (oldHeap, "L" ++ show hAddr)

showHeapNode :: HAddr -> Heap -> IndegNode -> (Heap, String)
showHeapNode hAddr oldHeap (indeg, NAtom atomName links) =
  let incommingLink = if indeg > 0 then "L" ++ show hAddr ++ " -> " else ""
      (newHeap, args) = mapAccumL showLinkedHeapNode (hDelete hAddr oldHeap) links
      stringArgs =
        if null args then ""
        else "(" ++ intercalate ", " args ++ ")"
  in
    (newHeap, incommingLink ++ atomName ++ stringArgs)
showHeapNode hAddr oldHeap (indeg, NInt i) =
  let incommingLink = if indeg > 0 then "L" ++ show hAddr ++ " -> " else ""
  in
    (hDelete hAddr oldHeap, incommingLink ++ show i)
showHeapNode hAddr oldHeap (indeg, node) =
  let incommingLink = if indeg > 0 then "L" ++ show hAddr ++ " -> " else ""
      (newHeap, str) = showLinkedHeapNode oldHeap hAddr
  in  (newHeap, incommingLink ++ str)


-- | (This should) pritty print the heap.
--   Not yet implemented the prity printing.
--   This just shows the node and the address of the nodes in the heap in order.
--   This function is a instance of the `show`.
showHeap :: Heap -> String
showHeap oldHeap =
  let loop (heap, strs) =
        case hFindMin heap of
          Nothing -> reverse strs
          Just (minHAddr, minIndegNode) ->
            let (newHeap, newStr) = showHeapNode minHAddr heap minIndegNode
            in  loop (newHeap, newStr : strs)
  in
    concatMap (++ ". ") $ loop (oldHeap, [])


-- | Show heap with addresses.
--   This is (mainly) used for debugging.
showHeapForDebugging :: Int -> Heap -> String
showHeapForDebugging indentLevel (Heap _ mapHAddr2IndegNode) =
  concatMap ((++ "\n") . (replicate indentLevel ' ' ++))
    . ("/* Address -> (Indeg, Node) */" :)
    . map
        (\(hAddr, (indeg, node)) ->
          "A" ++ show hAddr ++ " -> (" ++ show indeg ++ ", " ++ show node ++ ")"
        )
    . M.toAscList
    $ mapHAddr2IndegNode


type AtomList = [HAddr]
-- ^ AtomList is a list of addresses (pointers)

-- | initial heap
initialHeap :: Heap
initialHeap = Heap (map HAddr [1 ..]) M.empty

-- | translate heap to Atom List (assoc list)
toAtomList :: Heap -> AtomList
toAtomList (Heap _ mapHAddr2IndegNode) = map fst $ M.toList mapHAddr2IndegNode

-- | lookup
hLookup :: HAddr -> Heap -> IndegNode
hLookup hAddr (Heap _ mapHAddr2IndegNode) = mapHAddr2IndegNode M.! hAddr

-- | safe lookup
hSafeLookup :: HAddr -> Heap -> Maybe IndegNode
hSafeLookup hAddr (Heap _ mapHAddr2IndegNode) = M.lookup hAddr mapHAddr2IndegNode 

-- | delete
hDelete :: HAddr -> Heap -> Heap
hDelete hAddr (Heap freeAddrs mapHAddr2IndegNode) =
  Heap (hAddr : freeAddrs) (M.delete hAddr mapHAddr2IndegNode)

-- | Map.findMin
hFindMin :: Heap -> Maybe (HAddr, IndegNode)
hFindMin (Heap _ mapHAddr2IndegNode) =
  if M.null mapHAddr2IndegNode then Nothing
  else Just $ M.findMin mapHAddr2IndegNode

setIndeg :: HAddr -> Indeg -> Heap -> Heap
setIndeg hAddr indeg (Heap freeAddrs mapHAddr2IndegNode) =
  Heap freeAddrs $ M.adjust (first $ const indeg) hAddr mapHAddr2IndegNode

incrIndeg :: HAddr -> Heap -> Heap
incrIndeg hAddr (Heap freeAddrs mapHAddr2IndegNode) =
  Heap freeAddrs $ M.adjust (first (+ 1)) hAddr mapHAddr2IndegNode


-- | Memory allocation
hAlloc :: Heap -> IndegNode -> (Heap, HAddr)
hAlloc (Heap (hAddr : freeAddrs) mapHAddr2IndegNode) resource =
  (Heap freeAddrs (M.insert hAddr resource mapHAddr2IndegNode), hAddr)
hAlloc (Heap [] _) _ = error "run out of the free addresses"
  -- ^ This NEVER happen. Since the free addresses are represented with the INFINITE list.

-- | Replace resource at the given address with a certain resource
hReplace :: HAddr -> IndegNode -> Heap -> Heap
hReplace hAddr resource (Heap freeAddrs mapHAddr2IndegNode) =
  Heap freeAddrs (M.insert hAddr resource mapHAddr2IndegNode)


-- | Looksup for the certain value which satisfies the given condition.
hLookupVal :: (IndegNode -> Bool) -> Heap -> Maybe (HAddr, IndegNode)
hLookupVal f (Heap _ mapHAddr2IndegNode) =
  let lookupVal' ((key, val) : t) =
        if f val then Just (key, val) else lookupVal' t
      lookupVal' [] = Nothing
  in  lookupVal' $ M.toAscList mapHAddr2IndegNode


-- | A heap version Map.mapWithKey
hMapWithAddr :: (HAddr -> IndegNode -> IndegNode) -> Heap -> Heap
hMapWithAddr f (Heap freeAddrs mapHAddr2IndegNode) =
  Heap freeAddrs $ M.mapWithKey f mapHAddr2IndegNode


-- | Get indegree of the node at the given address
getIndeg :: HAddr -> Heap -> Indeg
getIndeg hAddr (Heap _ mapHAddr2IndegNode) = fst $ mapHAddr2IndegNode M.! hAddr




-- | Address substitution
substituteHAddr :: HAddr -> HAddr -> HAddr -> HAddr
substituteHAddr fromHAddr toHAddr hAddr =
  if hAddr == fromHAddr then toHAddr else hAddr

-- | Substitute addresses of the nodes to the given address
normalizeNode :: HAddr -> HAddr -> Node -> Node
normalizeNode fromHAddr toHAddr (NAtom atomName links) = NAtom atomName
  $ map substitute links
  where substitute = substituteHAddr fromHAddr toHAddr
normalizeNode fromHAddr toHAddr (NInd hAddr) =
  NInd $ substituteHAddr fromHAddr toHAddr hAddr
normalizeNode _ _ nInt = nInt

-- | Substitute addresses of the nodes to the given address
normalizeAddrNode
  :: (HAddr, Indeg) -> HAddr -> HAddr -> (Indeg, Node) -> (Indeg, Node)
normalizeAddrNode (fromHAddr, fromIndeg) toHAddr hAddr (oldIndeg, node) =
  let newIndeg =
        if toHAddr == hAddr then oldIndeg + fromIndeg - 1 else oldIndeg
  in  (newIndeg, normalizeNode fromHAddr toHAddr node)

isNInd :: Node -> Bool
isNInd (NInd _) = True
isNInd _        = False

-- | Normalize heap.
--   This should be the only exposed function.
--   Others should be hided.
normalizeHeap :: Heap -> Heap
normalizeHeap heap = case hLookupVal (isNInd . snd) heap of
  Just (fromHAddr, (indeg, NInd toHAddr)) ->
    normalizeHeap
      . hMapWithAddr (normalizeAddrNode (fromHAddr, indeg) toHAddr)
      . hDelete fromHAddr
      $ heap
  Nothing -> heap
  _       -> error "should not reach here"


-- | A heap for testing.
--   Should be eliminated after testing.
initTestHeap :: Heap
initTestHeap =
  let assocList =
        [ (1, NAtom "a" [HAddr 3, HAddr 1])   -- 1
        , (0, NAtom "b" [HAddr 3])            -- 2
        , (2, NAtom "c" [])                   -- 3
        , (0, NAtom "d" [])                   -- 4
        , (0, NAtom "e" [HAddr 6, HAddr 6])   -- 5
        , (2, NAtom "f" [])                   -- 6
        , (0, NAtom "g" [HAddr 8])            -- 7
        , (1, NAtom "h" [])                   -- 8
        , (1, NAtom "i" [HAddr 9])            -- 9
        ]
  in  fst $ mapAccumL hAlloc initialHeap assocList
