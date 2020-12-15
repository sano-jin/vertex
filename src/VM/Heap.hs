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
  , HAtomName(..)
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
  , hAtomName2AtomName
  , atomName2HAtomName
  ) where
import           Compiler.Process
import           Compiler.Syntax               ( AtomName(..) )
import           Data.List
import qualified Data.Map.Strict               as M
import           Data.Tuple.Extra
import           Vis.DGraph                     ( DGraph
                                                , map2DGraph
                                                )


heap2DGraph :: Floating s => Heap -> DGraph String s
heap2DGraph (Heap _ mapHAddr2IndegNode) =
  map2DGraph
  $ M.map (translateNode . snd)
  $ M.mapKeys hAddr2Int mapHAddr2IndegNode
 where
  translateNode (NAtom atomName links) = (show atomName     , map hAddr2Int links)
  translateNode (NInd  link          ) = ("->"         , [hAddr2Int link]   )


newtype HAddr = HAddr Int
  deriving (Eq, Ord)
-- ^ The address on the heap.
instance Show HAddr where
  show (HAddr addr) = show addr

hAddr2Addr :: HAddr -> Addr
hAddr2Addr = fromInt . hAddr2Int

hAddr2Int :: HAddr -> Int
hAddr2Int (HAddr addr) = addr

data Node = NAtom HAtomName [HAddr]
            -- ^ NAtom SymbolAtomName [Link]
            -- ^ Data (int, string)
          | NInd HAddr
            -- ^ Indirect to Addr
          deriving(Eq, Show)

data HAtomName = HInt Integer
               | HString String
               | HSymbol String
              deriving (Eq, Ord)

instance Show HAtomName where
  show (HInt    i   ) = show i
  show (HString str ) = show str
  show (HSymbol name) = name

hAtomName2AtomName :: HAtomName -> AtomName
hAtomName2AtomName (HInt    i   ) = Int i
hAtomName2AtomName (HString str ) = String str
hAtomName2AtomName (HSymbol name) = Symbol name

atomName2HAtomName :: AtomName -> HAtomName
atomName2HAtomName (Int    i   ) = HInt i
atomName2HAtomName (String str ) = HString str
atomName2HAtomName (Symbol name) = HSymbol name

type IndegNode = (Indeg, Node)
data Heap = Heap [HAddr] (M.Map HAddr IndegNode)
  -- ^ Heap is consists of ...
  --   the list of free addresses and
  --   the map from addresses to the tuples of the indegree and the node

instance Show Heap where
  show = showHeap

heapNode2ProcVal :: HAddr -> (Indeg, Node) -> ProcVal
heapNode2ProcVal hAddr (indeg, NAtom hAtomName links) =
  LocalAliasVal indeg (hAddr2Addr hAddr)
  $ AtomVal (hAtomName2AtomName hAtomName)
  $ map (LocalLinkVal . hAddr2Addr) links
heapNode2ProcVal hAddr (indeg, NInd link) =
  LocalAliasVal indeg (hAddr2Addr hAddr) $ LocalLinkVal $ hAddr2Addr link


heap2ProcVals :: Heap -> [ProcVal]
heap2ProcVals (Heap _ mapHAddr2IndegNode) =
  map (uncurry heapNode2ProcVal) $ M.toAscList mapHAddr2IndegNode

hSize :: Heap -> Int
hSize (Heap _ mapHAddr2IndegNode) = M.size mapHAddr2IndegNode



showLinkedHeapNode :: Heap -> HAddr -> (Heap, String)
showLinkedHeapNode oldHeap hAddr = case hSafeLookup hAddr oldHeap of
  Nothing -> (oldHeap, "L" ++ show hAddr)
  Just (1, NAtom atomName links) ->
    let (newHeap, args) =
          mapAccumL showLinkedHeapNode (hDelete hAddr oldHeap) links
        stringArgs =
          if null args then "" else "(" ++ intercalate ", " args ++ ")"
    in  (newHeap, show atomName ++ stringArgs)
  Just (1, NInd link) -> showLinkedHeapNode (hDelete hAddr oldHeap) link
  Just _              -> (oldHeap, "L" ++ show hAddr)

showHeapNode :: HAddr -> Heap -> IndegNode -> (Heap, String)
showHeapNode hAddr oldHeap (indeg, NAtom atomName links) =
  let
    incommingLink = if indeg > 0 then "L" ++ show hAddr ++ " -> " else ""
    (newHeap, args) =
      mapAccumL showLinkedHeapNode (hDelete hAddr oldHeap) links
    stringArgs = if null args then "" else "(" ++ intercalate ", " args ++ ")"
  in
    (newHeap, incommingLink ++ show atomName ++ stringArgs)
showHeapNode hAddr oldHeap (indeg, _) =
  let incommingLink  = if indeg > 0 then "L" ++ show hAddr ++ " -> " else ""
      (newHeap, str) = showLinkedHeapNode oldHeap hAddr
  in  (newHeap, incommingLink ++ str)


-- | Pritty print the heap.
--   This function is a instance of the `show`.
showHeap :: Heap -> String
showHeap oldHeap =
  let loop (heap@(Heap _ mapAddr2IndegNode), strs) = if hNull heap
        then reverse strs
        else case map (second fst) $ M.toList mapAddr2IndegNode of
          [] -> reverse strs
          h : t ->
            let (minHAddr, _) = foldl
                  (\ai1@(_, i1) ai2@(_, i2) -> if i1 < i2 then ai1 else ai2)
                  h
                  t
                (newHeap, newStr) =
                  showHeapNode minHAddr heap $ hLookup minHAddr heap
            in  loop (newHeap, newStr : strs)
  in  concatMap (++ ". ") $ loop (oldHeap, [])


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

-- | Initial heap
initialHeap :: Heap
initialHeap = Heap (map HAddr [1 ..]) M.empty

-- | Translate heap to Atom List (assoc list)
toAtomList :: Heap -> AtomList
toAtomList (Heap _ mapHAddr2IndegNode) = map fst $ M.toList mapHAddr2IndegNode

-- | lookup
hLookup :: HAddr -> Heap -> IndegNode
hLookup hAddr (Heap _ mapHAddr2IndegNode) = mapHAddr2IndegNode M.! hAddr

-- | safe lookup
hSafeLookup :: HAddr -> Heap -> Maybe IndegNode
hSafeLookup hAddr (Heap _ mapHAddr2IndegNode) =
  M.lookup hAddr mapHAddr2IndegNode

-- | delete
hDelete :: HAddr -> Heap -> Heap
hDelete hAddr (Heap freeAddrs mapHAddr2IndegNode) =
  Heap (hAddr : freeAddrs) (M.delete hAddr mapHAddr2IndegNode)

{--|
-- | Map.findMin
hFindMin :: Heap -> Maybe (HAddr, IndegNode)
hFindMin (Heap _ mapHAddr2IndegNode) = if M.null mapHAddr2IndegNode
  then Nothing
  else Just $ M.findMin mapHAddr2IndegNode
|--}

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
  -- ^ This NEVER happen.
  --   Since the free addresses are represented with the INFINITE list.

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

-- | Test if the heap is null or not
hNull :: Heap -> Bool
hNull (Heap _ mapAddr2IndegNode) = M.null mapAddr2IndegNode
