module Heap where
-- import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Data.Set as S
-- import Data.List
-- import Data.Bifunctor 
import Data.Tuple.Extra
import Compiler hiding (Envs)
import Util (
  mapEitherList,
  monadicMapAccumL,
  monadicFoldl
  )
import GHC.Base

type Arity = Int

data Node = NAtom AtomName [Addr]
            -- ^ NAtom SymbolAtomName [Link]
          | NInd Addr
            -- ^ Alias to Addr
          deriving(Eq)

-- | Heap is the map from addresses to the tuples of the indegree and the node
type Heap = M.Map Addr (Indeg, Node)
type AtomList = AssocList Addr (Indeg, Node)
type AssocList key val = [(key, val)]

type MatchedAddrs = S.Set Addr

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

normalizeHeap :: Heap -> Heap
normalizeHeap heap
  = case m_lookupVal (isNInd . snd) heap of
      Just (fromAddr, (indeg, NInd toAddr))
        -> normalizeHeap
           $ M.mapWithKey (normalizeAddrNode (fromAddr, indeg) toAddr)
           $ M.delete fromAddr heap
      Nothing -> heap
      _ -> error "should not reach here"

m_lookupVal :: Ord key => (val -> Bool) -> M.Map key val -> Maybe (key, val)
m_lookupVal f mapA2B
  = let list = M.toAscList mapA2B
        lookupVal' ((key, val):t)
          = if f val then Just (key, val)
            else lookupVal' t
        lookupVal' [] = Nothing
    in
      lookupVal' list


checkEmbeddedLinkVal :: Heap -> Maybe Indeg -> Envs -> (LinkVal, Addr) -> Maybe Envs
checkEmbeddedLinkVal heap indeg envs (linkVal@(AtomVal _ _), addr')
  = if S.member addr' $ matchedLocalAddrs envs
    then Nothing
         -- ^ non-injective matching of atoms
    else checkLinkVal heap indeg envs (linkVal, addr')
checkEmbeddedLinkVal heap indeg envs (linkVal, addr')
  = checkLinkVal heap indeg envs (linkVal, addr')


checkLinkVal :: Heap -> (Maybe Indeg) -> Envs -> (LinkVal, Addr) -> Maybe Envs
checkLinkVal heap maybeIndeg envs (AtomVal atomName links, addr')
  = case M.lookup addr' heap of
      Just tupIndegNode@(indeg', (NAtom atomName' links'))
        -> if (maybeIndeg == Just indeg' || maybeIndeg == Nothing)
              && atomName == atomName'
              && length links == length links'
           then
             let envs' = addMatchedLocalAddrs addr' envs
                 zippedLinks = zip links links' in
               monadicFoldl (checkEmbeddedLinkVal heap (Just 1)) envs' zippedLinks
           else Nothing
      Just _ -> error "indirection traversing is not implemented yet"
      Nothing -> error "not serial"
checkLinkVal _ _ envs (LocalLinkVal addr, addr')
  = case M.lookup addr $ localLink2Addr envs of
      Nothing -> Just $ addLocalLink2Addr addr addr' envs
      Just addr''
        -> if addr'' == addr' then Just envs
           else Nothing
                -- ^ non-injective matching of local links
checkLinkVal heap _ envs (FreeLinkVal freeLinkName, addr')
  = if S.member addr' $ matchedLocalAddrs envs
    then Nothing
       -- ^ free link cannot match with the addresses
       -- that the local link has already matched.
    else
      case M.lookup freeLinkName $ freeLink2Addr envs of
        Nothing ->
          let indeg = getIndeg addr' heap
              envs' =
                updateFreeLink2Addr (M.insert freeLinkName addr')
                $ updateFreeAddr2Indeg (M.insert addr' indeg)
                $ envs
          in Just envs'
        Just addr''
          -> if addr'' /= addr' then Nothing
             else
               let
                 indegLeft = freeAddr2Indeg envs M.! addr'
                 -- ^ The indegree that is left
                 newIndeg = indegLeft - 1
               in
                 if newIndeg < 0 then Nothing
                 else
                   Just
                   $ updateFreeAddr2Indeg (M.insert addr' newIndeg)
                   $ envs

getIndeg :: Addr -> Heap -> Indeg
getIndeg addr heap
  = fst $ heap M.! addr

data Envs = Envs { incommingLinks :: S.Set Addr
                   -- ^ A set of the addresseses of the matched incoming links
                 , matchedLocalAddrs :: S.Set Addr
                   -- ^ A set of the addresseses of the matched local links
                   -- contains the addresses to the "embedded atom"s.
                 , localLink2Addr :: M.Map Addr Addr
                   -- ^ A map from local links to the matched addresses
                   -- Does not contain the addresses to the "embedded atom"s.
                   -- Since the local links of them are not known
                 , freeLink2Addr :: M.Map String Addr
                   -- ^ A map from free link names to the matched addresses
                 , freeAddr2Indeg :: M.Map Addr Indeg
                 -- ^ A map from the addresses which the free links have mathced
                 -- to the indegrees that are left
                 -- That is, matching free links will consume the indegrees in this map
                 -- Notice the indegrees should be kept as non-negative value
                 }
            deriving (Show)

updateIncommingLinks :: (S.Set Addr -> S.Set Addr) -> Envs -> Envs
updateIncommingLinks f envs
  = envs { incommingLinks = f $ incommingLinks envs}


addMatchedLocalAddrs :: Addr -> Envs -> Envs
addMatchedLocalAddrs addr envs
  = updateMatchedLocalAddrs (S.insert addr) envs

updateMatchedLocalAddrs :: (S.Set Addr -> S.Set Addr) -> Envs -> Envs
updateMatchedLocalAddrs f envs
  = envs { matchedLocalAddrs = f $ matchedLocalAddrs envs}

addLocalLink2Addr :: Addr -> Addr -> Envs -> Envs
addLocalLink2Addr addr addr' envs
  = addMatchedLocalAddrs addr
    $ updateLocalLink2Addr (M.insert addr addr') envs

updateLocalLink2Addr :: (M.Map Addr Addr -> M.Map Addr Addr) -> Envs -> Envs
updateLocalLink2Addr f envs
  = envs { localLink2Addr = f $ localLink2Addr envs}

updateFreeLink2Addr :: (M.Map String Addr -> M.Map String Addr) -> Envs -> Envs
updateFreeLink2Addr f envs
  = envs { freeLink2Addr = f $ freeLink2Addr envs}

addFreeLink2Addr :: String -> Addr -> Envs -> Envs
addFreeLink2Addr linkName addr envs
  = updateFreeLink2Addr (M.insert linkName addr) envs


updateFreeAddr2Indeg :: (M.Map Addr Indeg -> M.Map Addr Indeg) -> Envs -> Envs
updateFreeAddr2Indeg f envs
  = envs { freeAddr2Indeg = f $ freeAddr2Indeg envs}


findAtom :: ProcVal -> AtomList -> Heap -> Envs -> Maybe Envs
findAtom localAliasVal@(LocalAliasVal indeg addr atomVal@(AtomVal atomName links)) ((addr', _):t) heap envs
  = if (S.member addr' $ incommingLinks envs) 
       -- ^ Has already matched
       || case M.lookup addr (localLink2Addr envs) of
            Just matchedAddr -> matchedAddr /= addr'  
            Nothing -> False
    then Nothing
         -- ^ non-injective matching of atoms
    else
      let envs' =
            updateIncommingLinks (S.insert addr')
            $ addLocalLink2Addr addr addr' envs
      in
      checkLinkVal heap (Just indeg) envs' (atomVal, addr')  
      <|> findAtom localAliasVal t heap envs
findAtom (LocalAliasVal _ _ (AtomVal _ _)) [] _ _ = Nothing
findAtom freeAliasVal@(FreeAliasVal linkName atomVal@(AtomVal atomName links)) ((addr', _):t) heap envs
  = if (S.member addr' $ incommingLinks envs) 
       -- ^ Has already matched
       || case M.lookup linkName (freeLink2Addr envs) of
            Just matchedAddr -> matchedAddr /= addr'  
            Nothing -> False
    then Nothing
         -- ^ non-injective matching of atoms
    else
      let envs' =
            updateIncommingLinks (S.insert addr')
            $ addFreeLink2Addr linkName addr' envs
      in
      checkLinkVal heap Nothing envs' (atomVal, addr')  
      <|> findAtom freeAliasVal t heap envs
