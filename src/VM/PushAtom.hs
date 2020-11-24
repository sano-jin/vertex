module VM.PushAtom (
  push
  ) where
import qualified Data.Map.Strict as M
import Data.List
import Compiler.Compiler hiding (Envs)
import VM.Heap (
  Node (..),
  Heap,
  getIndeg,
  hAlloc,
  hDelete,
  hReplace,
  setIndeg,
  incrIndeg,
  )
import VM.Envs (
  Envs (..),
  )


type LocalEnv = M.Map Addr Addr
                 -- ^ A map from the addresses in the procVal and linkVal
                 -- to the addresses in the heap.

deleteLocalAtoms :: Envs -> Heap -> Heap
deleteLocalAtoms envs heap
  = let localAddrs = matchedLocalAddrs envs in
      foldl (flip hDelete) heap localAddrs

{--|
-- | decrease the indegree where the free link was poinitng to.
-- This should be called after the matching, before the rewriting (pushing) process.
decrFreeLinkIndeg :: Envs -> Heap -> Heap
decrFreeLinkIndeg envs heap
  = M.foldrWithKey setIndeg heap $ freeAddr2Indeg envs
|--}

pushLinkVal :: Envs -> LocalEnv -> Heap -> LinkVal -> (Heap, Addr)
pushLinkVal envs _ heap (FreeLinkVal linkName) 
  = let hAddr = freeLink2Addr envs M.! linkName in
      (incrIndeg hAddr heap, hAddr)
pushLinkVal _ localEnv heap (LocalLinkVal addr)
  = let hAddr = localEnv M.! addr in
      (heap, hAddr)
pushLinkVal envs localEnv oldHeap (AtomVal atomName links) 
  = let (newHeap, hLinks)
          = mapAccumL (pushLinkVal envs localEnv) oldHeap links in
      hAlloc newHeap (1, NAtom atomName hLinks) 

pushProcVal :: Envs -> LocalEnv -> Heap -> ProcVal -> (Heap, Maybe (Addr, Addr))
pushProcVal envs localEnv oldHeap (LocalAliasVal indeg addr headingAtom) 
  = let (newHeap, hAddr)
          = pushLinkVal envs localEnv oldHeap headingAtom in
      (setIndeg hAddr indeg newHeap, Just (addr, hAddr))
pushProcVal envs localEnv oldHeap (FreeAliasVal linkName (AtomVal atomName links)) 
  = let (newHeap, hLinks)
          = mapAccumL (pushLinkVal envs localEnv) oldHeap links
        hAddr = freeLink2Addr envs M.! linkName
        indeg = freeAddr2Indeg envs M.! hAddr + getIndeg hAddr newHeap
    in 
      (hReplace hAddr (indeg, NAtom atomName hLinks) newHeap, Nothing)
pushProcVal envs _ heap (FreeAliasVal fromLinkName (FreeLinkVal toLinkName)) 
  = let fromHAddr = freeLink2Addr envs M.! fromLinkName
        toHAddr = freeLink2Addr envs M.! toLinkName 
        indeg = freeAddr2Indeg envs M.! fromHAddr + getIndeg fromHAddr heap
    in 
      (hReplace fromHAddr (indeg, NInd toHAddr) heap, Nothing)  
pushProcVal _ _ _ _ = error "not normalized"

pushProcVals :: Envs -> LocalEnv -> Heap -> [ProcVal] -> (Heap, [(Addr, Addr)])
pushProcVals envs localEnv oldHeap procVals
  = let (newHeap, hLinks)
          = mapAccumL (pushProcVal envs localEnv) oldHeap procVals
    in
      (newHeap, filterMaybe hLinks)
  where filterMaybe (Just h:t)  = h : filterMaybe t
        filterMaybe (Nothing:t) = filterMaybe t
        filterMaybe []          = []
          
push :: Heap -> [ProcVal] -> Envs -> Heap
push oldHeap procVals envs
  = let (newHeap, ascList)
          = pushProcVals envs localEnv (deleteLocalAtoms envs oldHeap) procVals
        localEnv = M.fromList ascList
    in
      newHeap
