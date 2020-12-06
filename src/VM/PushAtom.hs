module VM.PushAtom
  ( push
  ) where
import           Compiler.Process
import           Data.List
import qualified Data.Map.Strict               as M
import           VM.Envs                        ( Envs(..) )
import           VM.Heap                        ( Heap
                                                , Node(..)
                                                , HAddr
                                                , getIndeg
                                                , hAlloc
                                                , hDelete
                                                , hReplace
                                                , incrIndeg
                                                , setIndeg
                                                )

import           Data.Maybe                     ( catMaybes )
import           Data.Tuple.Extra               ( second )


type LocalEnv = M.Map Addr HAddr
                 -- ^ A map from the addresses in the procVal and linkVal
                 --   to the addresses on the heap.


-- | Delete the the atoms which are the head of the local links on the LHS of the rule.
deleteLocalAtoms :: Envs -> Heap -> Heap
deleteLocalAtoms envs heap =
  let localAddrs = matchedLocalAddrs envs
  in  foldl (flip hDelete) heap localAddrs

-- | decrease the indegree where the free link was poinitng to.
-- This should be called after the matching, before the rewriting (pushing) process.
decrFreeLinkIndeg :: Envs -> Heap -> Heap
decrFreeLinkIndeg envs heap =
  M.foldrWithKey setIndeg heap $ freeAddr2Indeg envs

-- | Pushes the atom of the argument of the atom to the heap.
--   Resolves if it is the link.
pushLinkVal :: Envs -> LocalEnv -> Heap -> LinkVal -> (Heap, HAddr)
pushLinkVal envs _ heap (FreeLinkVal linkName) =
  let hAddr = freeLink2Addr envs M.! linkName in (incrIndeg hAddr heap, hAddr)
pushLinkVal _ localEnv heap (LocalLinkVal addr) =
  let hAddr = localEnv M.! addr in (heap, hAddr)
pushLinkVal envs localEnv oldHeap (AtomVal atomName links) =
  let (newHeap, hLinks) = mapAccumL (pushLinkVal envs localEnv) oldHeap links
  in  hAlloc newHeap (1, NAtom atomName hLinks)
pushLinkVal _ _ oldHeap (DataVal dataAtom) = hAlloc oldHeap (1, NData dataAtom)


-- | Pushes the atom.
pushProcVal
  :: Envs -> LocalEnv -> Heap -> ProcVal -> (Heap, Maybe (Addr, HAddr))
pushProcVal envs localEnv oldHeap (LocalAliasVal indeg addr headingAtom) =
  let (newHeap, hAddr) = pushLinkVal envs localEnv oldHeap headingAtom
  in  (setIndeg hAddr indeg newHeap, Just (addr, hAddr))
pushProcVal envs localEnv oldHeap (FreeAliasVal linkName (AtomVal atomName links))
  = let (newHeap, hLinks) = mapAccumL (pushLinkVal envs localEnv) oldHeap links
        hAddr             = freeLink2Addr envs M.! linkName
        indeg             = getIndeg hAddr newHeap
    in  (hReplace hAddr (indeg, NAtom atomName hLinks) newHeap, Nothing)
pushProcVal envs localEnv oldHeap (FreeAliasVal linkName (DataVal dataAtom)) =
  let hAddr = freeLink2Addr envs M.! linkName
      indeg = getIndeg hAddr oldHeap
  in  (hReplace hAddr (indeg, NData dataAtom) oldHeap, Nothing)
pushProcVal envs _ heap (FreeAliasVal fromLinkName (FreeLinkVal toLinkName)) =
  let fromHAddr = freeLink2Addr envs M.! fromLinkName
      toHAddr   = freeLink2Addr envs M.! toLinkName
      indeg     = freeAddr2Indeg envs M.! fromHAddr + getIndeg fromHAddr heap
  in  (hReplace fromHAddr (indeg, NInd toHAddr) heap, Nothing)
pushProcVal _ _ _ _ = error "not normalized"

pushProcVals
  :: Envs -> LocalEnv -> Heap -> [ProcVal] -> (Heap, [(Addr, HAddr)])
pushProcVals envs localEnv oldHeap procVals =
  second catMaybes $ mapAccumL (pushProcVal envs localEnv) oldHeap procVals

-- | Pushes the RHS on the heap
push :: Heap -> [ProcVal] -> Envs -> Heap
push oldHeap procVals envs =
  let poppedHeap = decrFreeLinkIndeg envs . deleteLocalAtoms envs $ oldHeap
      (newHeap, ascList) = pushProcVals envs localEnv poppedHeap procVals
      localEnv = M.fromList ascList
  in  newHeap
