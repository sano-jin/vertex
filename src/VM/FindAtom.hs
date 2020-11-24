module VM.FindAtom where
-- import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
-- import Data.Bifunctor 
-- import Data.Tuple.Extra
import Data.Maybe
import Compiler.Compiler hiding (Envs)
import Util.Util (
--  mapEitherList,
--  monadicMapAccumL,
  monadicFoldl
  )
import GHC.Base
import VM.Heap 
import VM.Envs (
  Envs (..),
  nullEnvs,
  updateIncommingLinks,
  addMatchedLocalAddrs,
  -- updateMatchedLocalAddrs,
  addLocalLink2Addr,
  -- updateLocalLink2Addr,
  updateFreeLink2Addr,
  addFreeLink2Addr,
  updateFreeAddr2Indeg  
  )

-- | A helper function
-- inserts to a map if it does NOT exist
insertIfNone :: Ord key => key -> value -> M.Map key value -> M.Map key value
insertIfNone key value mapKeyValue
  = M.alter (Just . fromMaybe value) key mapKeyValue

-- | Check whether the given linkVal can match the heap or not.
-- This firstly checks that this is not "non-injectively matching" to the heap.
-- Since this is a matching for the embedded atom, whose incomming link is a local link,
-- which is not allowed to match non-injectively.
checkEmbeddedLinkVal :: Heap -> Maybe Indeg -> Envs -> (LinkVal, Addr) -> Maybe Envs
checkEmbeddedLinkVal heap maybeIndeg envs linkValAddr@(AtomVal _ _, hAddr)
  = if S.member hAddr $ matchedLocalAddrs envs
    then Nothing
         -- ^ non-injective matching
    else checkLinkVal heap maybeIndeg envs linkValAddr
checkEmbeddedLinkVal heap maybeIndeg envs linkValAddr
  = checkLinkVal heap maybeIndeg envs linkValAddr

-- | Check whether the given linkVal can match the heap or not.
-- This is a bit loose matching compared with the `checkEmbeddedLinkVal`
-- This doesn't check the matching address is a member of the `matchedLocalAddrs`
-- Since we want to use this also for matching the atom of the head of the incomming link.
checkLinkVal :: Heap -> (Maybe Indeg) -> Envs -> (LinkVal, Addr) -> Maybe Envs
checkLinkVal heap maybeIndeg envs (AtomVal atomName links, hAddr)
  = case hLookup hAddr heap of
      Just (hIndeg, (NAtom hAtomName hLinks))
        -> if (maybeIndeg == Nothing
               -- ^ this atom was the head of the free link
               || maybeIndeg == Just hIndeg
               -- ^ or the head of the local link with certain indegree
              )
              && atomName == hAtomName
              && length links == length hLinks
           then
             let envs' = addMatchedLocalAddrs hAddr envs
                 zippedLinks = zip links hLinks in
               monadicFoldl (checkEmbeddedLinkVal heap (Just 1)) envs' zippedLinks
           else Nothing
      Just _  -> error "indirection traversing is not implemented yet"
      Nothing -> error "not serial"
checkLinkVal _ _ envs (LocalLinkVal matchingAddr, hAddr)
  = case M.lookup matchingAddr $ localLink2Addr envs of
      Nothing -> Just $ addLocalLink2Addr matchingAddr hAddr envs
        -- ^ Haven't matched yet
      Just hAddr'
        -> if hAddr' == hAddr then Just envs
           else Nothing
                -- ^ non-functional matching of local links
checkLinkVal heap _ envs (FreeLinkVal freeLinkName, hAddr)
  = if S.member hAddr $ matchedLocalAddrs envs
    then Nothing
       -- ^ free link cannot match with the addresses
       -- that the local link had already matched.
    else
      case M.lookup freeLinkName $ freeLink2Addr envs of
        Nothing ->
          -- ^ the free link name was not matched before
          -- but may have matched with different names (non-injective matching)
          let defaultIndeg = getIndeg hAddr heap
              indeg
                = M.findWithDefault defaultIndeg hAddr (freeAddr2Indeg envs) - 1 in
            -- ^ If the address was not matched before,
            -- it returns the default value : the indegree that was obtained from the heap
            if indeg < 0 then Nothing
            else 
              Just
              $ updateFreeLink2Addr (M.insert freeLinkName hAddr)
              $ updateFreeAddr2Indeg (M.insert hAddr indeg)
              $ envs
        Just hAddr'
          -> if hAddr' /= hAddr
             then Nothing
                  -- ^ Non-functional match is not allowed.
                  -- The same named (free) links should match the same address.
             else
               let
                 indeg = (freeAddr2Indeg envs M.! hAddr) - 1
                 -- ^ The indegree that is left.
                 -- Since the free link has matched before,
                 -- it can be taken from the freeAddr2Indeg map.
               in
                 if indeg < 0 then Nothing
                 else
                   Just
                   $ updateFreeAddr2Indeg (M.insert hAddr indeg)
                   $ envs

findAtom :: (AtomList, Heap) -> AtomList -> [ProcVal] -> Envs -> Maybe Envs
findAtom _ _ [] envs = Just envs
findAtom (atomList, heap)
  -- ^ the head of the atom list for next findAtom to traverse from the head if succeeded
  -- and the heap
  (hAddr:tAtomList)
  -- ^ the traversing atomlist
  procVals@(LocalAliasVal indeg matchingAddr atomVal@(AtomVal _ _):tProcVals)
  -- ^ the process that want to match to the heap
  envs
  -- ^ envirnonmemnt
  = if (S.member hAddr $ incommingLinks envs)
       -- ^ Has already matched
       || case M.lookup matchingAddr $ localLink2Addr envs of
            Just hAddr' -> hAddr' /= hAddr
            Nothing     -> False
    then findAtom (atomList, heap) tAtomList procVals envs
         -- ^ non-injective matching of atoms
    else
      let envs' =
            updateIncommingLinks (S.insert hAddr)
            $ addLocalLink2Addr matchingAddr hAddr
            $ envs
      in
        (checkLinkVal heap (Just indeg) envs' (atomVal, hAddr)
          >>= findAtom (atomList, heap) atomList tProcVals)      
        <|> findAtom (atomList, heap) tAtomList procVals envs
findAtom (atomList, heap)
  (hAddr:tAtomList)
  procVals@(FreeAliasVal linkName atomVal@(AtomVal _ _):tProcVals)
  envs
  = if (S.member hAddr $ incommingLinks envs) 
       -- ^ Has already matched
       || case M.lookup linkName (freeLink2Addr envs) of
            Just hAddr' -> hAddr' /= hAddr  
            Nothing     -> False
    then findAtom (atomList, heap) tAtomList procVals envs
         -- ^ non-injective matching of atoms
    else
      let envs' =
            updateIncommingLinks (S.insert hAddr)
            $ addFreeLink2Addr linkName hAddr
            $ updateFreeAddr2Indeg (insertIfNone hAddr (getIndeg hAddr heap))
            $ envs
      in
        (checkLinkVal heap Nothing envs' (atomVal, hAddr)
         >>= findAtom (atomList, heap) atomList tProcVals)
        <|> findAtom (atomList, heap) tAtomList procVals envs
findAtom _ [] (_:_) _ = Nothing
findAtom _ _ _ _ = error "not normalized"

findAtoms :: [ProcVal] -> Heap -> Maybe Envs
findAtoms procVals heap
  = let atomList = toAtomList heap in
      findAtom (atomList, heap) atomList procVals nullEnvs 


initTestHeap :: Heap
initTestHeap =
  let assocList
        = [ (1, NAtom "a" [3, 1])   -- ^ 1
          , (0, NAtom "b" [3]   )   -- ^ 2
          , (2, NAtom "c" []    )   -- ^ 3
          , (0, NAtom "d" []    )   -- ^ 4          
          , (0, NAtom "e" [6, 6])   -- ^ 5
          , (2, NAtom "f" []    )   -- ^ 6
          , (0, NAtom "g" [8]   )   -- ^ 7
          , (1, NAtom "h" []    )   -- ^ 8
          , (1, NAtom "i" [9]    )  -- ^ 9
          ]
  in
    fst $ mapAccumL hAlloc initialHeap assocList
 
    
