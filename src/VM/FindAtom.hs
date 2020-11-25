{-|
Module      : FindAtom
Description : A test matching of the process
Copyright   : (c) sano, 2020
License     : MIT
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module VM.FindAtom (
  findAtoms
  ) where
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe
import Compiler.Process
import Util.Util (
  monadicFoldl
  )
import GHC.Base (
  (<|>)
  )     
import VM.Heap  (
  Node (..),
  Heap,
  AtomList,
  getIndeg,
  toAtomList,
  hLookup,
  )
import VM.Envs (
  Envs (..),
  nullEnvs,
  updateIncommingLinks,
  addMatchedLocalAddrs,
  addLocalLink2Addr,
  updateFreeLink2Addr,
  addFreeLink2Addr,
  updateFreeAddr2Indeg  
  )


-- | Inserts to a map if the key and the value does NOT exist.
-- Preserve the map if the they are present.
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
         -- non-injective matching
    else checkLinkVal heap maybeIndeg envs linkValAddr
checkEmbeddedLinkVal heap maybeIndeg envs linkValAddr
  = checkLinkVal heap maybeIndeg envs linkValAddr

-- | Check whether the given linkVal can match the heap or not.
-- This is a bit loose matching compared with the `checkEmbeddedLinkVal`.
-- This doesn't check the matching address is a member of the `matchedLocalAddrs`
-- Since we want to use this also for matching the atom of the head of the incomming link.
checkLinkVal :: Heap -> (Maybe Indeg) -> Envs -> (LinkVal, Addr) -> Maybe Envs
checkLinkVal heap maybeIndeg envs (AtomVal atomName links, hAddr)
  = case hLookup hAddr heap of
      (hIndeg, (NAtom hAtomName hLinks))
        -> if (maybeIndeg == Nothing
               -- this atom was the head of the free link
               || maybeIndeg == Just hIndeg
               -- or the head of the local link with a certain indegree.
              )
              && atomName == hAtomName
              && length links == length hLinks
           then
             let envs' =
                   if maybeIndeg == Nothing then envs
                   else addMatchedLocalAddrs hAddr envs
                 -- If the maybeIndeg == Nothing, the incomming link is a free link
                 -- otherwise, it is a local link and it should be added
                 -- to the matchedLocalAddrs of envs 
                 zippedLinks = zip links hLinks in
               monadicFoldl (checkEmbeddedLinkVal heap (Just 1)) envs' zippedLinks
           else Nothing
      _  -> error "indirection traversing is not implemented yet"
checkLinkVal _ _ envs (LocalLinkVal matchingAddr, hAddr)
  = case M.lookup matchingAddr $ localLink2Addr envs of
      Nothing -> Just $ addLocalLink2Addr matchingAddr hAddr envs
        -- Haven't matched yet
      Just hAddr'
        -> if hAddr' == hAddr then Just envs
           else Nothing
                -- non-functional matching of local links
checkLinkVal heap _ envs (FreeLinkVal freeLinkName, hAddr)
  = if S.member hAddr $ matchedLocalAddrs envs
    then Nothing
       -- free link cannot match with the addresses
       -- that the local link had already matched.
    else
      case M.lookup freeLinkName $ freeLink2Addr envs of
        Nothing ->
          -- the free link name was not matched before
          -- but may have matched with different names (non-injective matching)
          let defaultIndeg = getIndeg hAddr heap
              indeg
                = M.findWithDefault defaultIndeg hAddr (freeAddr2Indeg envs) - 1 in
            -- If the address was not matched before,
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
                  -- Non-functional match is not allowed.
                  -- The same named (free) links should match the same address.
             else
               let
                 indeg = (freeAddr2Indeg envs M.! hAddr) - 1
                 -- The indegree that is left.
                 -- Since the free link has matched before,
                 -- it can be taken from the freeAddr2Indeg map.
               in
                 if indeg < 0 then Nothing
                 else
                   Just
                   $ updateFreeAddr2Indeg (M.insert hAddr indeg)
                   $ envs

-- | findAtom's arguments are followings
--
--   - the head of the atom list for next findAtom to traverse from the head if succeeded
--     and the heap
--
--   - the traversing atomlist
--
--   - the process that want to match to the heap
--
--   - envirnonmemnt
findAtom :: (AtomList, Heap) -> AtomList -> [ProcVal] -> Envs -> Maybe Envs
findAtom _ _ [] envs = Just envs
findAtom (atomList, heap)
  (hAddr:tAtomList)
  procVal@(LocalAliasVal indeg matchingAddr atomVal@(AtomVal _ _):tProcVals)
  envs
  = if (S.member hAddr $ incommingLinks envs)
       -- Has already matched
       || case M.lookup matchingAddr $ localLink2Addr envs of
            Just hAddr' -> hAddr' /= hAddr
            Nothing     -> False
    then findAtom (atomList, heap) tAtomList procVal envs
         -- non-injective matching of atoms
    else
      let envs' =
            updateIncommingLinks (S.insert hAddr)
            $ addLocalLink2Addr matchingAddr hAddr
            $ envs
      in
        (checkLinkVal heap (Just indeg) envs' (atomVal, hAddr)
          >>= findAtom (atomList, heap) atomList tProcVals)      
        <|> findAtom (atomList, heap) tAtomList procVal envs
findAtom (atomList, heap)
  (hAddr:tAtomList)
  procVal@(FreeAliasVal linkName atomVal@(AtomVal _ _):tProcVals)
  envs
  = if (S.member hAddr $ incommingLinks envs) 
       -- Has already matched
       || case M.lookup linkName (freeLink2Addr envs) of
            Just hAddr' -> hAddr' /= hAddr  
            Nothing     -> False
    then findAtom (atomList, heap) tAtomList procVal envs
         -- non-injective matching of atoms
    else
      let envs' =
            updateIncommingLinks (S.insert hAddr)
            $ addFreeLink2Addr linkName hAddr
            $ updateFreeAddr2Indeg (insertIfNone hAddr (getIndeg hAddr heap))
            $ envs
      in
        (checkLinkVal heap Nothing envs' (atomVal, hAddr)
         >>= findAtom (atomList, heap) atomList tProcVals)
        <|> findAtom (atomList, heap) tAtomList procVal envs
findAtom _ [] (_:_) _ = Nothing
findAtom _ _ _ _ = error "not normalized"


-- | findAtoms tests whether the given process (template) would match that on heap.
-- 
--   - If matches, it returns the environment,
--     which is a collection of the correspondence of the links in the given process
--     and the matched addresses on the heap
--
--   - If it did't match, just returns the `Nothing`.
findAtoms :: [ProcVal] -> Heap -> Maybe Envs
findAtoms procVals heap
  = let atomList = toAtomList heap in
      findAtom (atomList, heap) atomList procVals nullEnvs 


 
    
