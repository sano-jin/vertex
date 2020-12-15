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
module VM.FindAtom
  ( findAtoms
  ) where
import           Control.Monad.Except           ( foldM )
import           Compiler.Process
import           Compiler.Syntax               
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           VM.Envs                        ( Envs(..)
                                                , addFreeLink2Addr
                                                , addLocalLink2Addr
                                                , addMatchedLocalAddrs
                                                , nullEnvs
                                                , updateFreeAddr2Indeg
                                                , insertPCtxName2Node
                                                , isMatchedLocalHAddr
                                                , isMatchedIncommingHAddr
                                                , addIncommingLinks
                                                , insertFreeAddr2Indeg
                                                , lookupLocalLink2Addr
                                                , lookupFreeLink2Addr
                                                )
import           VM.Heap                        ( AtomList
                                                , Heap
                                                , Node(..)
                                                , HAddr
                                                , HAtomName(..)
                                                , getIndeg
                                                , hLookup
                                                , toAtomList
                                                )


-- | Inserts to a map if the key and the value does NOT exist.
--   Preserve the map if the they are present.
insertIfNone :: Ord key => key -> value -> M.Map key value -> M.Map key value
insertIfNone key value = M.alter (Just . fromMaybe value) key

-- | Check whether the given linkVal can match the heap or not.
--   This firstly checks that this is not "non-injectively matching" to the heap.
--   Since this is a matching for the embedded atom, whose incomming link is a local link,
--   which is not allowed to match non-injectively.
checkEmbeddedLinkVal
  :: Heap -> Maybe Indeg -> Envs -> (LinkVal, HAddr) -> Maybe Envs
checkEmbeddedLinkVal heap maybeIndeg envs linkValAddr@(AtomVal _ _, hAddr) =
  if isMatchedLocalHAddr hAddr envs 
  then Nothing -- non-injective matching
  else checkLinkVal heap maybeIndeg envs linkValAddr
checkEmbeddedLinkVal heap maybeIndeg envs linkValAddr =
  checkLinkVal heap maybeIndeg envs linkValAddr


-- | Check indeg when matching in "checkLinkVal".
checkIndeg :: Maybe Indeg -> Indeg -> Bool
checkIndeg Nothing _ = True
  -- this atom was the head of the free link
checkIndeg (Just indeg) hIndeg
  = indeg == hIndeg
    -- or the head of the local link with a certain indegree.

-- | Check whether the given linkVal can match the heap or not.
--   This is a bit loose matching compared with the `checkEmbeddedLinkVal`.
--   This doesn't check the matching address is a member of the `matchedLocalAddrs`
--   Since we want to use this also for matching the atom of the head of the incomming link.
checkLinkVal :: Heap -> Maybe Indeg -> Envs -> (LinkVal, HAddr) -> Maybe Envs
checkLinkVal heap maybeIndeg envs (AtomVal (ProcessContext name (Just type_)) _, hAddr) =
  case hLookup hAddr heap of
    (hIndeg, node) ->
      if checkIndeg maybeIndeg hIndeg
         && isNodeSameType node type_
      then
        let newEnvs = insertPCtxName2Node name node envs in
          Just $ if isNothing maybeIndeg then newEnvs
                 else addMatchedLocalAddrs hAddr newEnvs
                    -- If the maybeIndeg == Nothing, the incomming link is a free link
                    -- otherwise, it is a local link and it should be added
                    -- to the matchedLocalAddrs of envs
      else Nothing
  where isNodeSameType (NAtom (HInt _   ) []) TypeInt    = True
        isNodeSameType (NAtom (HString _) []) TypeString = True
        isNodeSameType (NAtom _ []          ) TypeUnary  = True
        isNodeSameType (_                   ) _          = False
checkLinkVal _ _ _  pCtxVal@(AtomVal (ProcessContext _ Nothing) _, _)
  = error $ "matching of the untyped process context is not implemented "
    ++ show pCtxVal
checkLinkVal heap maybeIndeg envs (AtomVal atomName links, hAddr) =
  case hLookup hAddr heap of
    (hIndeg, NAtom hAtomName hLinks) ->
      if checkIndeg maybeIndeg hIndeg 
         && isNodeSameName atomName hAtomName
         && length links == length hLinks
      then
        let
          envs' = if isNothing maybeIndeg
            then envs
            else addMatchedLocalAddrs hAddr envs
            -- If the maybeIndeg == Nothing, the incomming link is a free link
            -- otherwise, it is a local link and it should be added
            -- to the matchedLocalAddrs of envs
          zippedLinks = zip links hLinks
        in
          foldM (checkEmbeddedLinkVal heap (Just 1)) envs' zippedLinks
      else
        Nothing
    _           -> error "indirection traversing is not implemented yet"
  where isNodeSameName (Int i1)       (HInt    i2)  = i1 == i2
        isNodeSameName (String s1)    (HString s2)  = s1 == s2
        isNodeSameName (Symbol name1) (HSymbol name2) = name1 == name2
        isNodeSameName _ _ = False
    
checkLinkVal _ _ envs (LocalLinkVal matchingAddr, hAddr) =
  case lookupLocalLink2Addr matchingAddr envs of
    Nothing     -> Just $ addLocalLink2Addr matchingAddr hAddr envs
      -- Haven't matched yet
    Just hAddr' -> if hAddr' == hAddr then Just envs else Nothing
      -- non-functional matching of local links
checkLinkVal heap _ envs (FreeLinkVal freeLinkName, hAddr) =
  if isMatchedLocalHAddr hAddr envs
    then Nothing
       -- free link cannot match with the addresses
       -- that the local link had already matched.
    else case lookupFreeLink2Addr freeLinkName envs of
      Nothing ->
        -- the free link name was not matched before
        -- but may have matched with different names (non-injective matching)
        let defaultIndeg = getIndeg hAddr heap
            indeg =
              M.findWithDefault defaultIndeg hAddr (freeAddr2Indeg envs) - 1
        in 
          -- If the address was not matched before,
          -- it returns the default value : the indegree that was obtained from the heap
            if indeg < 0
              then Nothing
              else
                Just
                . addFreeLink2Addr freeLinkName hAddr
                . insertFreeAddr2Indeg hAddr indeg
                $ envs
      Just hAddr' -> if hAddr' /= hAddr
        then Nothing
                          -- Non-functional match is not allowed.
                          -- The same named (free) links should match the same address.
        else
          let indeg = (freeAddr2Indeg envs M.! hAddr) - 1
            -- The indegree that is left.
            -- Since the free link has matched before,
            -- it can be taken from the freeAddr2Indeg map.
          in  if indeg < 0
                then Nothing
                else Just $ insertFreeAddr2Indeg hAddr indeg envs


-- | findAtom's arguments are followings.
--   The head of the atom list for next findAtom to traverse from the head if succeeded
--   and the heap,
--   the traversing atomlist,
--   the process that want to match to the heap,
--   and the envirnonmemnt
findAtom :: (AtomList, Heap) -> AtomList -> [ProcVal] -> Envs -> [Envs]
findAtom _ _ [] envs = [envs]
findAtom _ _ (LocalAliasVal _ fromAddr (LocalLinkVal toAddr) : _) _ =
  error $ "Local to Local indirection \""
  ++ show fromAddr ++ " -> " ++ show toAddr ++ "\" is not normalized"
findAtom _ _ (FreeAliasVal fromLinkName (LocalLinkVal toAddr) : _) _ =
  error $ "Free 2 Local indirection \""
  ++ fromLinkName ++ " -> " ++ show toAddr ++ "\" is not normalized"
findAtom _ _ (LocalAliasVal _ fromAddr (FreeLinkVal toLinkName) : _) _ =
  error $ "Local 2 Local indirection \""
  ++ show fromAddr ++ " -> " ++ toLinkName ++ "\" is not normalized"
findAtom _ _ (FreeAliasVal fromLinkName (FreeLinkVal toLinkName) : _) _ =
  error $ "Free 2 Free indirection on LHS is not implemented. \""
  ++ fromLinkName ++ " -> " ++ toLinkName ++ "\" is not normalized"
findAtom (atomList, heap) (hAddr : tAtomList) procVal@(LocalAliasVal indeg matchingAddr atomVal : tProcVals) envs
  = let rest = 
          findAtom (atomList, heap) tAtomList procVal envs
        -- The rest result for the case that this match failed.
        -- This is mainly used for the non-deterministic execution.
    in
      if isMatchedIncommingHAddr hAddr envs
         -- Has already matched with the other incomming link.
         || case lookupLocalLink2Addr matchingAddr envs of
              Just hAddr' -> hAddr' /= hAddr
              Nothing     -> False
      then rest
           -- Failed with non-injective matching of atoms.
      else
        let envs' =
              addIncommingLinks hAddr
              . addLocalLink2Addr matchingAddr hAddr
              $ envs
        in  case checkLinkVal heap (Just indeg) envs' (atomVal, hAddr) of
              Just envs'' -> 
                findAtom (atomList, heap) atomList tProcVals envs'' ++ rest
              Nothing -> rest
findAtom (atomList, heap) (hAddr : tAtomList) procVal@(FreeAliasVal linkName atomVal : tProcVals) envs
  = let rest = 
          findAtom (atomList, heap) tAtomList procVal envs
        -- The rest result for the case that this match failed.
        -- This is mainly used for the non-deterministic execution.
    in
      if isMatchedIncommingHAddr hAddr envs
         -- Has already matched
         || case lookupFreeLink2Addr linkName envs of
              Just hAddr' -> hAddr' /= hAddr
              Nothing     -> False
      then rest
           -- non-injective matching of atoms
      else
        let envs' =
              addIncommingLinks hAddr
              . addFreeLink2Addr linkName hAddr
              . updateFreeAddr2Indeg (insertIfNone hAddr (getIndeg hAddr heap))
              $ envs
        in  case checkLinkVal heap Nothing envs' (atomVal, hAddr) of
              Just envs'' -> 
                findAtom (atomList, heap) atomList tProcVals envs'' ++ rest
              Nothing -> rest
findAtom _ [] (_ : _) _ = []


-- | findAtoms tests whether the given process (template) would match that on heap.
--
--   - it returns the list of the matched environment,
--     which is a collection of the correspondence of the links in the given process
--     and the matched addresses on the heap
--
--   - If it did't match at all, it just returns the `[]`.
findAtoms :: [ProcVal] -> Heap -> [Envs]
findAtoms procVals heap =
  let atomList = toAtomList heap
  in  findAtom (atomList, heap) atomList procVals nullEnvs
