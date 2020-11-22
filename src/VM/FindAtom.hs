module VM.FindAtom where
-- import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Data.Set as S
-- import Data.List
-- import Data.Bifunctor 
-- import Data.Tuple.Extra
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

-- | Check whether the given linkVal can match the heap or not.
-- This firstly checks that this is not "non-injectively matching" to the heap.
-- Since this is a matching for the embedded atom, whose incomming link is a local link,
-- which is not allowed to match non-injectively.
checkEmbeddedLinkVal :: Heap -> Maybe Indeg -> Envs -> (LinkVal, Addr) -> Maybe Envs
checkEmbeddedLinkVal heap indeg envs (linkVal@(AtomVal _ _), addr')
  = if S.member addr' $ matchedLocalAddrs envs
    then Nothing
         -- ^ non-injective matching 
    else checkLinkVal heap indeg envs (linkVal, addr')
checkEmbeddedLinkVal heap indeg envs (linkVal, addr')
  = checkLinkVal heap indeg envs (linkVal, addr')

-- | Check whether the given linkVal can match the heap or not.
-- This is a bit loose matching compared with the `checkEmbeddedLinkVal`
-- This doesn't check the matching address is a member of the `matchedLocalAddrs`
-- Since we want to use this for matching the atom of the head of the link.
checkLinkVal :: Heap -> (Maybe Indeg) -> Envs -> (LinkVal, Addr) -> Maybe Envs
checkLinkVal heap maybeIndeg envs (AtomVal atomName links, addr')
  = case M.lookup addr' heap of
      Just (indeg', (NAtom atomName' links'))
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


findAtom :: AtomList -> Heap -> Envs -> ProcVal -> Maybe Envs
findAtom ((addr', _):t) heap envs localAliasVal@(LocalAliasVal indeg addr atomVal@(AtomVal _ _)) 
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
      <|> findAtom  t heap envs localAliasVal
findAtom [] _ _ (LocalAliasVal _ _ (AtomVal _ _)) = Nothing
findAtom ((addr', _):t) heap envs freeAliasVal@(FreeAliasVal linkName atomVal@(AtomVal _ _)) 
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
      <|> findAtom t heap envs freeAliasVal 
findAtom [] _ _ (FreeAliasVal _ (AtomVal _ _)) = Nothing
findAtom _ _ _ _ = error "not normalized"

findAtoms :: [ProcVal] -> Heap -> Maybe Envs
findAtoms procVals heap
  = let atomList = M.toAscList heap in
      monadicFoldl (findAtom atomList heap) nullEnvs procVals 
