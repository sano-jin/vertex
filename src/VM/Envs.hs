{-|
Module      : VM.Envs
Description : A data to keep the correspondence of the links and the matched addresses
Copyright   : (c) sano, 2020
License     : MIT
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module VM.Envs
  ( Envs(..)
  , nullEnvs
  , updateIncommingLinks
  , addMatchedLocalAddrs
  , updateMatchedLocalAddrs
  , addLocalLink2Addr
  , updateLocalLink2Addr
  , updateFreeLink2Addr
  , addFreeLink2Addr
  , updateFreeAddr2Indeg
  ) where
import           Compiler.Process
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import           VM.Heap                       (HAddr)


data Envs = Envs
  { incommingLinks    :: S.Set HAddr
                   -- ^ A set of the addresseses of the matched incoming links on the heap.
  , matchedLocalAddrs :: S.Set HAddr
                   -- ^ A set of the addresseses of the matched local links on the heap.
                   --   Contains the addresses to the "embedded atom"s.
  , localLink2Addr    :: M.Map Addr HAddr
                   -- ^ A map from local links on the LHS to the matched addresses on the heap.
                   --   This does not contain the addresses to the "embedded atom"s on the LHS.
                   --   Since the local links of them are not known
  , freeLink2Addr     :: M.Map String HAddr
                   -- ^ A map from free link names on the LHS to the matched addresses on the heap.
  , freeAddr2Indeg    :: M.Map HAddr Indeg
                 -- ^ A map from the addresses on the heap which the free links have mathced
                 --   to the indegrees that are left.
                 --   That is, matching free links will consume the indegrees in this map
                 --   Notice the indegrees should be kept as non-negative value
  }
  deriving Show

-- | The initial envirnonment, which holds nothing.
nullEnvs :: Envs
nullEnvs = Envs { incommingLinks    = S.empty
                , matchedLocalAddrs = S.empty
                , localLink2Addr    = M.empty
                , freeLink2Addr     = M.empty
                , freeAddr2Indeg    = M.empty
                }

updateIncommingLinks :: (S.Set HAddr -> S.Set HAddr) -> Envs -> Envs
updateIncommingLinks f envs = envs { incommingLinks = f $ incommingLinks envs }

addMatchedLocalAddrs :: HAddr -> Envs -> Envs
addMatchedLocalAddrs matchedHAddr =
  updateMatchedLocalAddrs (S.insert matchedHAddr)

updateMatchedLocalAddrs :: (S.Set HAddr -> S.Set HAddr) -> Envs -> Envs
updateMatchedLocalAddrs f envs =
  envs { matchedLocalAddrs = f $ matchedLocalAddrs envs }

-- | The arguments are
--   the matching address
--   , the matched address
--   and the envs to update.
addLocalLink2Addr :: Addr -> HAddr -> Envs -> Envs
addLocalLink2Addr matchingAddr matchedHAddr = addMatchedLocalAddrs matchedHAddr
  . updateLocalLink2Addr (M.insert matchingAddr matchedHAddr)

updateLocalLink2Addr :: (M.Map Addr HAddr -> M.Map Addr HAddr) -> Envs -> Envs
updateLocalLink2Addr f envs = envs { localLink2Addr = f $ localLink2Addr envs }

updateFreeLink2Addr :: (M.Map String HAddr -> M.Map String HAddr) -> Envs -> Envs
updateFreeLink2Addr f envs = envs { freeLink2Addr = f $ freeLink2Addr envs }


addFreeLink2Addr :: String -> HAddr -> Envs -> Envs
addFreeLink2Addr linkName matchedHAddr =
  updateFreeLink2Addr (M.insert linkName matchedHAddr)

updateFreeAddr2Indeg :: (M.Map HAddr Indeg -> M.Map HAddr Indeg) -> Envs -> Envs
updateFreeAddr2Indeg f envs = envs { freeAddr2Indeg = f $ freeAddr2Indeg envs }
