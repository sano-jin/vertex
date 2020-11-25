{-# LANGUAGE Safe #-}

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

module VM.Envs (
  Envs (..),
  nullEnvs,
  updateIncommingLinks,
  addMatchedLocalAddrs,
  updateMatchedLocalAddrs,
  addLocalLink2Addr,
  updateLocalLink2Addr,
  updateFreeLink2Addr,
  addFreeLink2Addr,
  updateFreeAddr2Indeg  
) where
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Compiler.Process


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

-- | The initial envirnonment, which holds nothing.
nullEnvs :: Envs
nullEnvs = Envs { incommingLinks = S.empty
                , matchedLocalAddrs = S.empty
                , localLink2Addr = M.empty
                , freeLink2Addr = M.empty
                , freeAddr2Indeg = M.empty
                }

updateIncommingLinks :: (S.Set Addr -> S.Set Addr) -> Envs -> Envs
updateIncommingLinks f envs
  = envs { incommingLinks = f $ incommingLinks envs}

addMatchedLocalAddrs :: Addr -> Envs -> Envs
addMatchedLocalAddrs matchedAddr envs
  = updateMatchedLocalAddrs (S.insert matchedAddr) envs

updateMatchedLocalAddrs :: (S.Set Addr -> S.Set Addr) -> Envs -> Envs
updateMatchedLocalAddrs f envs
  = envs { matchedLocalAddrs = f $ matchedLocalAddrs envs}

-- | the arguments are ...
--
--   - matching address
--
--   - matched address
--
--   - envs to update
addLocalLink2Addr :: Addr -> Addr -> Envs -> Envs
addLocalLink2Addr matchingAddr matchedAddr envs
  = addMatchedLocalAddrs matchedAddr
    $ updateLocalLink2Addr (M.insert matchingAddr matchedAddr) envs

updateLocalLink2Addr :: (M.Map Addr Addr -> M.Map Addr Addr) -> Envs -> Envs
updateLocalLink2Addr f envs
  = envs { localLink2Addr = f $ localLink2Addr envs}

updateFreeLink2Addr :: (M.Map String Addr -> M.Map String Addr) -> Envs -> Envs
updateFreeLink2Addr f envs
  = envs { freeLink2Addr = f $ freeLink2Addr envs}


addFreeLink2Addr :: String -> Addr -> Envs -> Envs
addFreeLink2Addr linkName matchedAddr envs
  = updateFreeLink2Addr (M.insert linkName matchedAddr) envs

updateFreeAddr2Indeg :: (M.Map Addr Indeg -> M.Map Addr Indeg) -> Envs -> Envs
updateFreeAddr2Indeg f envs
  = envs { freeAddr2Indeg = f $ freeAddr2Indeg envs}
