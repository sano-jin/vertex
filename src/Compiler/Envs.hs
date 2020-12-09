{-# LANGUAGE Safe #-}

module Compiler.Envs
  ( Envs(..)
  , updateLocalMapAddrIndeg
  , updateLocalEnv
  , addFreeTail
  , addFreeHead
  , nullEnvs
  , incrAddrSeed
  , incrLocalIndeg
  , hasLink
  ) where
import           Compiler.Process
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S


type HasHead = Bool
type EnvList = [(String, (Addr, HasHead))]
-- ^ A type for the environment of local links

type EnvSet = S.Set String
-- ^ A type for the environment of free links


-- | A type for the Envirnment of links
data Envs = Envs
  { localEnv          :: EnvList
                   -- ^ A mapping (list of tuple) from the local link names
                   -- to the tuples of the given address
                   -- and the boolean denotes whether its head appeared or not
  , localMapAddrIndeg :: M.Map Addr Indeg
                   -- ^ A map from the local link addresses to their indegrees
  , freeTailEnv       :: EnvSet
                   -- ^ A set of free tail link names
  , freeHeadEnv       :: EnvSet
                   -- ^ A set of free head link names
  , addrSeed          :: Int
                   -- ^ the number of the local links appeared in the process
  }
  deriving Show

-- | Some helper functions for the link environment
updateLocalEnv :: (EnvList -> EnvList) -> Envs -> Envs
updateLocalEnv f envs = envs { localEnv = f $ localEnv envs }

updateLocalMapAddrIndeg
  :: (M.Map Addr Indeg -> M.Map Addr Indeg) -> Envs -> Envs
updateLocalMapAddrIndeg f envs =
  envs { localMapAddrIndeg = f $ localMapAddrIndeg envs }

updateFreeTailEnv :: (EnvSet -> EnvSet) -> Envs -> Envs
updateFreeTailEnv f envs = envs { freeTailEnv = f $ freeTailEnv envs }

addFreeTail :: String -> Envs -> Envs
addFreeTail = updateFreeTailEnv . S.insert

updateFreeHeadEnv :: (EnvSet -> EnvSet) -> Envs -> Envs
updateFreeHeadEnv f envs = envs { freeHeadEnv = f $ freeHeadEnv envs }

addFreeHead :: String -> Envs -> Envs
addFreeHead = updateFreeHeadEnv . S.insert

-- | The initial envs 
nullEnvs :: Envs
nullEnvs = Envs { localEnv          = []
                , localMapAddrIndeg = M.empty
                , freeTailEnv       = S.empty
                , freeHeadEnv       = S.empty
                , addrSeed          = 0
                }

-- | Increse the seed for the local addresses by 1.
--   This should be hided and done implicitly when creating a new local link.
incrAddrSeed :: Envs -> Envs
incrAddrSeed envs = envs { addrSeed =  addrSeed envs + 1 }

incrLocalIndeg :: Addr -> Envs -> Envs
incrLocalIndeg addr = updateLocalMapAddrIndeg (M.adjust (+ 1) addr)

-- | Checks whether the link appeared in the process.
--   Notice the 0-indegree local link is ignored.
--   Since it is automatically added at compiling phase even user didn't write it.
hasLink :: Envs -> Bool
hasLink envs =
  not (M.null $ M.filter (/= 0) $ localMapAddrIndeg envs)
  || not (S.null $ freeTailEnv envs)
  || not (S.null $ freeHeadEnv envs)
