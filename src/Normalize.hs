module Normalize where
import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Data.Tuple.Extra
import Syntax
import Compiler

-- | Normalize the indirection from local link to local link
substituteAddr :: Addr -> Addr -> Addr -> Addr
substituteAddr fromAddr toAddr addr 
  = if addr == fromAddr then toAddr
    else addr

normalizeLocal2LocalLinkVal :: Addr -> Addr -> LinkVal -> LinkVal
normalizeLocal2LocalLinkVal fromAddr toAddr (LocalLinkVal addr)
  = LocalLinkVal $ substituteAddr fromAddr toAddr addr
normalizeLocal2LocalLinkVal fromAddr toAddr (AtomVal atomName links)
  = AtomVal atomName $ map (normalizeLocal2LocalLinkVal fromAddr toAddr) links
normalizeLocal2LocalLinkVal _ _ freeLinkVal = freeLinkVal

normalizeLocal2LocalAliasVal :: (Addr, Indeg) -> Addr -> ProcVal -> ProcVal
normalizeLocal2LocalAliasVal (fromAddr, fromIndeg) toAddr (LocalAliasVal indeg addr linkVal)
  = let indeg' = if addr == toAddr then indeg + fromIndeg - 1
                 else indeg
    in
      LocalAliasVal indeg' addr $ normalizeLocal2LocalLinkVal fromAddr toAddr linkVal
normalizeLocal2LocalAliasVal (fromAddr, _) toAddr (FreeAliasVal linkName linkVal)
  = FreeAliasVal linkName $ normalizeLocal2LocalLinkVal fromAddr toAddr linkVal

isLocal2LocalIndirection :: ProcVal -> Bool
isLocal2LocalIndirection (LocalAliasVal _ _ (LocalLinkVal _)) = True
isLocal2LocalIndirection _                                    = False

normalizeLocal2LocalIndirection :: [ProcVal] -> [ProcVal]
normalizeLocal2LocalIndirection procVals
  = case find isLocal2LocalIndirection procVals of
      Nothing -> procVals
      Just local2localAlias@(LocalAliasVal indeg fromAddr (LocalLinkVal toAddr))
        -> normalizeLocal2LocalIndirection
           $ map (normalizeLocal2LocalAliasVal (fromAddr, indeg) toAddr)
           $ filter (/= local2localAlias) procVals
      _ -> error "should not reach here"

-- | Normalize the indirection from local link to free link
normalizeLocal2FreeLinkVal :: Addr -> String -> LinkVal -> LinkVal
normalizeLocal2FreeLinkVal fromAddr toLinkName (LocalLinkVal addr)
  = if fromAddr == addr
    then FreeLinkVal toLinkName
    else LocalLinkVal addr
normalizeLocal2FreeLinkVal fromAddr toLinkName (AtomVal atomName links)
  = AtomVal atomName $ map (normalizeLocal2FreeLinkVal fromAddr toLinkName) links
normalizeLocal2FreeLinkVal _ _ freeLinkVal = freeLinkVal

normalizeLocal2FreeAliasVal :: Addr -> String -> ProcVal -> ProcVal
normalizeLocal2FreeAliasVal fromAddr toLinkName (LocalAliasVal indeg addr linkVal)
  = if fromAddr == addr
  then FreeAliasVal toLinkName linkVal'
  else LocalAliasVal indeg addr linkVal'
  where linkVal' = normalizeLocal2FreeLinkVal fromAddr toLinkName linkVal
normalizeLocal2FreeAliasVal _ _ freeAliasVal
  = freeAliasVal

isLocal2FreeIndirection :: ProcVal -> Bool
isLocal2FreeIndirection (LocalAliasVal _ _ (FreeLinkVal _)) = True
isLocal2FreeIndirection _                                    = False

normalizeLocal2FreeIndirection :: [ProcVal] -> [ProcVal]
normalizeLocal2FreeIndirection procVals
  = case find isLocal2FreeIndirection procVals of
      Nothing -> procVals
      Just local2freeAlias@(LocalAliasVal _ fromAddr (FreeLinkVal toLinkName))
        -> normalizeLocal2FreeIndirection
           $ map (normalizeLocal2FreeAliasVal fromAddr toLinkName)
           $ filter (/= local2freeAlias) procVals
      _ -> error "should not reach here"


-- | Normalize the indirection from free link to local link
isFree2LocalIndirection :: ProcVal -> Bool
isFree2LocalIndirection (FreeAliasVal _ (LocalLinkVal _)) = True
isFree2LocalIndirection _                                    = False

normalizeFree2LocalIndirection :: [ProcVal] -> [ProcVal]
normalizeFree2LocalIndirection procVals
  = case find isFree2LocalIndirection procVals of
      Nothing -> procVals
      Just free2localAlias@(FreeAliasVal fromLinkName (LocalLinkVal toAddr))
        -> normalizeFree2LocalIndirection
           $ map (normalizeLocal2FreeAliasVal toAddr fromLinkName)
           $ filter (/= free2localAlias) procVals
      _ -> error "should not reach here"


