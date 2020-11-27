{-# LANGUAGE Safe #-}
module Util.Util
  ( mapEitherList
  , monadicMapAccumL
  , monadicFoldl
  ) where
import           Control.Monad.Except

{--|
mapEitherList :: (a -> Either b c) -> [a] -> Either b [c]
mapEitherList f (h:t)
  = liftM2 (:) (f h) $ mapEitherList f t
mapEitherList _ [] = return []
|--}

mapEitherList :: (a -> Either b c) -> [a] -> Either b [c]
mapEitherList = monadicMapList

monadicMapList :: Monad m => (a -> m c) -> [a] -> m [c]
monadicMapList f (h : t) = liftM2 (:) (f h) $ monadicMapList f t
monadicMapList _ []      = return []

{--|
mapMaybeList :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybeList f (h:t)
  = liftM2 (:) (f h) $ mapMaybeList f t
mapMaybeList _ [] = return []
|--}

-- | A monadic version of List.mapAccumL
monadicMapAccumL :: Monad m => (a -> b -> m (a, c)) -> a -> [b] -> m (a, [c])
monadicMapAccumL f a (b : bs) = do
  (a' , c ) <- f a b
  (a'', cs) <- monadicMapAccumL f a' bs
  return (a'', c : cs)
monadicMapAccumL _ a [] = return (a, [])

monadicFoldl :: Monad m => (acc -> b -> m acc) -> acc -> [b] -> m acc
monadicFoldl f acc (h : t) = flip (monadicFoldl f) t =<< f acc h
monadicFoldl _ acc []      = return acc
