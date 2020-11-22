{-# LANGUAGE Safe #-}

module Util (
  mapEitherList
  ) where
import Control.Monad.Except

{--|
mapEitherList :: (a -> Either b c) -> [a] -> Either b [c]
mapEitherList f (h:t)
  = liftM2 (:) (f h) $ mapEitherList f t
mapEitherList _ [] = return []
|--}

mapEitherList :: (a -> Either b c) -> [a] -> Either b [c]
mapEitherList = monadicMapList

monadicMapList :: Monad m => (a -> m c) -> [a] -> m [c]
monadicMapList f (h:t)
  = liftM2 (:) (f h) $ monadicMapList f t
monadicMapList _ [] = return []


mapMaybeList :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybeList f (h:t)
  = liftM2 (:) (f h) $ mapMaybeList f t
mapMaybeList _ [] = return []

