{-# LANGUAGE Safe #-}
module Util.Util (
  monadicMapAccumL
  ) where

-- | A monadic version of List.mapAccumL
monadicMapAccumL :: Monad m => (a -> b -> m (a, c)) -> a -> [b] -> m (a, [c])
monadicMapAccumL f a (b : bs) = do
  (a' , c ) <- f a b
  (a'', cs) <- monadicMapAccumL f a' bs
  return (a'', c : cs)
monadicMapAccumL _ a [] = return (a, [])


