module Control.Monad.Util
    (
      iterateM
    ) where

iterateM :: (Monad m) => Int -> (a -> m a) -> a -> m a
iterateM 0 _ a = return a
iterateM n f a = f a >>= iterateM (n - 1) f
