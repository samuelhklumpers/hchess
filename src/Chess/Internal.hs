module Chess.Internal ( module Chess.Internal ) where

import Control.Monad (forM_)
import Control.Concurrent.STM.TMVar (TMVar, writeTMVar, takeTMVar)
import Control.Concurrent.STM (atomically)
import qualified Data.Map as M
import Control.Lens ((^.), at, (?~))
import Data.Function ((&))

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust = forM_

enumFromTo' :: (Enum a, Ord a, Num a) => a -> a -> [a]
enumFromTo' x y
    | x < y = out
    | otherwise = reverse out
    where
    out = enumFromTo (min x y + 1) (max x y - 1)

enumFromToLR' :: (Enum a, Ord a) => a -> a -> [a]
enumFromToLR' x y
    | x < y = out
    | otherwise = reverse out
    where
    out = enumFromTo (min x y) (max x y)

enumFromToL' :: (Enum a, Ord a) => a -> a -> [a]
enumFromToL' x y
    | x < y = drop 1 out
    | otherwise = drop 1 $ reverse out
    where
    out = enumFromTo (min x y) (max x y)

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (x, y) (v, w) = max (abs $ x - v) (abs $ y - w)


withTMVarIO_ :: TMVar a -> (a -> IO a) -> IO ()
withTMVarIO_ var f = do
    val  <- atomically $ takeTMVar var
    val' <- f val
    atomically $ writeTMVar var val'

withTMVarIO :: TMVar a -> (a -> IO (a, b)) -> IO b
withTMVarIO var f = do
    val  <- atomically $ takeTMVar var
    (val', ret) <- f val
    atomically $ writeTMVar var val'
    return ret

{-
setdefault :: Ord k => k -> a -> M.Map k a -> (M.Map k a , a)
setdefault k a m = case m ^. at k of
    Nothing -> (m & at k ?~ a, a)
    Just a' -> (m, a')

findInsertM :: (Ord k, Monad m) => (a -> a -> a) -> k -> m a -> M.Map k a -> m (M.Map k a , a)
findInsertM f k ma m = case m ^. at k of
    Nothing -> ma >>= \a -> return (m & at k ?~ a, a)
    Just a' -> return (m, a')
-}

-- almost insertLookupWithKey except for not doing the effect if the key exists
setdefaultM :: (Ord k, Monad m) => k -> m a -> M.Map k a -> m (M.Map k a , a)
setdefaultM k ma m = case m ^. at k of
    Nothing -> ma >>= \a -> return (m & at k ?~ a, a)
    Just a' -> return (m, a')
