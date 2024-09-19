module Chess.Internal ( module Chess.Internal ) where

import Control.Monad (forM_)
import Control.Concurrent.STM.TMVar (TMVar, readTMVar, writeTMVar)
import Control.Concurrent.STM (atomically)

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


withTMVarIO :: TMVar a -> (a -> IO a) -> IO ()
withTMVarIO var f = do
    val  <- atomically $ readTMVar var
    val' <- f val
    atomically $ writeTMVar var val'