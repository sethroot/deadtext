{-# LANGUAGE FlexibleContexts #-}

module Util where

import Control.Monad.State (MonadIO(liftIO), MonadState(get))
import Data.Char (toLower)
import qualified Data.Map.Strict as M
import Types (Game)

printE :: MonadIO m => Either String String -> m ()
printE = either go go
    where go = liftIO . putStrLn

invert :: (Ord v) => M.Map k [v] -> M.Map v [k]
invert m = M.fromListWith (++) pairs
    where pairs = [ (v, [k]) | (k, vs) <- M.toList m, v <- vs ]

fromBool :: Bool -> a -> Maybe a
fromBool False = const Nothing
fromBool True  = Just

(?) :: Bool -> a -> a -> a
(?) True  x _ = x
(?) False _ y = y

maybeToEither :: Maybe a -> Either () a
maybeToEither Nothing  = Left ()
maybeToEither (Just a) = Right a

lowEq :: (Functor f, Eq (f Char)) => f Char -> f Char -> Bool
lowEq a b = (toLower <$> a) == (toLower <$> b)

enumerate :: [a] -> [(Integer, a)]
enumerate = zip [0 ..]

debugGameState :: (MonadState Game m, MonadIO m) => m ()
debugGameState = do
    game <- get
    liftIO $ printGame game

printGame :: Game -> IO ()
printGame g = do
    putStrLn ""
    -- pPrint g
    putStrLn ""

partitionBy :: Eq a => a -> [a] -> [[a]]
partitionBy sep l = partitionByAux l []
    where
        partitionByAux (h : t) acc | sep == h = acc : partitionByAux t []
        partitionByAux (h : t) acc            = partitionByAux t (acc `snoc` h)
        partitionByAux []      acc            = [acc]

snoc :: [a] -> a -> [a]
snoc []      a = [a]
snoc (h : t) a = h : t `snoc` a
