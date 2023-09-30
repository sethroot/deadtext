{-# LANGUAGE FlexibleContexts #-}

module Util where

import Control.Error (hoistEither)
import Control.Monad.State (MonadIO(liftIO), MonadState())
import Control.Monad.Trans.Except (ExceptT)
import qualified Data.Map.Strict as M
import qualified Data.Text as T 
import Types (Game)

hoistL :: Monad m => e -> ExceptT e m a
hoistL = hoistEither . Left

hoistR :: Monad m => a -> ExceptT e m a
hoistR = hoistEither . Right

printE :: MonadIO m => Either T.Text T.Text -> m ()
printE = either go go where go = liftIO . putStrLn . T.unpack

invert :: (Ord v) => M.Map k [v] -> M.Map v [k]
invert m = M.fromListWith (++) pairs
    where pairs = [ (v, [k]) | (k, vs) <- M.toList m, v <- vs ]

fromBool :: Bool -> a -> Maybe a
fromBool False = const Nothing
fromBool True  = Just

(?) :: Bool -> a -> a -> a
(?) True  x _ = x
(?) False _ y = y

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing  = Left e
maybeToEither _ (Just a) = Right a

lowEq :: T.Text -> T.Text -> Bool
lowEq a b = T.toLower a == T.toLower b

enumerate :: [a] -> [(Integer, a)]
enumerate = zip [0 ..]

debugGameState :: (MonadState Game m, MonadIO m) => m ()
debugGameState = do
    pure ()
    -- game <- get
    -- liftIO $ printGame game

-- printGame :: Game -> IO ()
-- printGame g = do
--     putStrLn ""
--     -- pPrint g
--     putStrLn ""

partitionBy :: Eq a => a -> [a] -> [[a]]
partitionBy sep l = partitionByAux l []
    where
        partitionByAux (h : t) acc | sep == h = acc : partitionByAux t []
        partitionByAux (h : t) acc            = partitionByAux t (acc `snoc` h)
        partitionByAux []      acc            = [acc]

snoc :: [a] -> a -> [a]
snoc []      a = [a]
snoc (h : t) a = h : t `snoc` a
