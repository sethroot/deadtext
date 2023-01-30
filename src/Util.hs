{-# LANGUAGE FlexibleContexts #-}

module Util where

import Control.Monad.State (MonadIO(..), MonadState(get))
import Data.Char (toLower)
import qualified Data.Map.Strict as M
import Text.Pretty.Simple (pPrint)
import Types (Game)

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
    pPrint g
    putStrLn ""
