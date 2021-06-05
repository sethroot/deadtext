{-# LANGUAGE FlexibleContexts #-}

module Util where

import           Control.Lens                   ( (%=)
                                                , use
                                                )
import           Control.Monad.State.Lazy       ( MonadIO(..)
                                                , MonadState(get)
                                                )
import qualified Data.Map.Strict               as M
import           Types                          ( Game
                                                , HasUidGen(uidGen)
                                                )

dumpGameState :: (MonadState Game m, MonadIO m) => m ()
dumpGameState = do
    game <- get
    liftIO $ printGame game

printGame :: Game -> IO ()
printGame g = do
    putStrLn ""
    print g
    putStrLn ""

dumpInputs :: [String] -> IO ()
dumpInputs = print . zip [0 ..]

invert :: (Ord v) => M.Map k [v] -> M.Map v [k]
invert m = M.fromListWith (++) pairs
    where pairs = [ (v, [k]) | (k, vs) <- M.toList m, v <- vs ]

fromBool :: Bool -> a -> Maybe a
fromBool False = const Nothing
fromBool True  = Just

(?) :: Bool -> a -> a -> a
(?) True  x _ = x
(?) False _ y = y

genUid :: (MonadState Game m) => m Int
genUid = do
    uid <- use uidGen
    uidGen %= (+ 1)
    pure uid
