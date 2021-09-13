{-# LANGUAGE FlexibleContexts #-}

module Action.Walk where

import           Control.Error                  ( fromMaybe
                                                , hoistEither
                                                , runExceptT
                                                )
import           Control.Lens                   ( (.=)
                                                , (^.)
                                                , use
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.State.Lazy       ( MonadIO(..)
                                                , MonadState(get, put)
                                                , gets
                                                )
import           Data.List                      ( find )
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( fromMaybe )
import           Parser                         ( parseDir )
import           Types

walkAction :: (MonadState Game m, MonadIO m) => Maybe Input -> m ()
walkAction Nothing      = pure ()
walkAction (Just input) = do
    out <- walk input
    either printE printE out
    where printE = liftIO . putStrLn

walk :: (MonadState Game m, MonadIO m) => Input -> m (Either String String)
walk input = runExceptT $ do
    mDir <- parseDir $ input ^. normal
    dir  <- case mDir of
        Nothing -> do
            let out = "I don't know how to do that..."
            hoistEither $ Left out
        Just dir -> hoistEither $ Right dir
    currentLoc <- use loc
    locMap     <- use locs
    loc''      <- case M.lookup currentLoc locMap of
        Nothing -> hoistEither $ Left "Error: Could not lookup location"
        Just a  -> hoistEither $ Right a
    conns' <- use connections
    let resolvedNext = resolveMove (Movement currentLoc dir) conns'
    loc .= resolvedNext
    nextLoc <- case M.lookup resolvedNext locMap of
        Nothing ->
            hoistEither $ Left "Error: Could not lookup resolved next location"
        Just a -> hoistEither $ Right a
    let out = nextLoc ^. walkDesc
    hoistEither $ Right out

resolveMove :: Movement -> [Connection] -> UID
resolveMove move = fromMaybe (move ^. start) . maybeLocFromMove move

maybeLocFromMove :: Movement -> [Connection] -> Maybe UID
maybeLocFromMove move = fmap (^. dest) . maybeConnFromMove move

maybeConnFromMove :: Movement -> [Connection] -> Maybe Connection
maybeConnFromMove (Movement s d) = find pred
  where
    pred = \c ->
        let start' = c ^. start
            dir'   = c ^. dir
        in  s == start' && d == dir'
