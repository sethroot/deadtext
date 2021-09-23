{-# LANGUAGE FlexibleContexts #-}

module Action.Walk where

import           Control.Error                  ( fromMaybe
                                                , headMay
                                                , hoistEither
                                                , runExceptT
                                                )
import           Control.Lens                   ( (.=)
                                                , (^.)
                                                , use
                                                )
import           Control.Monad.State.Lazy       ( MonadIO(..)
                                                , MonadState
                                                )
import           Data.List                      ( find )
import qualified Data.Map.Strict               as M
import           Parser                         ( parseDir )
import           Types

walkAction :: (MonadState Game m, MonadIO m) => [Input] -> m ()
walkAction inputs = do
    out <- walk inputs
    either printE printE out
    where printE = liftIO . putStrLn

walk :: (MonadState Game m, MonadIO m) => [Input] -> m (Either String String)
walk inputs = runExceptT $ do
    let input' = headMay inputs
    target <- case input' of
        Nothing     -> hoistEither $ Left "Go where?"
        Just target -> hoistEither $ Right target

    mDir <- parseDir $ target ^. normal
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
