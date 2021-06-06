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
import           Data                           ( conns
                                                , walkDescMap
                                                )
import           Data.List                      ( find )
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( fromMaybe )
import           Msg
import           Parsing                        ( parseDir )
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
    loc' <- use loc
    let nextLoc = resolveMove $ Movement loc' dir
    loc .= nextLoc
    let out = walkDesc nextLoc
    hoistEither $ Right out

walkDesc :: Loc -> String
walkDesc loc = fromMaybe walkDescDefault $ M.lookup loc walkDescMap

resolveMove :: Movement -> Loc
resolveMove move = fromMaybe (move ^. start) $ maybeLocFromMove move

maybeLocFromMove :: Movement -> Maybe Loc
maybeLocFromMove = fmap (^. dest) . maybeConnFromMove

maybeConnFromMove :: Movement -> Maybe Connection
maybeConnFromMove (Movement s d) = find pred conns
  where
    pred = \c ->
        let start' = c ^. start
            dir'   = c ^. dir
        in  s == start' && d == dir'
