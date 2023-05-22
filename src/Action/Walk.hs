{-# LANGUAGE FlexibleContexts #-}

module Action.Walk (walkAction) where

import Common (dontKnowHowToDoThat, outF)
import Control.Error ((??), fromMaybe, headMay, hoistEither, runExceptT)
import Control.Lens ((.=), (^.), use)
import Control.Monad.State.Lazy (MonadIO(..), MonadState)
import Data.List (find)
import qualified Data.Map.Strict as M
import Parser (parseDirM)
import Types

walkAction :: (MonadState Game m, MonadIO m) => [Input] -> m ()
walkAction inputs = do
    out <- walk inputs
    either printE printE out
    where printE = liftIO . putStrLn

walk :: (MonadState Game m, MonadIO m) => [Input] -> m (Either String String)
walk inputs = runExceptT $ do
    target     <- headMay inputs ?? goWhere
    mDir       <- parseDirM $ target ^. normal
    dir'       <- mDir ?? dontKnowHowToDoThat
    currentLoc <- use loc
    locMap     <- use locs
    conns'     <- use connections
    let next = resolveMove (Movement currentLoc dir') conns'
    loc .= next
    nextLoc <- M.lookup next locMap ?? outF
    let out = nextLoc ^. walkDesc
    hoistEither $ Right out

resolveMove :: Movement -> [Connection] -> UID
resolveMove move = fromMaybe (move ^. start) . maybeLocFromMove move

maybeLocFromMove :: Movement -> [Connection] -> Maybe UID
maybeLocFromMove move = fmap (^. dest) . maybeConnFromMove move

maybeConnFromMove :: Movement -> [Connection] -> Maybe Connection
maybeConnFromMove (Movement s d) = find predicate
    where
        predicate = \c ->
            let
                start' = c ^. start
                dir'   = c ^. dir
            in s == start' && d == dir'

goWhere :: String
goWhere = "Go where?"
