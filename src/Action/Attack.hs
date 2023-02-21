{-# LANGUAGE FlexibleContexts #-}

module Action.Attack where

import Control.Error (MaybeT(runMaybeT), hoistEither, hoistMaybe, runExceptT)
import Control.Lens ((%~), Ixed(ix), (^.), use)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Lazy (MonadState(get, put))
import Data.List (elemIndex)
import Parser (parseRecM, parseNpcM)
import Safe (headMay)
import Types

attackAction :: (MonadState Game m, MonadIO m) => [Input] -> m ()
attackAction inputs = do
    out <- attack inputs
    either printE printE out
    where printE = liftIO . putStrLn

attack :: (MonadIO m, MonadState Game m) => [Input] -> m (Either String String)
attack inputs = runExceptT $ do
    let inputHead = headMay inputs
    target <- case inputHead of
        Nothing     -> hoistEither . Left $ "Attack what?"
        Just target -> hoistEither . Right $ target

    npc  <- parseRecM parseNpcM inputs
    npc' <- case npc of
        Nothing -> do
            let out = "You don't see " ++ target ^. normal ++ " here."
            hoistEither . Left $ out
        Just n -> hoistEither . Right $ n

    result <- attackMutation npc'
    _      <- case result of
        Nothing -> do
            let out = "Something has gone horribly wrong."
            hoistEither . Left $ out
        Just dmg -> hoistEither $ Right dmg

    avatar'   <- use avatar
    avatar''' <- case avatar' of
        Nothing -> do
            let out = "Something has gone horribly wrong."
            hoistEither . Left $ out
        Just avatar'' -> hoistEither $ Right avatar''

    let combatOut = avatar''' ^. combat . description $ npc'
    pure . mconcat $ [combatOut, "\n", npc' ^. name ++ " looks hurt."]

attackMutation :: MonadState Game m => Npc -> m (Maybe Int)
attackMutation targetNpc = runMaybeT $ do
    npcs' <- use npcs
    index <- case elemIndex targetNpc npcs' of
        Nothing -> hoistMaybe Nothing
        Just i  -> pure i
    game     <- get
    avatar'  <- use avatar
    avatar'' <- hoistMaybe avatar'
    let dmg = avatar'' ^. combat . damage
    put $ npcs . ix index . health %~ subtract dmg $ game
    pure dmg
    -- get >>= \g -> put $ npcs . ix index . health %~ subtract 10 $ g
