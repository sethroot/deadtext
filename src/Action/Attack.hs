{-# LANGUAGE FlexibleContexts #-}

module Action.Attack where

import Control.Error (hoistEither, runExceptT)
import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Lazy (MonadState)
import Parser (parseNpcRecM)
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

    npc <- parseNpcRecM inputs 
    npc'   <- case npc of
        Nothing -> do
            let out = "You don't see " ++ target ^. normal ++ " here."
            hoistEither . Left $ out
        Just n -> hoistEither . Right $ n


    pure $ "You attack " ++ npc' ^. name ++ " with your bare hands."
