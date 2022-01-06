{-# LANGUAGE FlexibleContexts #-}

module Action.Close where

import Control.Error (hoistEither, runExceptT)
import Control.Lens ((.=), Ixed(ix), (^.), use)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Lazy (MonadState)
import Data.List (elemIndex)
import Parser (parseContainerM)
import Safe (headMay)
import Types

closeAction :: (MonadState Game m, MonadIO m) => [Input] -> m ()
closeAction inputs = do
    out <- close inputs
    either printE printE out
    where printE = liftIO . putStrLn

close :: MonadState Game m => [Input] -> m (Either String String)
close inputs = runExceptT $ do
    let head = headMay inputs
    target     <- case head of
        Nothing     -> hoistEither $ Left "Close what?"
        Just target -> hoistEither $ Right target

    container  <- parseContainerM $ target ^. normal
    container' <- case container of
        Nothing        -> do
            let out = "You don't see a " ++ target ^. normal ++ "."
            hoistEither $ Left out
        Just container -> hoistEither $ Right container

    if (container' ^. cState) == Closed
        then do
            let out = "The " ++ container' ^. name ++ " is already closed."
            hoistEither $ Left out
        else hoistEither $ Right ()

    containers' <- use containers
    index       <- case elemIndex container' containers' of
        Nothing -> do
            let out = "Can't close that."
            hoistEither $ Left out
        Just i  -> hoistEither $ Right i

    containers . ix index . cState .= Closed
    let out = "You close the " ++ container' ^. name ++ "."
    hoistEither $ Right out
