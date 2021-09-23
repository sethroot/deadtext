{-# LANGUAGE FlexibleContexts #-}

module Action.Open where

import           Control.Error                  ( headMay
                                                , hoistEither
                                                , runExceptT
                                                )
import           Control.Lens                   ( (.=)
                                                , Ixed(ix)
                                                , (^.)
                                                , use
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.State.Lazy       ( MonadState )
import           Data.List                      ( elemIndex )
import           Parser                         ( parseContainerM )
import           Types

openAction :: (MonadState Game m, MonadIO m) => [Input] -> m ()
openAction inputs = do
    out <- open inputs
    either printE printE out
    where printE = liftIO . putStrLn

open :: MonadState Game m => [Input] -> m (Either String String)
open inputs = runExceptT $ do
    let input' = headMay inputs
    target <- case input' of
        Nothing     -> hoistEither $ Left "Open what?"
        Just target -> hoistEither $ Right target

    container  <- parseContainerM $ target ^. normal
    container' <- case container of
        Nothing -> do
            -- This feels wrong when trying to open a non-container object
            -- Detect if target is object and provide a message that the
            -- object is not a container before resolving to this
            let out = "You don't see a " ++ target ^. normal ++ "."
            hoistEither $ Left out
        Just container -> hoistEither $ Right container

    if (container' ^. cState) == Open
        then do
            let out = "The " ++ container' ^. name ++ " is already open."
            hoistEither $ Left out
        else hoistEither $ Right ()

    containers' <- use containers
    index       <- case elemIndex container' containers' of
        Nothing -> do
            let out = "Can't open that."
            hoistEither $ Left out
        Just i -> hoistEither $ Right i

    containers . ix index . cState .= Open
    let out = "You open the " ++ container' ^. name ++ "."
    hoistEither $ Right out
