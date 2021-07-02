{-# LANGUAGE FlexibleContexts #-}

module Action.Close where

import           Control.Error                  ( hoistEither
                                                , runExceptT
                                                )
import           Control.Lens                   ( (.=)
                                                , Ixed(ix)
                                                , (^.)
                                                , use
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.State.Lazy       ( MonadState )
import           Control.Monad.Trans.Except     ( runExceptT )
import           Data.List                      ( elemIndex )
import           Parsing                        ( parseContainer )
import           Types

closeAction :: (MonadState Game m, MonadIO m) => Maybe Input -> m ()
closeAction Nothing       = liftIO . putStrLn $ "Close what?"
closeAction (Just target) = do
    out <- close target
    either printE printE out
    where printE = liftIO . putStrLn

close :: MonadState Game m => Input -> m (Either String String)
close target = runExceptT $ do
    container  <- parseContainer $ target ^. normal

    container' <- case container of
        Nothing -> do
            let out = "You don't see a " ++ target ^. normal ++ "."
            hoistEither $ Left out
        Just container -> hoistEither $ Right container

    if (container' ^. cState) == Closed
        then do
            let out = "The " ++ container' ^. name ++ " is already open"
            hoistEither $ Left out
        else hoistEither $ Right ()

    containers' <- use containers
    index       <- case elemIndex container' containers' of
        Nothing -> do
            let out = "Can't close that."
            hoistEither $ Left out
        Just i -> hoistEither $ Right i

    containers . ix index . cState .= Closed
    let out = "You close the " ++ container' ^. name ++ "."
    hoistEither $ Right out
