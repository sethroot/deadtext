{-# LANGUAGE FlexibleContexts #-}

module Action.Open where

import           Control.Error                  ( fromMaybe )
import           Control.Lens                   ( (.=)
                                                , Ixed(ix)
                                                , (^.)
                                                , use
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.State.Lazy       ( MonadIO(..)
                                                , MonadState
                                                )
import           Data.List                      ( elemIndex )
import           Parsing                        ( parseContainer )
import           Types

openAction :: (MonadState Game m, MonadIO m) => Maybe Input -> m ()
openAction Nothing       = liftIO . putStrLn $ "Open what?"
openAction (Just target) = do
    out <- open target
    liftIO . putStrLn $ fromMaybe "Cant open that" out

open :: MonadState Game m => Input -> m (Maybe String)
open target = do
    container <- parseContainer $ target ^. normal
    case container of
        Nothing        -> pure . Just $ "You don't see a " ++ target ^. normal ++ "."
        Just container -> do
            if container ^. cState == Open
                then
                    pure
                    .  Just
                    $  "The "
                    ++ container
                    ^. name
                    ++ " is already open."
                else do
                    containers' <- use containers
                    case elemIndex container containers' of
                        Nothing -> pure Nothing
                        Just i  -> do
                            containers . ix i . cState .= Open
                            pure
                                .  Just
                                $  "You open the "
                                ++ container
                                ^. name
                                ++ "."
