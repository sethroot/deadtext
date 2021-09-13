{-# LANGUAGE FlexibleContexts #-}

module Action.Drop where

import           Control.Error                  ( MaybeT(runMaybeT)
                                                , fromMaybe
                                                , hoistMaybe
                                                )
import           Control.Lens                   ( (.=)
                                                , Ixed(ix)
                                                , (^.)
                                                , use
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.State.Lazy       ( MonadState )
import           Data.List                      ( elemIndex )
import           Parser                         ( parseInvItem )
import           Types

dropAction :: (MonadState Game m, MonadIO m) => Maybe Input -> m ()
dropAction Nothing      = pure ()
dropAction (Just input) = do
    let target = input ^. normal
    message <- dropItem target
    liftIO . putStrLn $ fromMaybe (dontHaveObject (input ^. raw)) message

dropItem :: MonadState Game m => String -> m (Maybe String)
dropItem input = runMaybeT $ do
    parsed <- parseInvItem input
    item   <- case parsed of
        Nothing   -> hoistMaybe Nothing
        Just item -> pure item
    dropMutation item
    pure . dropObject $ item ^. name

dropMutation :: MonadState Game m => Item -> m (Maybe ())
dropMutation item = runMaybeT $ do
    items' <- use items
    index  <- case elemIndex item items' of
        Nothing    -> hoistMaybe Nothing
        Just index -> pure index
    loc' <- use loc
    items . ix index . loc .= ItemLoc loc'

dontHaveObject :: String -> String
dontHaveObject object = "You do not have a " ++ object ++ "."

dropObject :: String -> String
dropObject object = "You drop the " ++ object ++ "."
