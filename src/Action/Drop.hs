{-# LANGUAGE FlexibleContexts #-}

module Action.Drop where

import Common (indefArt)
import Control.Error
    (MaybeT(runMaybeT), headMay, hoistEither, hoistMaybe, runExceptT)
import Control.Lens ((.=), Ixed(ix), (^.), use)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Lazy (MonadState)
import Data.List (elemIndex)
import Parser (parseInvItem)
import Types

dropAction :: (MonadState Game m, MonadIO m) => [Input] -> m ()
dropAction inputs = do
    out <- dropItem inputs
    either printE printE out
    where printE = liftIO . putStrLn

dropItem :: MonadState Game m => [Input] -> m (Either String String)
dropItem inputs = runExceptT $ do
    let input' = headMay inputs
    target <- case input' of
        Nothing     -> hoistEither $ Left "Drop what?"
        Just target -> hoistEither $ Right target
    itemM  <- parseInvItem $ target ^. normal

    item   <- case itemM of
        Nothing   -> do
            let out = dontHaveObject $ target ^. normal
            hoistEither $ Left out
        Just item -> pure item

    result <- dropMutation item
    case result of
        Nothing   -> do
            let
                out =
                    "Inexplicably, you are unable to drop your " ++ item ^. name
            hoistEither $ Left out
        Just item -> hoistEither $ Right ()

    pure . dropObject $ item ^. name

dropMutation :: MonadState Game m => Item -> m (Maybe ())
dropMutation item = runMaybeT $ do
    items' <- use items
    index  <- case elemIndex item items' of
        Nothing    -> hoistMaybe Nothing
        Just index -> pure index
    loc'   <- use loc
    items . ix index . loc .= ItemLoc loc'

dontHaveObject :: String -> String
dontHaveObject object =
    "You do not have " ++ indefArt object ++ " " ++ object ++ "."

dropObject :: String -> String
dropObject object = "You drop the " ++ object ++ "."
