{-# LANGUAGE FlexibleContexts #-}

module Action.Drop (dropAction) where

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
    itemM      <- parseInvItem $ target ^. normal

    targetItem <- case itemM of
        Nothing -> do
            let out = dontHaveObject $ target ^. normal
            hoistEither $ Left out
        Just i -> pure i

    result <- dropMutation targetItem
    case result of
        Nothing -> do
            let out = cantDrop $ targetItem ^. name
            hoistEither $ Left out
        Just _ -> hoistEither $ Right ()

    pure . dropObject $ targetItem ^. name

dropMutation :: MonadState Game m => Item -> m (Maybe ())
dropMutation targetItem = runMaybeT $ do
    items' <- use items
    index  <- case elemIndex targetItem items' of
        Nothing -> hoistMaybe Nothing
        Just i  -> pure i
    loc' <- use loc
    items . ix index . loc .= ItemLoc loc'

cantDrop :: String -> String
cantDrop object = "Inexplicably, you are unable to drop your " ++ object ++ "."

dontHaveObject :: String -> String
dontHaveObject object =
    "You do not have " ++ indefArt object ++ " " ++ object ++ "."

dropObject :: String -> String
dropObject object = "You drop the " ++ object ++ "."
