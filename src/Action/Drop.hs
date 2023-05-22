{-# LANGUAGE FlexibleContexts #-}

module Action.Drop (dropAction) where

import Common (indefArt, period)
import Control.Error
    ((??), MaybeT(runMaybeT), headMay, hoistMaybe, runExceptT)
import Control.Lens ((.=), Ixed(ix), (^.), use)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Lazy (MonadState)
import Data.List (elemIndex)
import Parser (parseInvItemM)
import Types

dropAction :: (MonadState Game m, MonadIO m) => [Input] -> m ()
dropAction inputs = do
    out <- dropItem inputs
    either printE printE out
    where printE = liftIO . putStrLn

dropItem :: MonadState Game m => [Input] -> m (Either String String)
dropItem inputs = runExceptT $ do
    target     <- headMay inputs ?? dropWhat
    itemM      <- parseInvItemM $ target ^. normal
    targetItem <- itemM ?? dontHaveObject (target ^. normal)
    let itemName = targetItem ^. name
    result <- dropMutation targetItem
    _      <- result ?? cantDrop itemName
    pure . dropObject $ itemName

dropMutation :: MonadState Game m => Item -> m (Maybe ())
dropMutation targetItem = runMaybeT $ do
    items' <- use items
    index  <- case elemIndex targetItem items' of
        Nothing -> hoistMaybe Nothing
        Just i  -> pure i
    loc' <- use loc
    items . ix index . loc .= ItemLoc loc'

dropWhat :: String
dropWhat = "Drop what?"

cantDrop :: String -> String
cantDrop object =
    period . unwords $ ["Inexplicably, you are unable to drop your", object]

dontHaveObject :: String -> String
dontHaveObject object =
    period . unwords $ ["You do not have", indefArt object, object]

dropObject :: String -> String
dropObject object = period . unwords $ ["You drop the", object]
