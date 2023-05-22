{-# LANGUAGE FlexibleContexts #-}

module Action.Use where

import Control.Error ((!?), runExceptT)
import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Lazy (MonadState)
import Parser (parseInvItemM, parseRecM)
import Types

useAction :: (MonadState Game m, MonadIO m) => [Input] -> m ()
useAction inputs = do
    out <- use inputs
    either printE printE out
    where printE = liftIO . putStrLn

use :: MonadState Game m => [Input] -> m (Either String String)
use inputs = runExceptT $ do
    item' <- parseRecM parseInvItemM inputs !? doNotHaveItem
    pure . unwords $ ["You use the", item' ^. name]

doNotHaveItem :: String
doNotHaveItem = "You do not have that item."
