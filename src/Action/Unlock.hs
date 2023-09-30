{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Action.Unlock (unlockAction) where
import Action.Use
import Control.Monad.State.Lazy (MonadState)
import qualified Data.Text as T
import Parser (matchesText)
import Types

unlockAction :: MonadState Game m => [Input] -> m (Either T.Text T.Text)
unlockAction [] = pure . Right $ "Unlock what?"
unlockAction inputs
    | matchesText inputs "door with key" = Action.Use.useAction [Input "" "key"]
    | otherwise                          = pure . Right $ "what"


