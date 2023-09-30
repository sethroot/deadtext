{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Action.Status (statusAction) where

import Common (outF)
import Control.Error (runExceptT)
import Control.Lens ((%=), (^?))
import Control.Lens.Prism (_Just)
import Control.Monad.State.Lazy (MonadState(get))
import qualified Data.Text as T
import Types
import Util (hoistR)

statusAction :: MonadState Game m => [Input] -> m (Either T.Text T.Text)
statusAction _ = runExceptT $ do
    game <- get
    let health' = game ^? avatar . _Just . health
    let out     = maybe outF healthStatus health'
    -- TODO: remove, for testing only.
    avatar . _Just . health %= (\n -> n - 20)
    hoistR out

healthStatus :: Int -> T.Text
healthStatus n | n > 90 = "You feel fine."
healthStatus n | n > 70 = "You are hurt."
healthStatus n | n > 50 = "You are injured."
healthStatus n | n > 30 = "You feel very weak."
healthStatus n | n > 10 = "You are close to death."
healthStatus _          = "You are dead."
