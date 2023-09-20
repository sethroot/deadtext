{-# LANGUAGE FlexibleContexts #-}

module Action.Unlock (unlockAction) where
import Control.Monad.State.Lazy (MonadState)
import Types
import Util (hoistR)
import Control.Error (runExceptT)

unlockAction :: MonadState Game m => [Input] -> m (Either String String)
unlockAction _ = runExceptT $ do
    hoistR "undefined"
