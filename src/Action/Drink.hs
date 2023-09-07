{-# LANGUAGE FlexibleContexts #-}

module Action.Drink (drinkAction) where

import Control.Monad.State.Lazy (MonadState)
import Types

drinkAction :: MonadState Game m => [Input] -> m (Either String String)
drinkAction _ = return . Right $ "drink not implemented"
