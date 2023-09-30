{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Action.Drink (drinkAction) where

import Control.Monad.State.Lazy (MonadState)
import qualified Data.Text as T
import Types

drinkAction :: MonadState Game m => [Input] -> m (Either T.Text T.Text)
drinkAction _ = return . Right $ "drink not implemented"
