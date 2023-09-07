{-# LANGUAGE FlexibleContexts #-}

module Action.Eat (eatAction) where

import Control.Monad.State.Lazy (MonadState)
import Types

eatAction :: MonadState Game m => [Input] -> m (Either String String)
eatAction _ = return . Right $ "eat not implemented"
