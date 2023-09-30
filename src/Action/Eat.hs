{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Action.Eat (eatAction) where

import Control.Monad.State.Lazy (MonadState)
import qualified Data.Text as T
import Types

eatAction :: MonadState Game m => [Input] -> m (Either T.Text T.Text)
eatAction _ = return . Right $ "eat not implemented"
