{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Action.Inv (invAction) where

import Common (inventory)
import Control.Lens ((^.))
import Control.Monad.State.Lazy (MonadState)
import qualified Data.Text as T
import Types

invAction :: MonadState Game m => [Input] -> m (Either T.Text T.Text)
invAction _ = do
    inv <- inventory
    if null inv
        then pure . Right $ "Your pockets are empty."
        else pure . Right $ invSummary inv

invSummary :: [Item] -> T.Text
invSummary = foldl combine _init
    where
        _init   = "You are holding: \n"
        combine = \xs _item -> xs <> " - " <> (_item ^. name) <> "\n"
