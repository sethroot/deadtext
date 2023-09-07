{-# LANGUAGE FlexibleContexts #-}

module Action.Inv (invAction) where

import Common (inventory)
import Control.Lens ((^.))
import Control.Monad.State.Lazy (MonadState)
import Types

invAction :: MonadState Game m
          => [Input]
          -> m (Either String String)
invAction _ = do
    inv <- inventory
    if null inv
        then pure . Right $ "Your pockets are empty."
        else pure . Right $ invSummary inv

invSummary :: [Item] -> String
invSummary = foldl combine _init
    where
        _init   = "You are holding: \n"
        combine = \xs _item -> xs ++ " - " ++ (_item ^. name) ++ "\n"
