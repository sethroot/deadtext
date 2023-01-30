{-# LANGUAGE FlexibleContexts #-}

module Action.Inv (invAction) where

import Common (inventory)
import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Lazy (MonadState)
import Types

invAction :: (MonadState Game m, MonadIO m) => m ()
invAction = do
    inv <- inventory
    out <- if not (null inv)
        then pure $ invSummary inv
        else pure "Your pockets are empty."
    liftIO . putStrLn $ out

invSummary :: [Item] -> String
invSummary = foldl combine _init
    where
        _init    = "You are holding: \n"
        combine = \xs _item -> xs ++ " - " ++ (_item ^. name) ++ "\n"
