{-# LANGUAGE FlexibleContexts #-}

module Action.Inv where

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
invSummary = foldl combine init
    where
        init    = "You are holding: \n"
        combine = \xs item -> xs ++ " - " ++ (item ^. name) ++ "\n"
