{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Action.Help (helpAction) where

import Control.Monad.State.Lazy (MonadState)
import qualified Data.Text as T
import Types

helpAction :: MonadState Game m => [Input] -> m (Either T.Text T.Text)
helpAction _ =
    pure
        .  Right
        $  "===================\n"
        <> "Available commands:\n"
        <> "===================\n"
        <> "\n"
        <> "  Look :: Let's use those skull holes.\n"
        <> "  -----------------------------------------------------\n"
        <> "    Look\n"
        <> "    Look at [target]\n"
        <> "\n"
        <> "  Walk :: Where to next?\n"
        <> "  -----------------------------------------------------\n"
        <> "    Walk North | North | N\n"
        <> "    North | South | East | West | Up | Down\n"
        <> "\n"
        <> "  Talk :: Maybe don't..\n"
        <> "  -----------------------------------------------------\n"
        <> "    Talk to [target]\n"
        <> "\n"
        <> "  Take :: Acquire stuff. You know, for later!\n"
        <> "  -----------------------------------------------------\n"
        <> "    Take [target]\n"
        <> "    Pickup [target]\n"
        <> "\n"
        <> "  Drop :: You're carrying garbage! Get rid of it.\n"
        <> "  -----------------------------------------------------\n"
        <> "    Drop [item]\n"
        <> "    Leave [item]\n"
        <> "\n"
        <> "  Inventory :: Physics will not apply.\n"
        <> "  -----------------------------------------------------\n"
        <> "    Inventory\n"
        <> "    Inv\n"
        <> "\n"
        <> "  Give :: The atomic unit of quest-advancing.\n"
        <> "  -----------------------------------------------------\n"
        <> "    Give [item] to [target]\n"
        <> "\n"
        <> "  Kill :: In here, violence is a tool.\n"
        <> "  -----------------------------------------------------\n"
        <> "    Kill [target]\n"
