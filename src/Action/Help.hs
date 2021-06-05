{-# LANGUAGE FlexibleContexts #-}

module Action.Help where

import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.State.Lazy       ( MonadIO(..) )
import           Safe

helpAction :: MonadIO m => m ()
helpAction = do
    liftIO
        .  putStrLn
        $  "===================\n"
        ++ "Available commands:\n"
        ++ "===================\n"
        ++ "\n"
        ++ "  Look :: Use those skull holes.\n"
        ++ "  -----------------------------------------------------\n"
        ++ "    Look\n"
        ++ "    Look at [target]\n"
        ++ "\n"
        ++ "  Walk :: Where will you venture next?\n"
        ++ "  -----------------------------------------------------\n"
        ++ "    Walk North | North | N\n"
        ++ "    North | South | East | West | Up | Down\n"
        ++ "\n"
        ++ "  Talk :: If you absolutely must..\n"
        ++ "  -----------------------------------------------------\n"
        ++ "    Talk to [target]\n"
        ++ "\n"
        ++ "  Take :: Acquire stuff you probably won't need.\n"
        ++ "  -----------------------------------------------------\n"
        ++ "    Take [target]\n"
        ++ "    Pickup [target]\n"
        ++ "\n"
        ++ "  Drop :: Sometimes it's best to let things go.\n"
        ++ "  -----------------------------------------------------\n"
        ++ "    Drop [item]\n"
        ++ "    Leave [item]\n"
        ++ "\n"
        ++ "  Inventory :: Physics do not apply to your fanny pack.\n"
        ++ "  -----------------------------------------------------\n"
        ++ "    Inventory\n"
        ++ "    Inv\n"
        ++ "\n"
        ++ "  Give :: It's a quest thing.\n"
        ++ "  -----------------------------------------------------\n"
        ++ "    Give [item] to [target]\n"
        ++ "\n"
        ++ "  Kill :: The violence.\n"
        ++ "  -----------------------------------------------------\n"
        ++ "    Kill [target]\n"

