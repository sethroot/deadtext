{-# LANGUAGE FlexibleContexts #-}

module Action
    ( processAction
    , Look.lookAction
    ) where

import qualified Action.Close                  as Close
import qualified Action.Drop                   as Drop
import qualified Action.Give                   as Give
import qualified Action.Help                   as Help
import qualified Action.Inv                    as Inv
import qualified Action.Kill                   as Kill
import qualified Action.Look                   as Look
import qualified Action.Open                   as Open
import qualified Action.Pickup                 as Pickup
import qualified Action.Talk                   as Talk
import qualified Action.Walk                   as Walk
import           Control.Lens                   ( (^.) )
import           Control.Monad.State            ( MonadIO
                                                , MonadState
                                                )
import           Types                          ( Game
                                                , HasNormal(normal)
                                                , Input
                                                )
import           Util                           ( debugGameState )

processAction :: (MonadState Game m, MonadIO m)
              => Input
              -> Maybe Input
              -> [Input]
              -> m ()
processAction action arg args = case action ^. normal of
    "close"     -> Close.closeAction arg
    "shut"      -> Close.closeAction arg
    "drop"      -> Drop.dropAction arg
    "leave"     -> Drop.dropAction arg
    "give"      -> Give.giveAction args
    "help"      -> Help.helpAction
    "inv"       -> Inv.invAction
    "inventory" -> Inv.invAction
    "kill"      -> Kill.killAction arg
    "look"      -> Look.lookAction args
    "open"      -> Open.openAction arg
    "pickup"    -> Pickup.pickupAction arg
    "take"      -> Pickup.pickupAction arg
    "talk"      -> Talk.talkAction args
    "go"        -> Walk.walkAction arg
    "walk"      -> Walk.walkAction arg
    "debug"     -> debugGameState
    _           -> Walk.walkAction $ Just action
