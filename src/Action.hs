{-# LANGUAGE FlexibleContexts #-}

module Action (processAction, Look.lookAction, Action(Action)) where

import qualified Action.Attack as Attack
import qualified Action.Close as Close
import qualified Action.Drop as Drop
import qualified Action.Give as Give
import qualified Action.Help as Help
import qualified Action.Inv as Inv
import qualified Action.Kill as Kill
import qualified Action.Look as Look
import qualified Action.Open as Open
import qualified Action.Pickup as Pickup
import qualified Action.Talk as Talk
import qualified Action.Walk as Walk
import Control.Lens ((^.))
import Control.Monad.State (MonadIO, MonadState)
import Types (Game, HasNormal(normal), Input)
import Util (debugGameState)

data Action = Action Input [Input]

processAction :: (MonadState Game m, MonadIO m) => Action -> m ()
processAction (Action action args) = case action ^. normal of
    "attack"    -> Attack.attackAction args
    "close"     -> Close.closeAction args
    "shut"      -> Close.closeAction args
    "drop"      -> Drop.dropAction args
    "leave"     -> Drop.dropAction args
    "give"      -> Give.giveAction args
    "help"      -> Help.helpAction
    "i"         -> Inv.invAction
    "inv"       -> Inv.invAction
    "inventory" -> Inv.invAction
    "kill"      -> Kill.killAction args
    "l"         -> Look.lookAction args
    "look"      -> Look.lookAction args
    "open"      -> Open.openAction args
    "pickup"    -> Pickup.pickupAction args
    "take"      -> Pickup.pickupAction args
    "talk"      -> Talk.talkAction args
    "go"        -> Walk.walkAction args
    "walk"      -> Walk.walkAction args
    "debug"     -> debugGameState
    _           -> Walk.walkAction [action]
