{-# LANGUAGE FlexibleContexts #-}

module Action
    ( Action.Drop.dropAction
    , Action.Give.giveAction
    , Action.Help.helpAction
    , Action.Inv.invAction
    , Action.Kill.killAction
    , Action.Look.lookAction
    , Action.Open.openAction
    , Action.Close.closeAction
    , Action.Pickup.pickupAction
    , Action.Talk.talkAction
    , Action.Walk.walkAction
    ) where

import qualified Action.Close
import qualified Action.Drop
import qualified Action.Give
import qualified Action.Help
import qualified Action.Inv
import qualified Action.Kill
import qualified Action.Look
import qualified Action.Open
import qualified Action.Pickup
import qualified Action.Talk
import qualified Action.Walk
