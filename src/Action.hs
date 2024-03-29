{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Action (processAction, Look.lookAction, Action(Action)) where

import qualified Action.Attack as Attack
import qualified Action.Close as Close
import qualified Action.Drink as Drink
import qualified Action.Drop as Drop
import qualified Action.Eat as Eat
import qualified Action.Give as Give
import qualified Action.Help as Help
import qualified Action.Inv as Inv
import qualified Action.Kill as Kill
import qualified Action.Look as Look
import qualified Action.Open as Open
import qualified Action.Pickup as Pickup
import qualified Action.Status as Status
import qualified Action.Talk as Talk
import qualified Action.Unlock as Unlock
import qualified Action.Use as Use
import qualified Action.Walk as Walk
import Common (dontKnowHowToDoThat)
import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO())
import Control.Monad.State (MonadState)
import qualified Data.Text as T
import Types
import Util (debugGameState, printE)

data Action = Action Input [Input]

processAction :: (MonadState Game m, MonadIO m) => Action -> m ()
processAction action@(Action input' _) = do
    case input' ^. normal of
        "debug" -> debugGameState
        "n"     -> go "n"
        "e"     -> go "e"
        "w"     -> go "w"
        "s"     -> go "s"
        "ne"    -> go "ne"
        "nw"    -> go "nw"
        "se"    -> go "se"
        "sw"    -> go "sw"
        "u"     -> go "u"
        "d"     -> go "d"
        _       -> processGameAction action
    where
        go dir' = processGameAction $ Action (Input "" "walk") [Input "" dir']

processGameAction :: (MonadState Game m, MonadIO m) => Action -> m ()
processGameAction (Action input' args) = do
    let action = mapAction input'
    out <- action args
    printE out

mapAction :: (MonadState Game m)
          => Input
          -> ([Input] -> m (Either T.Text T.Text))
mapAction input' = case input' ^. normal of
    "attack"    -> Attack.attackAction
    "close"     -> Close.closeAction
    "shut"      -> Close.closeAction
    "drink"     -> Drink.drinkAction
    "drop"      -> Drop.dropAction
    "leave"     -> Drop.dropAction
    "eat"       -> Eat.eatAction
    "examine"   -> Look.lookAction
    "give"      -> Give.giveAction
    "help"      -> Help.helpAction
    "i"         -> Inv.invAction
    "inv"       -> Inv.invAction
    "inventory" -> Inv.invAction
    "kill"      -> Kill.killAction
    "l"         -> Look.lookAction
    "look"      -> Look.lookAction
    "open"      -> Open.openAction
    "pickup"    -> Pickup.pickupAction
    "take"      -> Pickup.pickupAction
    "status"    -> Status.statusAction
    "talk"      -> Talk.talkAction
    "unlock"    -> Unlock.unlockAction
    "use"       -> Use.useAction
    "go"        -> Walk.walkAction
    "walk"      -> Walk.walkAction
    _           -> \_ -> pure . Right $ dontKnowHowToDoThat
