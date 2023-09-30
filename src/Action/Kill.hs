{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Action.Kill (killAction) where

import Common (npcIsHere, period)
import Control.Error (headMay, hoistEither, runExceptT)
import Control.Lens ((.=), Ixed(ix), (^.), use)
import Control.Monad.State.Lazy (MonadState)
import Data.List (elemIndex)
import qualified Data.Text as T
import Parser (parseNpc)
import Types
import Util (hoistL, hoistR)

killAction :: MonadState Game m => [Input] -> m (Either T.Text T.Text)
killAction inputs = runExceptT $ do
    let input' = headMay inputs
    target <- case input' of
        Nothing     -> hoistEither $ Left "Kill who?"
        Just target -> hoistEither $ Right target

    if target ^. normal == "self"
        then hoistL "You are dead. Congrats."
        else hoistR ()

    npcs' <- use npcs
    npc   <- case parseNpc npcs' $ target ^. normal of
        Nothing  -> hoistL $ dontSee target
        Just npc -> hoistR npc
    npcHere <- npcIsHere npc

    if not npcHere then hoistL $ dontSee target else hoistR ()

    if not (npc ^. alive) then hoistL $ alreadyDead npc else hoistR ()

    case elemIndex npc npcs' of
        Nothing    -> hoistL "Something went terrible wrong"
        Just index -> do
            npcs . ix index . alive .= False
            hoistR $ kill npc

dontSee :: Input -> T.Text
dontSee input' = period . T.unwords $ ["You don't see", input' ^. raw, "here"]

alreadyDead :: Npc -> T.Text
alreadyDead npc = period . T.unwords $ [npc ^. name, "is already dead"]

kill :: Npc -> T.Text
kill npc =
    period . T.unwords $ ["You kill", npc ^. name, "with your bare hands"]
