{-# LANGUAGE FlexibleContexts #-}

module Action.Kill (killAction) where

import Common (npcIsHere)
import Control.Error (headMay, hoistEither, runExceptT)
import Control.Lens ((.=), Ixed(ix), (^.), use)
import Control.Monad.State.Lazy (MonadState)
import Data.List (elemIndex)
import Parser (parseNpc)
import Types
import Util (hoistL, hoistR)

killAction :: MonadState Game m => [Input] -> m (Either String String)
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

dontSee :: Input -> String
dontSee _input = "You don't see " ++ input' ++ " here."
    where input' = _input ^. raw

alreadyDead :: Npc -> String
alreadyDead npc = npc' ++ " is already dead." where npc' = npc ^. name

kill :: Npc -> String
kill npc = "You kill " ++ npc' ++ " with your bare hands."
    where npc' = npc ^. name
