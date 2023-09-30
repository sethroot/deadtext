{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Action.Attack where

import Common (period)
import Control.Error ((??), MaybeT(runMaybeT), hoistMaybe, runExceptT)
import Control.Lens ((%=), Ixed(ix), (^.), use)
import Control.Monad.State.Lazy (MonadState)
import Data.List (elemIndex, intersperse)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Parser (parseNpcM, parseRecM)
import Safe (headMay)
import Types

attackAction :: MonadState Game m => [Input] -> m (Either T.Text T.Text)
attackAction inputs = runExceptT $ do
    target  <- headMay inputs ?? attackWhat
    npc     <- parseRecM parseNpcM inputs >>= (?? dontSee target)
    avatar' <- fromJust <$> use avatar
    _       <- attackMutation npc
    pure
        . mconcat
        . intersperse "\n\n"
        $ [avatar' ^. combat . description $ npc, looksHurt npc]

attackMutation :: MonadState Game m => Npc -> m (Maybe Int)
attackMutation targetNpc = runMaybeT $ do
    npcs' <- use npcs
    index <- case elemIndex targetNpc npcs' of
        Nothing -> hoistMaybe Nothing
        Just i  -> pure i
    avatarM <- use avatar
    avatar' <- hoistMaybe avatarM
    let dmg = avatar' ^. combat . damage
    npcs . ix index . health %= subtract dmg
    pure dmg

attackWhat :: T.Text
attackWhat = "Attack what?"

dontSee :: Input -> T.Text
dontSee target =
    period . T.unwords $ ["You don't see ", target ^. normal, "here"]

looksHurt :: Npc -> T.Text
looksHurt npc = period . T.unwords $ [npc ^. name, "looks hurt"]
