{-# LANGUAGE FlexibleContexts #-}

module Action.Talk (talkAction) where

import Common (npcIsHere)
import Control.Error ((??), hoistEither, runExceptT)
import Control.Lens ((%=), Ixed(ix), (^.), use)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Lazy (MonadState)
import Data.List (elemIndex)
import Parser (parseNpcM, parseRecM)
import Types
import Util ((?))

talkAction :: (MonadState Game m, MonadIO m) => [Input] -> m ()
talkAction []                  = liftIO . putStrLn $ noTalkTarget
talkAction (Input _ "to" : xs) = processTalkTo xs
talkAction (target       : _ ) = processTalkTo [target]

processTalkTo :: (MonadState Game m, MonadIO m) => [Input] -> m ()
processTalkTo target = do
    out <- talkTo target
    either printE printE out
    where printE = liftIO . putStrLn

talkTo :: (MonadState Game m, MonadIO m) => [Input] -> m (Either String String)
talkTo inputs = runExceptT $ do
    -- todo: add support for talking to unknown npcs by gender, description
    let target = head inputs ^. raw
    npc      <- parseRecM parseNpcM inputs >>= (?? dontSeeTarget target)
    npcs'    <- use npcs
    index    <- elemIndex npc npcs' ?? dontSeeTarget (npc ^. name)
    npcHere' <- npcIsHere npc
    if not npcHere'
        then do
            let out = dontSeeTarget $ npc ^. name
            hoistEither $ Left out
        else hoistEither $ Right ()
    if not $ npc ^. alive
        then do
            let out = talkCorpse npc
            hoistEither $ Left out
        else do
            hoistEither $ Right ()
    let out = (npc ^. dialog) !! (npc ^. dialogCursor)
    advanceDialog index
    hoistEither $ Right out

noTalkTarget :: String
noTalkTarget = "You shout 'Hellllooooo?'"

dontSeeTarget :: String -> String
dontSeeTarget target = "You don't see " ++ target ++ " here."

talkCorpse :: Npc -> String
talkCorpse npc = npc ^. name ++ "'s corpse has nothing to say to you."

advanceDialog :: MonadState Game m => Int -> m ()
advanceDialog index = do
    npcs' <- use npcs
    let npc = npcs' !! index
    let dialog'       = npc ^. dialog
        dialogCursor' = npc ^. dialogCursor
        f             = (dialogCursor' < length dialog' - 1) ? (+ 1) $ const 0
    npcs . ix index . dialogCursor %= f
