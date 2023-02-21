{-# LANGUAGE FlexibleContexts #-}

module Action.Talk (talkAction) where

import Common (npcIsHere)
import Control.Error (hoistEither, runExceptT)
import Control.Lens ((%=), Ixed(ix), (^.), use)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Lazy (MonadState)
import Data.Char (toLower)
import Data.List (elemIndex, findIndex)
import qualified Data.Map.Strict as M
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
    npcs' <- use npcs
    -- todo: add support for talking to unknown npcs by gender, description

    npc   <- parseRecM parseNpcM inputs
    npc'  <- case npc of
        Nothing -> do
            let input' = head inputs
            let out    = dontSeeTarget (input' ^. raw)
            hoistEither . Left $ out
        Just npc' -> hoistEither . Right $ npc'

    index <- case elemIndex npc' npcs' of
        Nothing -> do
            let out = dontSeeTarget (npc' ^. name)
            hoistEither . Left $ out
        Just i -> hoistEither . Right $ i

    npcHere' <- npcIsHere npc'
    if not npcHere'
        then do
            let out = dontSeeTarget $ npc' ^. name
            hoistEither $ Left out
        else hoistEither $ Right ()
    if not $ npc' ^. alive
        then do
            let out = talkCorpse npc'
            hoistEither $ Left out
        else do
            hoistEither $ Right ()
    let out = (npc' ^. dialog) !! (npc' ^. dialogCursor)
    advanceDialog index
    hoistEither $ Right out

noTalkTarget :: String
noTalkTarget = "You shout 'Hellllooooo?'"

nameMatches :: Input -> Npc -> Bool
nameMatches input npc = fmap toLower (npc ^. name) == input ^. normal

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

npcAlive :: M.Map Npc Bool -> Npc -> Bool
npcAlive map npc = case M.lookup npc map of
    (Just b) -> b
    _        -> True
