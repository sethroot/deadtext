{-# LANGUAGE FlexibleContexts #-}

module Action.Talk where

import           Common                         ( npcIsHere )
import           Control.Error                  ( hoistEither
                                                , runExceptT
                                                )
import           Control.Lens                   ( (%=)
                                                , Ixed(ix)
                                                , (^.)
                                                , use
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.State.Lazy       ( MonadIO(..)
                                                , MonadState
                                                )
import           Data.Char                      ( toLower )
import           Data.List                      ( findIndex )
import qualified Data.Map.Strict               as M
import           Types
import           Util                           ( (?) )

talkAction :: (MonadState Game m, MonadIO m) => Maybe Input -> m ()
talkAction Nothing      = pure ()
talkAction (Just input) = do
    out <- talkTo input
    either printE printE out
    where printE = liftIO . putStrLn

talkTo :: (MonadState Game m, MonadIO m) => Input -> m (Either String String)
talkTo input = runExceptT $ do
    npcs' <- use npcs
    index <- case findIndex (nameMatches input) npcs' of
        Nothing -> do
            let out = dontSeeTarget (input ^. raw)
            hoistEither $ Left out
        Just i -> hoistEither $ Right i
    let npc = npcs' !! index
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
