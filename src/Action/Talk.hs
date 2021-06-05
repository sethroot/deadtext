{-# LANGUAGE FlexibleContexts #-}

module Action.Talk where

import           Common                         ( npcIsHere )
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
import           Msg
import           Types
import           Util                           ( (?) )

talkAction :: (MonadState Game m, MonadIO m) => Maybe Input -> m ()
talkAction Nothing       = pure ()
talkAction (Just target) = do
    npcs' <- use npcs
    let
        index = findIndex
            (\x -> fmap toLower (x ^. name) == (target ^. normal))
            npcs'
    case index of
        Nothing -> liftIO . putStrLn $ dontSeeTarget (target ^. raw)
        Just i  -> do
            talkTo i

talkTo :: (MonadState Game m, MonadIO m) => Int -> m ()
talkTo index = do
    npcs' <- use npcs
    let npc = npcs' !! index
    liftIO $ print npc
    npcHere' <- npcIsHere npc
    liftIO $ print npcHere'
    if not npcHere'
        then liftIO . putStrLn . dontSeeTarget $ npc ^. name
        else do
            if not $ npc ^. alive
                then liftIO . putStrLn $ talkCorpse npc
                else do
                    liftIO . putStrLn $ (npc ^. dialog) !! (npc ^. dialogCursor)
                    advanceDialog index

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
