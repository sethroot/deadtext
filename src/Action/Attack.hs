{-# LANGUAGE FlexibleContexts #-}

module Action.Attack where

import Control.Error ((??), MaybeT(runMaybeT), hoistMaybe, runExceptT)
import Control.Lens ((%~), Ixed(ix), (^.), use)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Lazy (MonadState(get, put))
import Data.Function ((&))
import Data.List (elemIndex, intersperse)
import Data.Maybe (fromJust)
import Parser (parseNpcM, parseRecM)
import Safe (headMay)
import Types

attackAction :: (MonadState Game m, MonadIO m) => [Input] -> m ()
attackAction inputs = do
    out <- attack inputs
    either printE printE out
    where printE = liftIO . putStrLn

attack :: (MonadIO m, MonadState Game m) => [Input] -> m (Either String String)
attack inputs = runExceptT $ do
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
    game    <- get
    avatarM <- use avatar
    avatar' <- hoistMaybe avatarM
    let dmg = avatar' ^. combat . damage
    _ <- put $ game & npcs . ix index . health %~ subtract dmg
    pure dmg

attackWhat :: String
attackWhat = "Attack what?"

dontSee :: Input -> String
dontSee target = "You don't see " ++ target ^. normal ++ " here."

looksHurt :: Npc -> String
looksHurt npc = npc ^. name ++ " looks hurt."
