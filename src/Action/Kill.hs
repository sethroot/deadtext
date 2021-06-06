{-# LANGUAGE FlexibleContexts #-}

module Action.Kill where

import           Common                         ( npcIsHere )
import           Control.Error                  ( hoistEither
                                                , runExceptT
                                                )
import           Control.Lens                   ( (.=)
                                                , Ixed(ix)
                                                , (^.)
                                                , use
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.State.Lazy       ( MonadIO(..)
                                                , MonadState
                                                )
import           Data.List                      ( elemIndex )
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                )
import           Parsing                        ( parseNpc )
import           System.Exit                    ( exitSuccess )
import           Types
import           Util                           ( (?) )

killAction :: (MonadState Game m, MonadIO m) => Maybe Input -> m ()
killAction Nothing = liftIO . putStrLn $ "Kill what?"
killAction input
    | fromJust input ^. normal == "self" = do
        liftIO $ putStrLn "You are dead. Congrats"
        liftIO exitSuccess
    | otherwise = do
        out <- killTarget $ fromJust input
        either printE printE out
    where printE = liftIO . putStrLn

killTarget :: MonadState Game m => Input -> m (Either String String)
killTarget target = runExceptT $ do
    npcs' <- use npcs
    npc   <- case parseNpc npcs' $ target ^. normal of
        Nothing  -> hoistEither $ Left $ dontSee target
        Just npc -> hoistEither $ Right npc
    npcHere <- npcIsHere npc

    if not npcHere
        then hoistEither $ Left $ dontSee target
        else hoistEither $ Right ()

    if not (npc ^. alive)
        then hoistEither . Left $ alreadyDead npc
        else hoistEither $ Right ()

    case elemIndex npc npcs' of
        Nothing    -> hoistEither . Left $ "Something went terrible wrong"
        Just index -> do
            npcs . ix index . alive .= False
            hoistEither $ Right $ kill npc

dontSee :: Input -> String
dontSee input = "You don't see " ++ input' ++ " here."
    where input' = input ^. raw

alreadyDead :: Npc -> String
alreadyDead npc = npc' ++ " is already dead." where npc' = npc ^. name

kill :: Npc -> String
kill npc = "You kill " ++ npc' ++ " with your bare hands."
    where npc' = npc ^. name
