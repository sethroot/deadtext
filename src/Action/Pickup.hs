{-# LANGUAGE FlexibleContexts #-}

module Action.Pickup where

import           Common                         ( indefArt )
import           Control.Error                  ( headMay
                                                , hoistEither
                                                , runExceptT
                                                )
import           Control.Lens                   ( (.=)
                                                , Ixed(ix)
                                                , (^.)
                                                , use
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.State.Lazy       ( MonadState )
import           Data.Char                      ( toLower )
import           Data.List                      ( elemIndex )
import           Parser                         ( parseItemM )
import           Types

pickupAction :: (MonadState Game m, MonadIO m) => Maybe Input -> m ()
pickupAction Nothing      = liftIO . putStrLn $ "What do you want to pickup?"
pickupAction (Just input) = do
    out <- pickup input
    either printE printE out
    where printE = liftIO . putStrLn

dontSeeObject :: String -> String
dontSeeObject object =
    "You don't see " ++ indefArt object ++ " " ++ object ++ " here."

pickup :: (MonadState Game m) => Input -> m (Either String String)
pickup input = runExceptT $ do
    loc'        <- use loc
    containers' <- use containers
    items'      <- use items
    mItem       <- parseItemM $ input ^. normal
    item        <- case mItem of
        Nothing -> do
            let out = dontSeeObject $ input ^. raw
            hoistEither $ Left out
        Just item -> hoistEither $ Right item
    if item ^. loc == ItemLoc loc'
        then do
            pickupItemMutation item
            let out = (item ^. name) ++ " taken."
            hoistEither $ Left out
        else hoistEither $ Right ()

    let closedTransContainersHere = filter (hereTransClosed loc') containers'
    let itemsInClosedTransContainersHere =
            itemsInContainers items' closedTransContainersHere
    let inputInTransContainer =
            filter (inputMatchesItem input) itemsInClosedTransContainersHere
    case headMay inputInTransContainer of
        Nothing                -> hoistEither $ Right ()
        Just (item, container) -> do
            let out =
                    "You can see "
                        ++ indefArt (item ^. name)
                        ++ " "
                        ++ (item ^. name)
                        ++ " in the "
                        ++ (container ^. name)
                        ++ ", but the "
                        ++ (container ^. name)
                        ++ " is closed."
            hoistEither $ Left out

    let openContainersHere    = filter (openHere loc') containers'
    let itemsInOpenContainers = itemsInContainers items' openContainersHere
    let filtered              = filter (inputMatchesItem input) itemsInOpenContainers
    case headMay filtered of
        Nothing -> do
            let out = dontSeeObject $ input ^. normal
            hoistEither $ Left out
        Just (item, container) -> do
            pickupItemMutation item
            let out = takeItemFromContainer item container
            hoistEither $ Right out

openHere :: UID -> Container -> Bool
openHere loc' cont = (cont ^. loc == loc') && (cont ^. cState == Open)

hereTransClosed :: UID -> Container -> Bool
hereTransClosed loc' cont =
    (cont ^. loc == loc') && (cont ^. trans) && (cont ^. cState == Closed)

itemsInContainers :: [Item] -> [Container] -> [(Item, Container)]
itemsInContainers is cs =
    [ (i, c) | i <- is, c <- cs, i ^. loc == ItemContainer (c ^. uid) ]

inputMatchesItem :: Input -> (Item, Container) -> Bool
inputMatchesItem input (i, _) = fmap toLower (i ^. name) == input ^. normal

pickupItemMutation :: MonadState Game m => Item -> m ()
pickupItemMutation item = do
    items' <- use items
    case elemIndex item items' of
        Nothing -> pure ()
        Just i  -> items . ix i . loc .= ItemInv

takeItemFromContainer :: Item -> Container -> String
takeItemFromContainer item container =
    let i = item ^. name
        c = container ^. name
    in  "You take the " ++ i ++ " from the " ++ c ++ "."
