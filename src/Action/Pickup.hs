{-# LANGUAGE FlexibleContexts #-}

module Action.Pickup (pickupAction) where

import Common (indefArt)
import Control.Error (headMay, hoistEither, runExceptT)
import Control.Lens ((.=), Ixed(ix), (^.), use)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Lazy (MonadState)
import Data.Char (toLower)
import Data.List (elemIndex)
import Parser (parseItemM)
import Types

pickupAction :: (MonadState Game m, MonadIO m) => [Input] -> m ()
pickupAction inputs = do
    out <- pickup inputs
    either printE printE out
    where printE = liftIO . putStrLn

pickup :: (MonadState Game m) => [Input] -> m (Either String String)
pickup inputs = runExceptT $ do
    let input' = headMay inputs
    target <- case input' of
        Nothing     -> hoistEither $ Left "Pickup what?"
        Just target -> hoistEither $ Right target

    loc'        <- use loc
    containers' <- use containers
    items'      <- use items
    mItem       <- parseItemM $ target ^. normal
    item        <- case mItem of
        Nothing -> do
            let out = dontSeeObject $ target ^. raw
            hoistEither $ Left out
        Just item -> hoistEither $ Right item
    if item ^. loc == ItemLoc loc'
        then do
            pickupItemMutation item
            let out = (item ^. name) ++ " taken."
            hoistEither $ Left out
        else hoistEither $ Right ()

    let closedTransContainersHere = filter (closedTransHere loc') containers'
    let
        itemsInClosedTransContainersHere =
            itemsInContainers items' closedTransContainersHere
    let
        inputInTransContainer =
            filter (inputMatchesItem target) itemsInClosedTransContainersHere
    case headMay inputInTransContainer of
        Nothing                -> hoistEither $ Right ()
        Just (item, container) -> do
            let
                out =
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
    let filtered              = filter (inputMatchesItem target) itemsInOpenContainers
    case headMay filtered of
        Nothing -> do
            let out = dontSeeObject $ target ^. normal
            hoistEither $ Left out
        Just (item, container) -> do
            pickupItemMutation item
            let out = takeItemFromContainer item container
            hoistEither $ Right out

openHere :: UID -> Container -> Bool
openHere loc' cont = (cont ^. loc == loc') && (cont ^. cState == Open)

closedTransHere :: UID -> Container -> Bool
closedTransHere loc' cont =
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
    let
        i = item ^. name
        c = container ^. name
    in "You take the " ++ i ++ " from the " ++ c ++ "."

dontSeeObject :: String -> String
dontSeeObject object =
    "You don't see " ++ indefArt object ++ " " ++ object ++ " here."
