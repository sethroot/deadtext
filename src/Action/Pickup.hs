{-# LANGUAGE FlexibleContexts #-}

module Action.Pickup (pickupAction) where

import Common (indefArt, period)
import Control.Error (headMay, hoistEither, runExceptT)
import Control.Lens ((.=), Ixed(ix), (^.), use)
import Control.Monad.State.Lazy (MonadState)
import Data.Char (toLower)
import Data.List (elemIndex)
import Parser (parseItemM, parseRecM, recParseNpc)
import Types
import Util (hoistL, hoistR)

pickupAction :: (MonadState Game m) => [Input] -> m (Either String String)
pickupAction inputs = runExceptT $ do
    npc' <- recParseNpc inputs
    _    <- case npc' of
        Just npc'' -> hoistL $ cantPickupNpc npc''
        Nothing    -> hoistR ()

    let input' = headMay inputs
    target <- case input' of
        Nothing     -> hoistL pickupWhat
        Just target -> hoistR target

    loc'        <- use loc
    containers' <- use containers
    items'      <- use items
    mItem       <- parseRecM parseItemM inputs
    item''      <- case mItem of
        Nothing -> do
            let out = dontSeeObject $ target ^. raw
            hoistL out
        Just item' -> hoistR item'
    if item'' ^. loc == ItemLoc loc'
        then do
            pickupItemMutation item''
            let out = (item'' ^. name) ++ " taken."
            hoistL out
        else hoistR ()

    let closedTransContainersHere = filter (closedTransHere loc') containers'
    let
        itemsInClosedTransContainersHere =
            itemsInContainers items' closedTransContainersHere
    let
        inputInTransContainer =
            filter (inputMatchesItem target) itemsInClosedTransContainersHere
    case headMay inputInTransContainer of
        Nothing                 -> hoistEither $ Right ()
        Just (item', container) -> do
            let out = seeClosedCont item' container
            hoistL out

    let openContainersHere    = filter (openHere loc') containers'
    let itemsInOpenContainers = itemsInContainers items' openContainersHere
    let filtered              = filter (inputMatchesItem target) itemsInOpenContainers
    case headMay filtered of
        Nothing -> do
            let out = dontSeeObject $ target ^. normal
            hoistL out
        Just (item', container) -> do
            pickupItemMutation item'
            let out = takeItemFromContainer item' container
            hoistR out

openHere :: UID -> Container -> Bool
openHere loc' cont = (cont ^. loc == loc') && (cont ^. cState == Open)

closedTransHere :: UID -> Container -> Bool
closedTransHere loc' cont =
    (cont ^. loc == loc')
        && (cont ^. trans == Transparent)
        && (cont ^. cState == Closed)

itemsInContainers :: [Item] -> [Container] -> [(Item, Container)]
itemsInContainers is cs =
    [ (i, c) | i <- is, c <- cs, i ^. loc == ItemContainer (c ^. uid) ]

inputMatchesItem :: Input -> (Item, Container) -> Bool
inputMatchesItem input' (i, _) = fmap toLower (i ^. name) == input' ^. normal

pickupItemMutation :: MonadState Game m => Item -> m ()
pickupItemMutation item' = do
    items' <- use items
    case elemIndex item' items' of
        Nothing -> pure ()
        Just i  -> items . ix i . loc .= ItemInv

takeItemFromContainer :: Item -> Container -> String
takeItemFromContainer item' container =
    let
        i = item' ^. name
        c = container ^. name
    in period . unwords $ ["You take the", i, "from the", c]

pickupWhat :: String
pickupWhat = "Pickup what?"

cantPickupNpc :: Npc -> String
cantPickupNpc npc = period . unwords $ ["You can't pickup", npc ^. name]

dontSeeObject :: String -> String
dontSeeObject object =
    period . unwords $ ["You don't see", indefArt object, object, "here"]

seeClosedCont :: Item -> Container -> String
seeClosedCont item' container =
    period
        . unwords
        $ [ "You can see"
          , indefArt i
          , i
          , "in the"
          , c ++ ","
          , "but the"
          , c
          , "is closed"
          ]
    where
        i = item' ^. name
        c = container ^. name
