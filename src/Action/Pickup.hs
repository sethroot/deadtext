{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Action.Pickup (pickupAction) where

import Common (indefArt, period)
import Control.Error (headMay, hoistEither, runExceptT)
import Control.Lens ((.=), Each(each), Ixed(ix), (^.), use, filtered)
import Control.Monad.State.Lazy (MonadState)
import Data.List (elemIndex, intersperse)
import qualified Data.Text as T
import Parser (parseItemM, parseRecM, recParseNpc)
import Types
import Util (hoistL, hoistR)

pickupAction :: (MonadState Game m) => [Input] -> m (Either T.Text T.Text)
pickupAction inputs = runExceptT $ do
    npc' <- recParseNpc inputs
    _    <- case npc' of
        Just npc'' -> hoistL $ cantPickupNpc npc''
        Nothing    -> hoistR ()

    let input' = headMay inputs
    target <- case input' of
        Nothing     -> hoistL pickupWhat
        Just target -> hoistR target

    _ <- case target ^. normal of
        "all" -> pickupAll >>= hoistL
        _     -> hoistR ()
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
            hoistL $ item'' ^. takeDesc
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

pickupAll :: MonadState Game m => m T.Text
pickupAll = do
    loc'   <- use loc
    items' <- use items
    _ <- items . each . filtered (\i -> i ^. loc == ItemLoc loc') . loc .= ItemInv
    let here = filter (\i -> i ^. loc == ItemLoc loc') items'
    let texts = (^. takeDesc) <$> here
    let out   = mconcat $ intersperse "\n\n" texts
    pure out

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
inputMatchesItem input' (i, _) = T.toLower (i ^. name) == input' ^. normal

pickupItemMutation :: MonadState Game m => Item -> m ()
pickupItemMutation item' = do
    items' <- use items
    case elemIndex item' items' of
        Nothing -> pure ()
        Just i  -> items . ix i . loc .= ItemInv

takeItemFromContainer :: Item -> Container -> T.Text
takeItemFromContainer item' container =
    let
        i = item' ^. name
        c = container ^. name
    in period . T.unwords $ ["You take the", i, "from the", c]

pickupWhat :: T.Text
pickupWhat = "Pickup what?"

cantPickupNpc :: Npc -> T.Text
cantPickupNpc npc = period . T.unwords $ ["You can't pickup", npc ^. name]

dontSeeObject :: T.Text -> T.Text
dontSeeObject object =
    period . T.unwords $ ["You don't see", indefArt object, object, "here"]

seeClosedCont :: Item -> Container -> T.Text
seeClosedCont item' container =
    period
        . T.unwords
        $ [ "You can see"
          , indefArt i
          , i
          , "in the"
          , c <> ","
          , "but the"
          , c
          , "is closed"
          ]
    where
        i = item' ^. name
        c = container ^. name
