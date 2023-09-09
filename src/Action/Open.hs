{-# LANGUAGE FlexibleContexts #-}

module Action.Open (openAction) where

import Control.Error (headMay, hoistEither, runExceptT)
import Control.Lens ((.=), Ixed(ix), (^.), use)
import Control.Monad.State.Lazy (MonadState)
import Data.List (elemIndex)
import Parser (parseContainerM, parseInvObjM, parseItemObjM)
import Types
import Util (hoistL, hoistR)

openAction :: MonadState Game m => [Input] -> m (Either String String)
openAction inputs = runExceptT $ do
    let input' = headMay inputs
    target <- case input' of
        Nothing     -> hoistL openWhat
        Just target -> hoistR target

    container  <- parseContainerM $ target ^. normal
    container' <- case container of
        Nothing -> do
            -- Not a container, but might be a world Item
            itemObj <- parseItemObjM $ target ^. normal
            _       <- case itemObj of
                Nothing -> hoistR ()
                Just _  -> do
                    let out = cantBeOpened $ target ^. raw
                    hoistL out
            -- Not a container or world Item, but might be an Inv Item
            invObj <- parseInvObjM $ target ^. normal
            _      <- case invObj of
                Nothing -> hoistR ()
                Just _  -> do
                    let out = cantBeOpened $ target ^. raw
                    hoistL out
            -- Not found yet, stop searching
            let out = dontSee $ target ^. normal
            hoistL out
        Just c -> hoistR c

    if (container' ^. cState) == Open
        then do
            let out = alreadyOpen $ container' ^. name
            hoistL out
        else hoistR ()

    containers' <- use containers
    index       <- case elemIndex container' containers' of
        Nothing -> hoistL somethingWrong
        Just i  -> hoistR i

    containers . ix index . cState .= Open
    let out = openContainer $ container' ^. name
    hoistR out

openWhat :: String
openWhat = "Open what?"

cantBeOpened :: String -> String
cantBeOpened target = "The " ++ target ++ " is not able to be openend."

dontSee :: String -> String
dontSee target = "You don't see a " ++ target ++ "."

alreadyOpen :: String -> String
alreadyOpen target = "The " ++ target ++ " is already open."

somethingWrong :: String
somethingWrong = "Something has gone terribly wrong."

openContainer :: String -> String
openContainer target = "You open the " ++ target ++ "."
