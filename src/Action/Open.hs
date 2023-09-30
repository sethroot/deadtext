{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Action.Open (openAction) where

import Common (period)
import Control.Error (headMay, runExceptT)
import Control.Lens ((.=), Ixed(ix), (^.), use)
import Control.Monad.State.Lazy (MonadState)
import Data.List (elemIndex)
import qualified Data.Text as T
import Parser (parseContainerM, parseInvObjM, parseItemObjM)
import Types
import Util (hoistL, hoistR)

openAction :: MonadState Game m => [Input] -> m (Either T.Text T.Text)
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

openWhat :: T.Text
openWhat = "Open what?"

cantBeOpened :: T.Text -> T.Text
cantBeOpened target =
    period . T.unwords $ ["The", target, "is not able to be openend"]

dontSee :: T.Text -> T.Text
dontSee target = period . T.unwords $ ["You don't see a", target]

alreadyOpen :: T.Text -> T.Text
alreadyOpen target = period . T.unwords $ ["The", target, "is already open"]

somethingWrong :: T.Text
somethingWrong = "Something has gone terribly wrong."

openContainer :: T.Text -> T.Text
openContainer target = period . T.unwords $ ["You open the", target]
