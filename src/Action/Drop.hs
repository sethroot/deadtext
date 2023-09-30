{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Action.Drop (dropAction) where

import Common (indefArt, inventory, period)
import Control.Error
    ( (??)
    , ExceptT
    , MaybeT(runMaybeT)
    , headMay
    , hoistMaybe
    , runExceptT
    )
import Control.Lens ((%=), (&), (.=), (.~), Ixed(ix), (^.), use)
import Control.Lens.Each (each)

import Control.Monad.State.Lazy (MonadState)
import Data.List (elemIndex)
import qualified Data.Text as T
import Parser (recParseInvItem)
import Types
import Util (hoistL, hoistR)

dropAction :: MonadState Game m => [Input] -> m (Either T.Text T.Text)
dropAction inputs = runExceptT $ do
    target <- headMay inputs ?? dropWhat
    _      <- case target ^. normal of
        "all"        -> dropAll
        "everything" -> dropAll
        _            -> hoistR ()
    itemM      <- recParseInvItem inputs
    targetItem <- itemM ?? dontHaveObject (target ^. normal)
    let itemName = targetItem ^. name
    result <- dropMutation targetItem
    _      <- result ?? cantDrop itemName
    pure . dropObject $ itemName

dropAll :: MonadState Game m => ExceptT T.Text m a
dropAll = do
    inv <- inventory
    _   <- if null inv
        then hoistL nothingToDrop
        else hoistR ()
    loc' <- use loc
    items . each %= moveFromInventoryToLoc loc'
    hoistL youDropAllBelongings

moveFromInventoryToLoc :: UID -> Item -> Item
moveFromInventoryToLoc loc' i =
    if i ^. loc == ItemInv then i & loc .~ ItemLoc loc' else i

dropMutation :: MonadState Game m => Item -> m (Maybe ())
dropMutation targetItem = runMaybeT $ do
    items' <- use items
    index  <- case elemIndex targetItem items' of
        Nothing -> hoistMaybe Nothing
        Just i  -> pure i
    loc' <- use loc
    items . ix index . loc .= ItemLoc loc'

dropWhat :: T.Text
dropWhat = "Drop what?"

nothingToDrop :: T.Text
nothingToDrop = "You are not carrying anything."

youDropAllBelongings :: T.Text
youDropAllBelongings = "You drop all your belongings."

cantDrop :: T.Text -> T.Text
cantDrop object =
    period . T.unwords $ ["Inexplicably, you are unable to drop your", object]

dontHaveObject :: T.Text -> T.Text
dontHaveObject object =
    period . T.unwords $ ["You do not have", indefArt object, object]

dropObject :: T.Text -> T.Text
dropObject object = period . T.unwords $ ["You drop the", object]
