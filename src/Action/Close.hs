{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Action.Close (closeAction) where

import Common (indefArt, period)
import Control.Error ((??), hoistEither, runExceptT)
import Control.Lens ((.=), Ixed(ix), (^.), use)
import Control.Monad.State.Lazy (MonadState)
import Data.List (elemIndex)
import qualified Data.Text as T
import Parser (parseContainerM)
import Safe (headMay)
import Types
import Util (hoistL, hoistR)

closeAction :: MonadState Game m => [Input] -> m (Either T.Text T.Text)
closeAction inputs = runExceptT $ do
    target     <- headMay inputs ?? closeWhat
    container  <- parseContainerM $ target ^. normal
    container' <- container ?? youDontSeeA target

    if (container' ^. cState) == Closed
        then do
            hoistL $ alreadyClosed container'
        else hoistR ()

    containers' <- use containers
    index       <- case elemIndex container' containers' of
        Nothing -> hoistEither $ Left cantCloseThat
        Just i  -> hoistEither $ Right i

    containers . ix index . cState .= Closed
    hoistR $ youCloseThe container'

closeWhat :: T.Text
closeWhat = "Close what?"

youDontSeeA :: Input -> T.Text
youDontSeeA i =
    let
        target    = i ^. normal
        indefArt' = indefArt target
    in period . T.unwords $ ["You don't see", indefArt', i ^. normal]

alreadyClosed :: Container -> T.Text
alreadyClosed c = T.unwords ["The", c ^. name, "is already closed."]

cantCloseThat :: T.Text
cantCloseThat = "Can't close that."

youCloseThe :: Container -> T.Text
youCloseThe c = period . T.unwords $ ["You close the", c ^. name]
