{-# LANGUAGE FlexibleContexts #-}

module Action.Close (closeAction) where

import Common (indefArt, period)
import Control.Error ((??), hoistEither, runExceptT)
import Control.Lens ((.=), Ixed(ix), (^.), use)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Lazy (MonadState)
import Data.List (elemIndex)
import Data.String (String)
import Parser (parseContainerM)
import Safe (headMay)
import Types

closeAction :: (MonadState Game m, MonadIO m) => [Input] -> m ()
closeAction inputs = do
    out <- close inputs
    either printE printE out
    where printE = liftIO . putStrLn

close :: MonadState Game m => [Input] -> m (Either String String)
close inputs = runExceptT $ do
    target     <- headMay inputs ?? closeWhat
    container  <- parseContainerM $ target ^. normal
    container' <- container ?? youDontSeeA target

    if (container' ^. cState) == Closed
        then do
            hoistEither . Left $ alreadyClosed container'
        else hoistEither . Right $ ()

    containers' <- use containers
    index       <- case elemIndex container' containers' of
        Nothing -> hoistEither $ Left cantCloseThat
        Just i -> hoistEither $ Right i

    containers . ix index . cState .= Closed
    hoistEither . Right $ youCloseThe container'

closeWhat :: String
closeWhat = "Close what?"

youDontSeeA :: Input -> String
youDontSeeA i =
    let
        target    = i ^. normal
        indefArt' = indefArt target
    in period . unwords $ ["You don't see", indefArt', i ^. normal]

alreadyClosed :: Container -> String
alreadyClosed c = unwords ["The", c ^. name, "is already closed."]

cantCloseThat :: String
cantCloseThat = "Can't close that."

youCloseThe :: Container -> String
youCloseThe c = period . unwords $ ["You close the", c ^. name]
