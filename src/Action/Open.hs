{-# LANGUAGE FlexibleContexts #-}

module Action.Open (openAction) where

import Control.Error (headMay, hoistEither, runExceptT)
import Control.Lens ((.=), Ixed(ix), (^.), use)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Lazy (MonadState)
import Data.Aeson (Value(String))
import Data.List (elemIndex)
import Parser (parseContainerM, parseInvObj, parseItemObj)
import Types

openAction :: (MonadState Game m, MonadIO m) => [Input] -> m ()
openAction inputs = do
    out <- open inputs
    either printE printE out
    where printE = liftIO . putStrLn

open :: MonadState Game m => [Input] -> m (Either String String)
open inputs = runExceptT $ do
    let input' = headMay inputs
    target <- case input' of
        Nothing     -> hoistEither . Left $ openWhat 
        Just target -> hoistEither . Right $ target

    container  <- parseContainerM $ target ^. normal
    container' <- case container of
        Nothing -> do
            -- Not a container, but might be a world Item
            itemObj <- parseItemObj $ target ^. normal
            _       <- case itemObj of
                Nothing -> hoistEither . Right $ ()
                Just _  -> do
                    let out = cantBeOpened $ target ^. raw
                    hoistEither . Left $ out
            -- Not a container or world Item, but might be an Inv Item
            invObj <- parseInvObj $ target ^. normal
            _      <- case invObj of
                Nothing -> hoistEither . Right $ ()
                Just _  -> do
                    let out = cantBeOpened $ target ^. raw
                    hoistEither . Left $ out
            -- Not found yet, stop searching
            let out = dontSee $ target ^. normal
            hoistEither . Left $ out
        Just c -> hoistEither . Right $ c

    if (container' ^. cState) == Open
        then do
            let out = alreadyOpen $ container' ^. name
            hoistEither . Left $ out
        else hoistEither . Right $ ()

    containers' <- use containers
    index       <- case elemIndex container' containers' of
        Nothing -> hoistEither . Left $ somethingWrong
        Just i  -> hoistEither . Right $ i

    containers . ix index . cState .= Open
    let out = openContainer $ container' ^. name
    hoistEither . Right $ out

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
