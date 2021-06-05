{-# LANGUAGE FlexibleContexts #-}

module Action.Pickup where

import           Control.Error                  ( MaybeT(runMaybeT)
                                                , fromMaybe
                                                , headMay
                                                , hoistMaybe
                                                )
import           Control.Lens                   ( (.=)
                                                , Ixed(ix)
                                                , (^.)
                                                , use
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.State.Lazy       ( MonadIO(..)
                                                , MonadState
                                                )
import           Control.Monad.Trans.Maybe      ( MaybeT(runMaybeT) )
import           Data.Char                      ( toLower )
import           Data.List                      ( elemIndex )
import           Data.Maybe                     ( fromMaybe )
import           Parsing                        ( parseItemM )
import           Types

pickupAction :: (MonadState Game m, MonadIO m) => Maybe Input -> m ()
pickupAction Nothing      = liftIO . putStrLn $ "What do you want to pickup?"
pickupAction (Just input) = do
    pickupResult <- pickup input
    let msg = fromMaybe (dontSeeObject (input ^. raw)) pickupResult
    liftIO . putStrLn $ msg

dontSeeObject :: String -> String
dontSeeObject object = "You don't see a " ++ object ++ " here."

pickup :: (MonadState Game m) => Input -> m (Maybe String)
pickup target = runMaybeT $ do
    mItem <- parseItemM $ target ^. normal
    item  <- hoistMaybe mItem
    loc'  <- use loc
    if item ^. loc == ItemLoc loc'
        then do
            pickupItemMutation item
            pure $ (item ^. name) ++ " taken."
        else do
            containers' <- use containers
            let openContainersHere = filter
                    (\c -> c ^. loc == Lobby && c ^. cState == Open)
                    containers'
            items' <- use items
            let itemsInOpenContainers =
                    [ (i, c)
                    | i <- items'
                    , c <- openContainersHere
                    , i ^. loc == ItemContainer (c ^. uid)
                    ]
            let
                found =
                    headMay
                        . filter
                              (\(i, _) ->
                                  fmap toLower (i ^. name) == target ^. normal
                              )
                        $ itemsInOpenContainers
            case found of
                Nothing                -> hoistMaybe Nothing
                Just (item, container) -> do
                    pickupItemMutation item
                    pure
                        $  "You take the "
                        ++ (item ^. name)
                        ++ " from the "
                        ++ container
                        ^. name
                        ++ "."

pickupItemMutation :: MonadState Game m => Item -> m ()
pickupItemMutation item = do
    items' <- use items
    case elemIndex item items' of
        Nothing -> pure ()
        Just i  -> items . ix i . loc .= ItemInv
