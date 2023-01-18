{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Action.Give (giveAction) where

import Common (inventory, npcIsHere)
import Control.Error (headMay, hoistEither, runExceptT, tailMay)
import Control.Lens ((.=), Ixed(ix), (^.), makeFields, use)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Lazy (MonadState)
import Data.List (elemIndex)
import Parser (parseItemM, parseNpcM)
import Types

data GiveArgsInput = GiveArgsInput
    { _giveArgsInputItem   :: Input
    , _giveArgsInputTarget :: Input
    }

data GiveArgs = GiveArgs
    { _giveArgsItem   :: Maybe Item
    , _giveArgsTarget :: Maybe Npc
    }

makeFields ''GiveArgsInput
makeFields ''GiveArgs

giveAction :: (MonadState Game m, MonadIO m) => [Input] -> m ()
giveAction args = do
    out <- doGive args
    either printE printE out
    where printE = liftIO . putStrLn

doGive :: MonadState Game m => [Input] -> m (Either String String)
doGive args = runExceptT $ do
    giveArgsInput <- case parseGiveArgsRaw args of
        Nothing -> hoistEither . Left $ giveWhatToWho
        Just i  -> hoistEither $ Right i
    giveArgs <- parseGiveArgsInput giveArgsInput
    case giveArgs of
        GiveArgs Nothing _ ->
            hoistEither . Right . doNotHave $ giveArgsInput ^. item . raw
        GiveArgs _ Nothing ->
            hoistEither . Right . isNotHere $ giveArgsInput ^. target . raw
        GiveArgs (Just targetItem) (Just npc) -> do
            out <- attemptExecGive targetItem npc
            hoistEither out

attemptExecGive :: MonadState Game m => Item -> Npc -> m (Either String String)
attemptExecGive targetItem npc = runExceptT $ do
    inv <- inventory
    if targetItem `notElem` inv
        then hoistEither . Left . doNotHave $ targetItem ^. name
        else hoistEither . Right $ ()
    npcHere <- npcIsHere npc
    if not npcHere
        then hoistEither . Left . isNotHere $ npc ^. name
        else hoistEither . Right $ ()
    items' <- use items
    case elemIndex targetItem items' of
        Nothing    -> hoistEither . Left $ somethingWrong
        Just index -> do
            items . ix index . loc .= ItemNpc (npc ^. uid)
            hoistEither . Right $ giveTo targetItem npc

giveTo :: Item -> Npc -> String
giveTo targetItem npc = youGive item' npc'
    where
        item' = targetItem ^. name
        npc'  = npc ^. name

parseGiveArgsRaw :: [Input] -> Maybe GiveArgsInput
parseGiveArgsRaw rawInput = do
    let sanitized = filter (\x -> x ^. normal /= "to") rawInput
    itemInput   <- headMay sanitized
    targetInput <- tailMay sanitized >>= headMay
    Just $ GiveArgsInput itemInput targetInput

parseGiveArgsInput :: MonadState Game m => GiveArgsInput -> m GiveArgs
parseGiveArgsInput giveArgsInput = do
    item' <- parseItemM $ giveArgsInput ^. item . normal
    npc   <- parseNpcM $ giveArgsInput ^. target . normal
    pure $ GiveArgs item' npc

giveWhatToWho :: String
giveWhatToWho = "Give what to who now?"

doNotHave :: String -> String
doNotHave targetItem = "You do not have a " ++ targetItem ++ "."

isNotHere :: String -> String
isNotHere npc = npc ++ " is not here."

somethingWrong :: String
somethingWrong = "Something has gone terribly wrong."

youGive :: String -> String -> String
youGive targetItem npc = "You give the " ++ targetItem ++ " to " ++ npc ++ "."
