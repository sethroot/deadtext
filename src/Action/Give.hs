{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Action.Give where

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
    input  <- case parseGiveArgsRaw args of
        Nothing    -> hoistEither . Left $ "Give what to who now?"
        Just input -> hoistEither $ Right input
    parsed <- parseGiveArgsInput input
    case parsed of
        GiveArgs Nothing     _          -> hoistEither . Right $ dontHaveA input
        GiveArgs _           Nothing    -> hoistEither . Right $ isNotHere input
        GiveArgs (Just item) (Just npc) -> do
            out <- attemptExecGive input item npc
            hoistEither out

dontHaveA :: GiveArgsInput -> String
dontHaveA input = "You don't have a " ++ item' ++ "."
    where item' = input ^. item . raw

isNotHere :: GiveArgsInput -> String
isNotHere input = npc' ++ " is not here." where npc' = input ^. target . raw

attemptExecGive :: MonadState Game m
                => GiveArgsInput
                -> Item
                -> Npc
                -> m (Either String String)
attemptExecGive input item npc = runExceptT $ do
    inv <- inventory
    if item `notElem` inv
        then hoistEither . Left $ "You do not have a " ++ item ^. name ++ "."
        else hoistEither . Right $ ()
    npcHere <- npcIsHere npc
    if not npcHere
        then hoistEither . Left $ input ^. target . raw ++ " is not here."
        else hoistEither . Right $ ()
    items' <- use items
    case elemIndex item items' of
        Nothing    -> hoistEither . Left $ "Something has gone terribly wrong."
        Just index -> do
            items . ix index . loc .= ItemNpc (npc ^. uid)
            hoistEither . Right $ giveTo item npc

giveTo :: Item -> Npc -> String
giveTo item npc = "You give the " ++ item' ++ "to" ++ npc' ++ "."
    where
        item' = item ^. name
        npc'  = npc ^. name

parseGiveArgsRaw :: [Input] -> Maybe GiveArgsInput
parseGiveArgsRaw raw = do
    let sanitized = filter (\x -> x ^. normal /= "to") raw
    itemInput   <- headMay sanitized
    targetInput <- tailMay sanitized >>= headMay
    Just $ GiveArgsInput itemInput targetInput

parseGiveArgsInput :: MonadState Game m => GiveArgsInput -> m GiveArgs
parseGiveArgsInput input = do
    item' <- parseItemM $ input ^. item . normal
    npc   <- parseNpcM $ input ^. target . normal
    pure $ GiveArgs item' npc
