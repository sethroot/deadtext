{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Action.Give (giveAction) where

import Common (inventory, npcIsHere, period)
import Control.Error ((??), hoistEither, runExceptT)
import Control.Lens ((.=), Ixed(ix), (^.), makeFields, use)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Lazy (MonadState)
import Data.List (elemIndex)
import Parser (parseItemM, parseNpcM, parseRecM)
import Types
import Util (partitionBy)

data GiveArgsInput = GiveArgsInput
    { _giveArgsInputItem   :: [Input]
    , _giveArgsInputTarget :: [Input]
    }
    deriving Eq

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
    giveArgsInput <- parseGiveArgsRaw args ?? giveWhatToWho
    giveArgs      <- parseGiveArgsInput giveArgsInput
    case giveArgs of
        GiveArgs Nothing _ -> do
            -- let
            --     targetItem =
            --         intersperse " " . fmap (^. raw) $ giveArgsInput ^. item
            hoistEither . Right . doNotHave $ "item"
        GiveArgs _ Nothing -> do

            -- let
            --     targetNpc =
            --         intersperse " " . fmap (^. raw) $ giveArgsInput ^. target
            hoistEither . Right . isNotHere $ "npc"
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
    index  <- elemIndex targetItem items' ?? somethingWrong
    items . ix index . loc .= ItemNpc (npc ^. uid)
    pure $ giveTo targetItem npc

giveTo :: Item -> Npc -> String
giveTo targetItem npc = youGive item' npc'
    where
        item' = targetItem ^. name
        npc'  = npc ^. name

parseGiveArgsRaw :: [Input] -> Maybe GiveArgsInput
parseGiveArgsRaw [] = Nothing
parseGiveArgsRaw rawInput = do
    -- FIX: Crashes if args do not follow this pattern
    let partitioned = partitionBy (Input "to" "to") rawInput
    let a           = head partitioned
    let b           = partitioned !! 1
    Just $ GiveArgsInput a b

parseGiveArgsInput :: MonadState Game m => GiveArgsInput -> m GiveArgs
parseGiveArgsInput giveArgsInput = do
    item' <- parseRecM parseItemM $ giveArgsInput ^. item
    npc   <- parseRecM parseNpcM $ giveArgsInput ^. target
    pure $ GiveArgs item' npc

giveWhatToWho :: String
giveWhatToWho = "Give what to who now?"

doNotHave :: String -> String
doNotHave targetItem = period . unwords $ ["You do not have a", targetItem]

isNotHere :: String -> String
isNotHere npc = period . unwords $ [npc, "is not here"]

somethingWrong :: String
somethingWrong = "Something has gone terribly wrong."

youGive :: String -> String -> String
youGive targetItem npc =
    period . unwords $ ["You give the", targetItem, "to", npc]
