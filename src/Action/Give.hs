{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Action.Give (giveAction) where

import Common (inventory, npcIsHere, period)
import Control.Error ((??), hoistEither, runExceptT)
import Control.Lens ((.=), Ixed(ix), (^.), makeFields, use)
import Control.Monad.State.Lazy (MonadState)
import Data.List (elemIndex)
import Parser (parseItemM, parseNpcM, parseRecM)
import Types
import Util (hoistL, hoistR, partitionBy)

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

giveAction :: MonadState Game m => [Input] -> m (Either String String)
giveAction args = runExceptT $ do
    giveArgsInput <- parseGiveArgsRaw args ?? giveWhatToWho
    giveArgs      <- parseGiveArgsInput giveArgsInput
    case giveArgs of
        GiveArgs Nothing _ -> do
            hoistR . doNotHave $ "item"
        GiveArgs _ Nothing -> do
            hoistR . isNotHere $ "npc"
        GiveArgs (Just targetItem) (Just npc) -> do
            out <- attemptExecGive targetItem npc
            hoistEither out

attemptExecGive :: MonadState Game m => Item -> Npc -> m (Either String String)
attemptExecGive targetItem npc = runExceptT $ do
    inv <- inventory
    if targetItem `notElem` inv
        then hoistL . doNotHave $ targetItem ^. name
        else hoistR ()
    npcHere <- npcIsHere npc
    if not npcHere then hoistL . isNotHere $ npc ^. name else hoistR ()
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
parseGiveArgsRaw []       = Nothing
parseGiveArgsRaw rawInput = do
    let separator  = Input "to" "to"
    let isElem     = separator `elem` rawInput
    let normalHead = head rawInput
    if not isElem || normalHead == separator
        then Nothing
        else do
            let partitioned = partitionBy separator rawInput
            let item'       = head partitioned
            let target'     = partitioned !! 1
            Just $ GiveArgsInput item' target'

parseGiveArgsInput :: MonadState Game m => GiveArgsInput -> m GiveArgs
parseGiveArgsInput giveArgsInput = do
    item' <- parseRecM parseItemM $ giveArgsInput ^. item
    npc   <- parseRecM parseNpcM $ giveArgsInput ^. target
    pure $ GiveArgs item' npc

giveWhatToWho :: String
giveWhatToWho = "Give what to who?"

doNotHave :: String -> String
doNotHave targetItem = period . unwords $ ["You do not have a", targetItem]

isNotHere :: String -> String
isNotHere npc = period . unwords $ [npc, "is not here"]

somethingWrong :: String
somethingWrong = "Something has gone terribly wrong."

youGive :: String -> String -> String
youGive targetItem npc =
    period . unwords $ ["You give the", targetItem, "to", npc]
