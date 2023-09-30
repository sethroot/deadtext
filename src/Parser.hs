{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Common (inInventory, notHolding)
import Control.Error (MaybeT(runMaybeT), hoistMaybe)
import Control.Lens.Getter (Getting, (^.), use)
import Control.Lens.Iso (iso, under)
import Control.Monad.State.Lazy (MonadState)
import Data.List (find)
import Data.Monoid (All(All, getAll), Any(Any, getAny))
import qualified Data.Text as T
import Types
import Util (lowEq)

-- [Verb] [op: the] [Target] [op: to, with] [Target]
-- Unlock door with key
-- Use key on door
-- Use key on north door
-- Use key on door to north
-- Give torch to dwarf
-- Attack pyramid head with knife
-- Shoot gun at nurse
-- Look in box
-- Look at car
-- Examine book

parseRawInput :: T.Text -> [T.Text]
parseRawInput = T.words

-- Test if Inputs match String pattern
matchesText :: [Input] -> T.Text -> Bool
matchesText inputs txt =
    let
        words'           = T.words txt
        sameLength       = length words' == length inputs
        alls             = zipWith (\t i -> All $ i ^. normal == t) words' inputs
        inputMatchesText = getAll . mconcat $ alls
    in sameLength && inputMatchesText

-- TODO: fixme
-- removePunctuation :: String -> String
-- removePunctuation = filter (notElem ",.<>/?;:\"\'!@#$%^&*()-_=+")

normalizeInput :: [T.Text] -> [T.Text]
normalizeInput = fmap T.toLower

-- Generic

parseTarget :: (HasName a T.Text) => [a] -> T.Text -> Maybe a
parseTarget xs _input = find _pred xs where _pred x = lowEq (x ^. name) _input

parseTargetWithSyn :: (HasName a T.Text, HasSyn a [T.Text])
                   => [a]
                   -> T.Text
                   -> Maybe a
parseTargetWithSyn xs _input = find (nameOrSynMatchesInput _input) xs

parseTargetM :: (MonadState Game m, HasName a T.Text)
             => Getting [a] Game [a]
             -> T.Text
             -> m (Maybe a)
parseTargetM get _input = do
    xs <- use get
    pure $ parseTarget xs _input

parseTargetWithSynM :: (MonadState Game m, HasName a T.Text, HasSyn a [T.Text])
                    => Getting [a] Game [a]
                    -> T.Text
                    -> m (Maybe a)
parseTargetWithSynM get _input = do
    xs <- use get
    pure $ parseTargetWithSyn xs _input

parseTargetObj :: (MonadState Game m, HasName a T.Text)
               => Getting [a] Game [a]
               -> (a -> Obj)
               -> T.Text
               -> m (Maybe Obj)
parseTargetObj get f _input = runMaybeT $ do
    xs    <- use get
    found <- hoistMaybe $ find _pred xs
    pure $ f found
    where _pred x = lowEq (x ^. name) _input

parseRecM :: MonadState Game m
          => (T.Text -> m (Maybe a))
          -> [Input]
          -> m (Maybe a)
parseRecM _ []             = pure Nothing
parseRecM f inputs@(x : _) = do
    result <- f $ x ^. normal
    case result of
        Just found -> pure . Just $ found
        Nothing    -> parseRecM f $ consumeNext inputs

consumeNext :: [Input] -> [Input]
consumeNext [] = []
consumeNext (x : xs) =
    let
        next       = head xs
        rest       = tail xs
        nextRaw    = T.unwords [x ^. raw, next ^. raw]
        nextNormal = T.unwords [x ^. normal, next ^. normal]
        nextInput  = Input nextRaw nextNormal
    in case compare (length xs) 1 of
        GT -> [nextInput] <> rest
        EQ -> [nextInput]
        LT -> []

nameOrSynMatchesInput :: (HasName a T.Text, HasSyn a [T.Text])
                      => T.Text
                      -> a
                      -> Bool
nameOrSynMatchesInput _input k =
    let
        name'       = k ^. name
        syns        = k ^. syn
        test        = lowEq _input
        matchName   = test name'
        synsResult  = fmap test syns
        toAny       = iso getAny (fmap Any)
        matchAnySyn = under toAny mconcat synsResult
    in matchName || matchAnySyn

-- Item

parseItem :: [Item] -> T.Text -> Maybe Item
parseItem = parseTargetWithSyn

parseItemM :: MonadState Game m => T.Text -> m (Maybe Item)
parseItemM = parseTargetWithSynM items

parseItemObjM :: MonadState Game m => T.Text -> m (Maybe Obj)
parseItemObjM _input = runMaybeT $ do
    items' <- use items
    found  <- hoistMaybe $ find pred' items'
    pure $ ObjItem found
    where pred' i = notHolding i && nameOrSynMatchesInput _input i

-- Inventory Items

parseInvItemM :: MonadState Game m => T.Text -> m (Maybe Item)
parseInvItemM _input = runMaybeT $ do
    items' <- use items
    hoistMaybe $ find pred' items'
    where pred' i = inInventory i && nameOrSynMatchesInput _input i

recParseInvItem :: MonadState Game m => [Input] -> m (Maybe Item)
recParseInvItem = parseRecM parseInvItemM

parseInvObjM :: MonadState Game m => T.Text -> m (Maybe Obj)
parseInvObjM _input = runMaybeT $ do
    items' <- use items
    hoistMaybe . fmap ObjInv $ find pred' items'
    where pred' i = inInventory i && nameOrSynMatchesInput _input i

recParseInvObj :: MonadState Game m => [Input] -> m (Maybe Obj)
recParseInvObj = parseRecM parseInvObjM

-- NPC

parseNpc :: [Npc] -> T.Text -> Maybe Npc
parseNpc = parseTarget

parseNpcM :: MonadState Game m => T.Text -> m (Maybe Npc)
parseNpcM = parseTargetM npcs

recParseNpc :: MonadState Game m => [Input] -> m (Maybe Npc)
recParseNpc = parseRecM parseNpcM

parseNpcObjM :: MonadState Game m => T.Text -> m (Maybe Obj)
parseNpcObjM = parseTargetObj npcs ObjNpc

-- Container

parseContainer :: [Container] -> T.Text -> Maybe Container
parseContainer = parseTarget

parseContainerM :: MonadState Game m => T.Text -> m (Maybe Container)
parseContainerM = parseTargetM containers

parseContObjM :: MonadState Game m => T.Text -> m (Maybe Obj)
parseContObjM = parseTargetObj containers ObjCont

-- Direction

parseDirM :: Monad m => T.Text -> m (Maybe Direction)
parseDirM = pure . go . T.toLower
    where
        go "n"         = Just N
        go "north"     = Just N
        go "s"         = Just S
        go "south"     = Just S
        go "e"         = Just E
        go "east"      = Just E
        go "w"         = Just W
        go "west"      = Just W
        go "nw"        = Just NW
        go "northwest" = Just NW
        go "ne"        = Just NE
        go "northeast" = Just NE
        go "sw"        = Just SW
        go "southwest" = Just SW
        go "se"        = Just SE
        go "southeast" = Just SE
        go "u"         = Just U
        go "up"        = Just U
        go "d"         = Just D
        go "down"      = Just D
        go _           = Nothing
