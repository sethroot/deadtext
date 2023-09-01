{-# LANGUAGE FlexibleContexts #-}

module Parser where

import Common (inInventory)
import Control.Error (MaybeT(runMaybeT), hoistMaybe)
import Control.Lens.Getter (Getting, (^.), use)
import Control.Lens.Iso (iso, under)
import Control.Monad.State.Lazy (MonadState)
import Data.Char (toLower)
import Data.List (find)
import Data.List.Split (splitOn)
import Data.Monoid (Any(Any, getAny))
import Types
import Util (lowEq)

parseRawInput :: String -> [String]
parseRawInput = filter (/= "") . splitOn " "

normalizeInput :: [String] -> [String]
normalizeInput = fmap $ fmap toLower

-- Generic

parseTarget :: (HasName a String) => [a] -> String -> Maybe a
parseTarget xs _input = find _pred xs where _pred x = lowEq (x ^. name) _input

parseTargetWithSyn :: (HasName a String, HasSyn a [String])
                   => [a]
                   -> String
                   -> Maybe a
parseTargetWithSyn xs _input = find (nameOrSynMatchesInput _input) xs

parseTargetM :: (MonadState Game m, HasName a String)
             => Getting [a] Game [a]
             -> String
             -> m (Maybe a)
parseTargetM get _input = do
    xs <- use get
    pure $ parseTarget xs _input

parseTargetWithSynM :: (MonadState Game m, HasName a String, HasSyn a [String])
                    => Getting [a] Game [a]
                    -> String
                    -> m (Maybe a)
parseTargetWithSynM get _input = do
    xs <- use get
    pure $ parseTargetWithSyn xs _input

parseTargetObj :: (MonadState Game m, HasName a String)
               => Getting [a] Game [a]
               -> (a -> Obj)
               -> String
               -> m (Maybe Obj)
parseTargetObj get f _input = runMaybeT $ do
    xs    <- use get
    found <- hoistMaybe $ find _pred xs
    pure $ f found
    where _pred x = lowEq (x ^. name) _input

parseRecM :: MonadState Game m
          => (String -> m (Maybe a))
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
        nextRaw    = unwords [x ^. raw, next ^. raw]
        nextNormal = unwords [x ^. normal, next ^. normal]
        nextInput  = Input nextRaw nextNormal
    in case compare (length xs) 1 of
        GT -> [nextInput] <> rest
        EQ -> [nextInput]
        LT -> []

nameOrSynMatchesInput :: (HasName a String, HasSyn a [String])
                      => String
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

parseItem :: [Item] -> String -> Maybe Item
parseItem = parseTargetWithSyn

parseItemM :: MonadState Game m => String -> m (Maybe Item)
parseItemM = parseTargetWithSynM items

parseItemObjM :: MonadState Game m => String -> m (Maybe Obj)
parseItemObjM _input = runMaybeT $ do
    items' <- use items
    found  <- hoistMaybe $ find pred' items'
    pure $ ObjItem found
    where pred' i = notHolding i && nameOrSynMatchesInput _input i

notHolding :: Item -> Bool
notHolding = not . inInventory

-- Inventory Items 

parseInvItemM :: MonadState Game m => String -> m (Maybe Item)
parseInvItemM _input = runMaybeT $ do
    items' <- use items
    hoistMaybe $ find pred' items'
    where pred' i = inInventory i && nameOrSynMatchesInput _input i

parseInvObjM :: MonadState Game m => String -> m (Maybe Obj)
parseInvObjM _input = runMaybeT $ do
    items' <- use items
    hoistMaybe . fmap ObjInv $ find pred' items'
    where pred' i = inInventory i && nameOrSynMatchesInput _input i

-- NPC

parseNpc :: [Npc] -> String -> Maybe Npc
parseNpc = parseTarget

parseNpcM :: MonadState Game m => String -> m (Maybe Npc)
parseNpcM = parseTargetM npcs

parseNpcObjM :: MonadState Game m => String -> m (Maybe Obj)
parseNpcObjM = parseTargetObj npcs ObjNpc

-- Container

parseContainer :: [Container] -> String -> Maybe Container
parseContainer = parseTarget

parseContainerM :: MonadState Game m => String -> m (Maybe Container)
parseContainerM = parseTargetM containers

parseContObjM :: MonadState Game m => String -> m (Maybe Obj)
parseContObjM = parseTargetObj containers ObjCont

-- Direction

parseDirM :: Monad m => String -> m (Maybe Direction)
parseDirM = pure . go . map toLower
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
