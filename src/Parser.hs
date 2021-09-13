{-# LANGUAGE FlexibleContexts #-}

module Parser where

import           Common                         ( inInventory )
import           Control.Error                  ( MaybeT(runMaybeT)
                                                , hoistMaybe
                                                )
import           Control.Lens.Getter            ( Getting
                                                , (^.)
                                                , use
                                                )
import           Control.Monad.State.Lazy       ( MonadState )
import           Data.Char                      ( toLower )
import           Data.List                      ( find )
import           Data.List.Split                ( splitOn )
import           Types
import           Util                           ( lowEq )

parseRawInput :: String -> [String]
parseRawInput = filter (/= "") . splitOn " "

normalizeInput :: [String] -> [String]
normalizeInput = fmap $ fmap toLower

-- Generic

parseTarget :: (HasName a String) => [a] -> String -> Maybe a
parseTarget xs input = find pred xs where pred x = lowEq (x ^. name) input

parseTargetM :: (MonadState Game m, HasName a String)
             => Getting [a] Game [a]
             -> String
             -> m (Maybe a)
parseTargetM get input = do
    xs <- use get
    pure $ parseTarget xs input

parseTargetObj :: (MonadState Game m, HasName a String)
               => Getting [a] Game [a]
               -> (a -> Obj)
               -> String
               -> m (Maybe Obj)
parseTargetObj get f input = runMaybeT $ do
    xs    <- use get
    found <- hoistMaybe $ find pred xs
    pure $ f found
    where pred x = lowEq (x ^. name) input

-- Item

parseItem :: [Item] -> String -> Maybe Item
parseItem = parseTarget

parseItemM :: MonadState Game m => String -> m (Maybe Item)
parseItemM = parseTargetM items

parseItemObj :: MonadState Game m => String -> m (Maybe Obj)
parseItemObj input = runMaybeT $ do
    items' <- use items
    found  <- hoistMaybe $ find pred items'
    pure $ ObjItem found
    where pred item = not (inInventory item) && lowEq (item ^. name) input

-- Inventory Items 

parseInvItem :: MonadState Game m => String -> m (Maybe Item)
parseInvItem input = do
    items' <- use items
    pure $ find pred items'
    where pred item = inInventory item && lowEq (item ^. name) input

parseInvObj :: MonadState Game m => String -> m (Maybe Obj)
parseInvObj input = runMaybeT $ do
    items' <- use items
    found  <- hoistMaybe $ find pred items'
    pure $ ObjInv found
    where pred item = inInventory item && lowEq (item ^. name) input

-- NPC

parseNpc :: [Npc] -> String -> Maybe Npc
parseNpc = parseTarget

parseNpcM :: MonadState Game m => String -> m (Maybe Npc)
parseNpcM = parseTargetM npcs

parseNpcObj :: MonadState Game m => String -> m (Maybe Obj)
parseNpcObj = parseTargetObj npcs ObjNpc

-- Container

parseContainer :: [Container] -> String -> Maybe Container
parseContainer = parseTarget

parseContainerM :: MonadState Game m => String -> m (Maybe Container)
parseContainerM = parseTargetM containers

parseContObj :: MonadState Game m => String -> m (Maybe Obj)
parseContObj = parseTargetObj containers ObjCont

-- Direction

parseDir :: Monad m => String -> m (Maybe Direction)
parseDir = pure . go . map toLower
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
