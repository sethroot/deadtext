{-# LANGUAGE FlexibleContexts #-}

module Parser where

import           Common                         ( inInventory )
import           Control.Error                  ( MaybeT(runMaybeT)
                                                , hoistMaybe
                                                )
import           Control.Lens                   ( (^.)
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

-- Attempt to parse input to an Item held in Inventory

parseInvObj :: MonadState Game m => String -> m (Maybe Obj)
parseInvObj input = runMaybeT $ do
    items' <- use items
    found  <- hoistMaybe $ find go items'
    pure $ ObjInv found
    where go item = inInventory item && lowEq (item ^. name) input

-- Attempt to parse input to an Item not held in Inventory

parseItemObj :: MonadState Game m => String -> m (Maybe Obj)
parseItemObj input = runMaybeT $ do
    items' <- use items
    found  <- hoistMaybe $ find go items'
    pure $ ObjItem found
    where go item = not (inInventory item) && lowEq (item ^. name) input

parseNpcObj :: MonadState Game m => String -> m (Maybe Obj)
parseNpcObj input = runMaybeT $ do
    npcs' <- use npcs
    found <- hoistMaybe $ find go npcs'
    pure $ ObjNpc found
    where go npc = lowEq (npc ^. name) input

parseItemM :: MonadState Game m => String -> m (Maybe Item)
parseItemM input = do
    items' <- use items
    pure $ find go items'
    where go item = lowEq (item ^. name) input

parseInvItem :: MonadState Game m => String -> m (Maybe Item)
parseInvItem input = do
    items' <- use items
    pure $ find go items'
    where go item = inInventory item && lowEq (item ^. name) input

parseNpc :: [Npc] -> String -> Maybe Npc
parseNpc npcs input = do
    find go npcs
    where go npc = lowEq (npc ^. name) input

parseNpcM :: MonadState Game m => String -> m (Maybe Npc)
parseNpcM input = do
    npcs' <- use npcs
    pure $ parseNpc npcs' input

parseContainer :: MonadState Game m => String -> m (Maybe Container)
parseContainer input = do
    containers' <- use containers
    pure $ find go containers'
    where go cont = lowEq (cont ^. name) input

parseContObj :: MonadState Game m => String -> m (Maybe Obj)
parseContObj input = runMaybeT $ do
    containers' <- use containers
    found       <- hoistMaybe $ find
        (\x -> (toLower <$> (x ^. name)) == (toLower <$> input))
        containers'
    pure $ ObjCont found

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
