{-# LANGUAGE FlexibleContexts #-}

module Parsing where

import           Control.Error                  ( MaybeT(runMaybeT)
                                                , hoistMaybe
                                                )
import           Control.Lens                   ( (^.)
                                                , use
                                                )
import           Control.Monad.State.Lazy       ( MonadState )
import           Control.Monad.Trans.Maybe      ( MaybeT(runMaybeT) )
import           Data.Char                      ( toLower )
import           Data.List                      ( find )
import           Data.List.Split                ( splitOn )
import           Types

parseRawInput :: String -> [String]
parseRawInput = filter (/= "") . splitOn " "

normalizeInput :: [String] -> [String]
normalizeInput = fmap $ fmap toLower

-- Attempt to parse input to an Item held in Inventory

parseInvObj :: MonadState Game m => String -> m (Maybe Obj)
parseInvObj input = runMaybeT $ do
    items' <- use items
    found  <- hoistMaybe $ find
        (\x ->
            x
                ^. loc
                == ItemInv
                && (toLower <$> (x ^. name))
                == (toLower <$> input)
        )
        items'
    pure $ ObjInv found

-- Attempt to parse input to an Item not held in Inventory

parseItemObj :: MonadState Game m => String -> m (Maybe Obj)
parseItemObj input = runMaybeT $ do
    items' <- use items
    found  <- hoistMaybe $ find
        (\x ->
            x
                ^. loc
                /= ItemInv
                && (toLower <$> (x ^. name))
                == (toLower <$> input)
        )
        items'
    pure $ ObjItem found

parseNpcObj :: MonadState Game m => String -> m (Maybe Obj)
parseNpcObj input = runMaybeT $ do
    npcs' <- use npcs
    found <- hoistMaybe $ find
        (\x -> (toLower <$> (x ^. name)) == (toLower <$> input))
        npcs'
    pure $ ObjNpc found

parseItemM :: MonadState Game m => String -> m (Maybe Item)
parseItemM input = do
    items' <- use items
    pure $ find (\x -> (toLower <$> (x ^. name)) == (toLower <$> input)) items'

parseInvItem :: MonadState Game m => String -> m (Maybe Item)
parseInvItem input = do
    items' <- use items
    pure $ find
        (\x ->
            x
                ^. loc
                == ItemInv
                && (toLower <$> (x ^. name))
                == (toLower <$> input)
        )
        items'

parseNpc :: [Npc] -> String -> Maybe Npc
parseNpc npcs input = do
    find (\x -> (toLower <$> (x ^. name)) == (toLower <$> input)) npcs

parseNpcM :: MonadState Game m => String -> m (Maybe Npc)
parseNpcM input = do
    npcs' <- use npcs
    pure $ parseNpc npcs' input

parseContainer :: MonadState Game m => String -> m (Maybe Container)
parseContainer input = do
    containers' <- use containers
    pure $ find (\x -> (toLower <$> (x ^. name)) == (toLower <$> input))
                containers'

parseDir :: Monad m => String -> m (Maybe Direction)
parseDir = pure . parseDir' . map toLower
  where
    parseDir' "n"     = Just N
    parseDir' "north" = Just N
    parseDir' "s"     = Just S
    parseDir' "south" = Just S
    parseDir' "e"     = Just E
    parseDir' "east"  = Just E
    parseDir' "w"     = Just W
    parseDir' "west"  = Just W
    parseDir' "u"     = Just U
    parseDir' "up"    = Just U
    parseDir' "d"     = Just D
    parseDir' "down"  = Just D
    parseDir' _       = Nothing
