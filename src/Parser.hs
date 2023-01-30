{-# LANGUAGE FlexibleContexts #-}

module Parser where

import Common (inInventory)
import Control.Error (MaybeT(runMaybeT), hoistMaybe)
import Control.Lens.Getter (Getting, (^.), use)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Lazy (MonadState)
import Data.Char (toLower)
import Data.List (find)
import Data.List.Split (splitOn)
import Types
import Util (lowEq)

parseRawInput :: String -> [String]
parseRawInput = filter (/= "") . splitOn " "

normalizeInput :: [String] -> [String]
normalizeInput = fmap $ fmap toLower

-- Generic

parseTarget :: (HasName a String) => [a] -> String -> Maybe a
parseTarget xs _input = find _pred xs where _pred x = lowEq (x ^. name) _input

parseTargetM :: (MonadState Game m, HasName a String)
             => Getting [a] Game [a]
             -> String
             -> m (Maybe a)
parseTargetM get _input = do
    xs <- use get
    pure $ parseTarget xs _input

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

-- Item

parseItem :: [Item] -> String -> Maybe Item
parseItem = parseTarget

parseItemM :: MonadState Game m => String -> m (Maybe Item)
parseItemM = parseTargetM items

parseItemObj :: MonadState Game m => String -> m (Maybe Obj)
parseItemObj _input = runMaybeT $ do
    items' <- use items
    found  <- hoistMaybe $ find _pred items'
    pure $ ObjItem found
    where _pred _item = not (inInventory _item) && lowEq (_item ^. name) _input

-- Inventory Items 

parseInvItem :: MonadState Game m => String -> m (Maybe Item)
parseInvItem _input = do
    items' <- use items
    pure $ find _pred items'
    where _pred _item = inInventory _item && lowEq (_item ^. name) _input

parseInvObj :: MonadState Game m => String -> m (Maybe Obj)
parseInvObj _input = runMaybeT $ do
    items' <- use items
    found  <- hoistMaybe $ find _pred items'
    pure $ ObjInv found
    where _pred _item = inInventory _item && lowEq (_item ^. name) _input

-- NPC

parseNpc :: [Npc] -> String -> Maybe Npc
parseNpc = parseTarget

parseNpcM :: MonadState Game m => String -> m (Maybe Npc)
parseNpcM = parseTargetM npcs

parseNpcRecM :: (MonadIO m, MonadState Game m) => [Input] -> m (Maybe Npc)
parseNpcRecM []       = pure Nothing
parseNpcRecM (x : xs) = do
    -- liftIO . print $ "x: " ++ show x
    -- liftIO . print $ "xs: " ++ show xs
    -- liftIO . putStrLn $ ""
    npc' <- parseNpcM $ x ^. normal
    case npc' of
        Just n  -> pure . Just $ n
        Nothing -> do
            case compare (length xs) 1 of
                GT ->
                    let
                        next       = head xs
                        rest       = tail xs
                        nextRaw    = x ^. raw ++ " " ++ next ^. raw
                        nextNormal = x ^. normal ++ " " ++ next ^. normal
                        nextInput  = Input nextRaw nextNormal
                        joined     = [nextInput] <> rest
                    in parseNpcRecM joined
                EQ ->
                    let
                        next       = head xs
                        nextRaw    = x ^. raw ++ " " ++ next ^. raw
                        nextNormal = x ^. normal ++ " " ++ next ^. normal
                        nextInput  = Input nextRaw nextNormal
                    in parseNpcRecM [nextInput]
                LT -> parseNpcRecM []

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
