{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Ext where

import qualified Control.Lens                  as L
import           Control.Monad
import           Control.Monad.State.Lazy
import           Data.Aeson
import           Data.Aeson.Types               ( Parser )
import qualified Data.ByteString.Lazy          as LB
import           Data.Map.Strict               as M
import           Data.Maybe
import           Types
import           UID

data LocExt = LocExt
    { _locExtId       :: String
    , _locExtName     :: String
    , _locExtWalkDesc :: String
    , _locExtLookDesc :: String
    }
    deriving Show

L.makeFields ''LocExt

instance FromJSON LocExt where
    parseJSON = withObject "LocExt" $ \obj -> do
        id       <- obj .: "id"
        name     <- obj .: "name"
        walkDesc <- obj .: "walkDesc"
        lookDesc <- obj .: "lookDesc"
        pure $ LocExt id name walkDesc lookDesc

data ItemLocExt =
  ItemInvExt
  | ItemLocExt String
  | ItemNpcExt String
  | ItemContainerExt String
  deriving Show

instance FromJSON ItemLocExt where
    parseJSON = withObject "ItemLocExt" $ \obj -> do
        locType <- (obj .: "type") :: Parser String
        locData <- obj .:? "data" :: Parser (Maybe String)
        case locType of
            "location" -> pure $ ItemLocExt $ fromJust locData
            _          -> pure ItemInvExt

data ItemExt = ItemExt
    { _itemExtId   :: String
    , _itemExtName :: String
    , _itemExtDesc :: String
    , _itemExtLoc  :: ItemLocExt
    }
    deriving Show

L.makeFields ''ItemExt

instance FromJSON ItemExt where
    parseJSON = withObject "ItemExt" $ \obj -> do
        id   <- obj .: "id"
        name <- obj .: "name"
        desc <- obj .: "desc"
        loc  <- obj .: "loc"
        pure $ ItemExt id name desc loc

data GameExt = GameExt
    { _gameExtLocation  :: String
    , _gameExtLocations :: [LocExt]
    , _gameExtItems     :: [ItemExt]
    }
    deriving Show

L.makeFields ''GameExt

instance FromJSON GameExt where
    parseJSON = withObject "GameExt" $ \obj -> do
        location  <- obj .: "location"
        locations <- obj .: "locations"
        items     <- obj .: "items"
        pure $ GameExt location locations items

foldLocId :: (MonadState Game m)
          => Map String Int
          -> LocExt
          -> m (Map String Int)
foldLocId m l = do
    let nameId = l L.^. Ext.id
    if M.member nameId m
        then pure m
        else do
            uid <- genUid
            pure $ M.insert nameId uid m

toGame :: (MonadState Game m) => GameExt -> m Game
toGame g = do
    locUid <- genUid
    let locsMap = M.insert (g L.^. location) locUid M.empty
    locsMap' <- foldM foldLocId locsMap $ g L.^. locations
    let loc    = 0
    let locs   = M.empty
    let conns  = []
    let npcs   = []
    let items  = []
    let conts  = []
    let input  = []
    let gen    = 0
    let ingest = Ingest locsMap' 
    pure $ Game loc locs conns npcs items conts input gen ingest

toLoc :: (MonadState Game m) => [LocExt] -> m [Loc]
toLoc ls = do
    traverse
        (\l -> do
            uid <- genUid
            pure $ Loc uid (l L.^. name) (l L.^. walkDesc) (l L.^. lookDesc)
        )
        ls
