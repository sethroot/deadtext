{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Ext where

import           Control.Lens
import           Control.Monad.State.Lazy
import           Data.Aeson
import           Data.Aeson.Types               ( Parser )
import qualified Data.ByteString.Lazy          as LB
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

makeFields ''LocExt

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
            _          -> pure $ ItemInvExt

data ItemExt = ItemExt
    { _itemExtId   :: String
    , _itemExtName :: String
    , _itemExtDesc :: String
    , _itemExtLoc  :: ItemLocExt 
    }
    deriving Show

makeFields ''ItemExt

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

makeFields ''GameExt

instance FromJSON GameExt where
    parseJSON = withObject "GameExt" $ \obj -> do
        location  <- obj .: "location"
        locations <- obj .: "locations"
        items     <- obj .: "items"
        pure $ GameExt location locations items


toLoc :: (MonadState Game m) => [LocExt] -> m [Loc]
toLoc ls = do
    traverse
        (\l -> do
            uid <- genUid
            pure $ Loc uid (l ^. name) (l ^. walkDesc) (l ^. lookDesc)
        )
        ls

