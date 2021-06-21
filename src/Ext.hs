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
import qualified Data.ByteString.Lazy          as LB
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
        return $ LocExt id name walkDesc lookDesc

data ItemExt = ItemExt
    { _itemExtId   :: String
    , _itemExtName :: String
    , _itemExtDesc :: String
    , _itemExtLoc  :: String
    } deriving Show

makeFields ''ItemExt

instance FromJSON ItemExt where
  parseJSON = withObject "ItemExt" $ \obj -> do
      id <- obj .: "id"
      name <- obj .: "name"
      desc <- obj .: "desc"
      loc <- obj .: "loc"
      return $ ItemExt id name desc loc

data GameExt = GameExt
    { _gameExtLocations :: [LocExt]
    , _gameExtItems     :: [ItemExt]
    }
    deriving Show

makeFields ''GameExt

instance FromJSON GameExt where
    parseJSON = withObject "GameExt" $ \obj -> do
        locations <- obj .: "locations"
        items     <- obj .: "items"
        return $ GameExt locations items 


toLoc :: (MonadState Game m) => [LocExt] -> m [Loc]
toLoc ls = do
    traverse
        (\l -> do
            uid <- genUid
            pure $ Loc uid (l ^. name) (l ^. walkDesc) (l ^. lookDesc)
        )
        ls

