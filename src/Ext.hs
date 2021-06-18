{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Ext where

import           Control.Monad.State.Lazy
import           Data.Aeson
import qualified Data.ByteString.Lazy          as LB
import           Types
import           UID

data LocExt = LocExt
    { name     :: String
    , walkDesc :: String
    , lookDesc :: String
    }
    deriving Show

newtype LocsExt
  = LocsExt {locations :: [LocExt]}
  deriving Show

instance FromJSON LocExt where
    parseJSON = withObject "LocExt" $ \obj -> do
        name     <- obj .: "name"
        walkDesc <- obj .: "walkDesc"
        lookDesc <- obj .: "lookDesc"
        return LocExt { Ext.name     = name
                      , Ext.walkDesc = walkDesc
                      , Ext.lookDesc = lookDesc
                      }

instance FromJSON LocsExt where
    parseJSON = withObject "LocsExt" $ \obj -> do
        locations <- obj .: "locations"
        return LocsExt { Ext.locations = locations }

toLoc :: (MonadState Game m) => [LocExt] -> m [Loc]
toLoc ls = do
    traverse
        (\l -> do
            uid <- genUid
            pure $ Loc uid (Ext.name l) (Ext.walkDesc l) (Ext.lookDesc l)
        )
        ls

