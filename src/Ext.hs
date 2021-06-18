{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Ext where

import Control.Monad.State.Lazy
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

instance FromJSON LocExt where
    parseJSON = withObject "LocExt" $ \obj -> do
        name     <- obj .: "name"
        walkDesc <- obj .: "walkDesc"
        lookDesc <- obj .: "lookDesc"
        return LocExt { Ext.name = name, Ext.walkDesc = walkDesc, Ext.lookDesc = lookDesc }

testLocExt :: LB.ByteString
testLocExt =
    "{\"name\":\"Bathroom\",\"walkDesc\":\"You have entered a dingy bathroom.\",\"lookDesc\":\"This bathroom is filthy.\"}"

toLoc :: (MonadState Game m) => LocExt -> m Loc
toLoc ext = do
  uid <- genUid
  pure $ Loc uid (Ext.name ext) (Ext.walkDesc ext) (Ext.lookDesc ext)
