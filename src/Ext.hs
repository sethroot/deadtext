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
import qualified Data.List                     as DL
                                                ( findIndex )
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

data ConnectionExt = ConnectionExt
    { _connectionExtStart :: String
    , _connectionExtEnd   :: String
    , _connectionExtDir   :: String
    }
    deriving Show

L.makeFields ''ConnectionExt

instance FromJSON ConnectionExt where
    parseJSON = withObject "ConnectionExt" $ \obj -> do
        start <- obj .: "start"
        end   <- obj .: "end"
        dir   <- obj .: "dir"
        pure $ ConnectionExt start end dir

data GameExt = GameExt
    { _gameExtLocation    :: String
    , _gameExtLocations   :: [LocExt]
    , _gameExtItems       :: [ItemExt]
    , _gameExtConnections :: [ConnectionExt]
    }
    deriving Show

L.makeFields ''GameExt

instance FromJSON GameExt where
    parseJSON = withObject "GameExt" $ \obj -> do
        location    <- obj .: "location"
        locations   <- obj .: "locations"
        items       <- obj .: "items"
        connections <- obj .: "connections"
        pure $ GameExt location locations items connections

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

toDir :: String -> Direction
toDir d = case d of
    "N" -> N
    "S" -> S
    "E" -> E
    "W" -> W
    "U" -> U
    "D" -> D
    _   -> N


toConnection :: M.Map String Int -> ConnectionExt -> Connection
toConnection m c =
    let lookup getter = fromJust $ M.lookup (c L.^. getter) m
        start' = lookup start
        end'   = lookup end
        dir'   = toDir $ c L.^. dir
    in  Connection start' dir' end'

toLocation :: LocExt -> Loc
toLocation l = Loc (l L.^. name) (l L.^. walkDesc) (l L.^. lookDesc)

invertMap :: M.Map String Int -> M.Map Int String
invertMap = M.foldrWithKey (flip M.insert) M.empty

nameToLoc :: [LocExt] -> [Loc] -> String -> Loc
nameToLoc les ls extId =
    let
        locExtIdx =
            fromJust $ DL.findIndex (\le -> (le L.^. Ext.id) == extId) les
        locExt = les !! locExtIdx
        locIndex =
            fromJust $ DL.findIndex (\l -> (l L.^. loc) == (locExt L.^. name)) ls
    in
        ls !! locIndex

toGame :: MonadState Game m => GameExt -> m Game
toGame g = do
    locUid <- genUid
    let locations' = g L.^. locations

    -- Construct a map of the external string IDs to generated UIDs
    -- First insert the starting location
    -- Then, fold the remaining locations into the map, inserting new entries
    -- with associated UIDs
    -- [("overlook_bath": 0]
    let locsMap    = M.insert (g L.^. location) locUid M.empty
    locsMap' <- foldM foldLocId locsMap locations'

    -- Invert the map from [String: UID] to [UID: String]
    -- [0: "overlook_bath"]
    let invLocsMap = invertMap locsMap'

    -- Create Locs from LocExts
    -- [LocExt] -> [Loc]
    let locs       = fmap toLocation locations'

    -- Construct map of [UID: Loc] for use in engine
    -- [0: "overlook_bath"] -> [0: Loc loc walk look]
    let locs'      = M.map (nameToLoc locations' locs) invLocsMap

    let conns      = g L.^. connections
    let conns'     = fmap (toConnection locsMap') conns
    let loc        = 0
    let locs       = M.empty
    let npcs       = []
    let items      = []
    let conts      = []
    let input      = []
    let gen        = 0
    let ingest     = Ingest locsMap'
    pure $ Game locUid locs' conns' npcs items conts input gen ingest

-- toLoc :: (MonadState Game m) => [LocExt] -> m [Loc]
-- toLoc ls = do
--     traverse
--         (\l -> do
--             uid <- genUid
--             pure $ Loc uid (l L.^. name) (l L.^. walkDesc) (l L.^. lookDesc)
--         )
--         ls
