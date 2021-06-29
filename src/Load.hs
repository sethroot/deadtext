{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Load where

import qualified Control.Lens                  as L
import           Control.Monad                  ( foldM )
import           Control.Monad.State.Lazy       ( MonadState )
import           Data.Aeson                     ( (.:)
                                                , (.:?)
                                                , FromJSON(parseJSON)
                                                , withObject
                                                )
import           Data.Aeson.Types               ( Parser )
import qualified Data.ByteString.Lazy          as LB
import qualified Data.List                     as DL
                                                ( findIndex )
import           Data.Map.Strict               as M
                                                ( Map
                                                , empty
                                                , foldrWithKey
                                                , insert
                                                , lookup
                                                , map
                                                , member
                                                )
import           Data.Maybe
import           Data.Types.Injective           ( Injective(..) )
import           GHC.Generics            hiding ( to )
import           Types
import           UID                            ( genUid )

type LocsMap = M.Map String UID
type NpcMap = M.Map String UID
type ContsMap = M.Map String UID

-- Location

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

toLocation :: LocExt -> Loc
toLocation l = Loc (l L.^. name) (l L.^. walkDesc) (l L.^. lookDesc)

instance Injective LocExt Loc where
    to = toLocation

-- Item

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
            "location"  -> pure . ItemLocExt . fromJust $ locData
            "container" -> pure . ItemContainerExt . fromJust $ locData
            _           -> pure ItemInvExt

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

-- Container

data ContainerExt = ContainerExt
    { _containerExtId     :: String
    , _containerExtName   :: String
    , _containerExtDesc   :: String
    , _containerExtLoc    :: String
    , _containerExtCState :: String
    }
    deriving Show

L.makeFields ''ContainerExt

instance FromJSON ContainerExt where
    parseJSON = withObject "ContainerExt" $ \obj -> do
        id     <- obj .: "id"
        name   <- obj .: "name"
        desc   <- obj .: "desc"
        loc    <- obj .: "loc"
        cState <- obj .: "state"
        pure $ ContainerExt id name desc loc cState

toContainer :: ContsMap -> LocsMap -> ContainerExt -> Container
toContainer contsMap locsMap container =
    let id'     = fromJust $ M.lookup (container L.^. Load.id) contsMap
        name'   = container L.^. name
        desc'   = container L.^. desc
        loc'    = fromJust $ M.lookup (container L.^. loc) locsMap
        cState' = case container L.^. cState of
            "open"   -> Open
            "closed" -> Closed
            _        -> Closed
    in  Container id' name' desc' loc' cState'

newtype ContainerInj = ContainerInj (ContsMap, LocsMap, ContainerExt)

instance Injective ContainerInj Container where
    to (ContainerInj (conts, locs, c)) = toContainer conts locs c

-- Connection

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

-- Direction

toDir :: String -> Direction
toDir d = case d of
    "N" -> N
    "S" -> S
    "E" -> E
    "W" -> W
    "U" -> U
    "D" -> D
    _   -> N

instance Injective String Direction where
    to = toDir

toConnection :: LocsMap -> ConnectionExt -> Connection
toConnection m c =
    let lookup getter = fromJust $ M.lookup (c L.^. getter) m
        start' = lookup start
        end'   = lookup end
        dir'   = to $ c L.^. dir
    in  Connection start' dir' end'

newtype ConnectionInj = ConnectionInj (LocsMap, ConnectionExt)

instance Injective ConnectionInj Connection where
    to (ConnectionInj (m, c)) = toConnection m c

-- NPC

data NpcRoleExt =
  DialogRole
  | QuestRole
  deriving (Show, Generic, FromJSON)

L.makeFields  ''NpcRoleExt

instance Injective NpcRoleExt Role where
    to r = case r of
        Load.DialogRole -> Types.DialogRole
        Load.QuestRole  -> Types.QuestRole

data NpcExt = NpcExt
    { _npcExtId           :: String
    , _npcExtName         :: String
    , _npcExtDesc         :: String
    , _npcExtRole         :: String
    , _npcExtLoc          :: String
    , _npcExtAlive        :: Bool
    , _npcExtDialog       :: [String]
    , _npcExtDialogCursor :: Int
    , _npcExtQuest        :: [String]
    }
    deriving Show

L.makeFields ''NpcExt

instance FromJSON NpcExt where
    parseJSON = withObject "NpcExt" $ \obj -> do
        id           <- obj .: "id"
        name         <- obj .: "name"
        desc         <- obj .: "desc"
        role         <- obj .: "role"
        loc          <- obj .: "loc"
        alive        <- obj .: "alive"
        dialog       <- obj .: "dialog"
        dialogCursor <- obj .: "dialogCursor"
        quest        <- obj .: "quest"
        pure $ NpcExt id name desc role loc alive dialog dialogCursor quest

toNpc :: NpcMap -> LocsMap -> NpcExt -> Npc
toNpc npcMap locsMap n =
    let id'   = fromJust $ M.lookup (n L.^. Load.id) npcMap
        name' = n L.^. name
        desc' = n L.^. desc
        role' = case n L.^. role of
            "dialog" -> Types.DialogRole
            "quest"  -> Types.QuestRole
            _        -> Types.DialogRole
        loc'          = fromJust $ M.lookup (n L.^. loc) locsMap
        alive'        = n L.^. alive
        dialog'       = n L.^. dialog
        dialogCursor' = n L.^. dialogCursor
        quest'        = []
    in  Npc id' name' desc' role' loc' alive' dialog' dialogCursor' quest'

newtype NpcInj = NpcInj (NpcMap, LocsMap, NpcExt)

instance Injective NpcInj Npc where
    to (NpcInj (npcMap, locsMap, n)) = toNpc npcMap locsMap n

-- Game

data GameExt = GameExt
    { _gameExtLocation    :: String
    , _gameExtLocations   :: [LocExt]
    , _gameExtItems       :: [ItemExt]
    , _gameExtContainers  :: [ContainerExt]
    , _gameExtConnections :: [ConnectionExt]
    , _gameExtNpcs        :: [NpcExt]
    }
    deriving Show

L.makeFields ''GameExt

instance FromJSON GameExt where
    parseJSON = withObject "GameExt" $ \obj -> do
        location    <- obj .: "location"
        locations   <- obj .: "locations"
        items       <- obj .: "items"
        containers  <- obj .: "containers"
        connections <- obj .: "connections"
        npcs        <- obj .: "npcs"
        pure $ GameExt location locations items containers connections npcs

foldIdGen :: (MonadState Game m, HasId s String)
          => Map String Int
          -> s
          -> m (Map String Int)
foldIdGen m s = do
    let stringId = s L.^. Load.id
    if M.member stringId m
        then pure m
        else do
            uid <- genUid
            pure $ M.insert stringId uid m

invertMap :: M.Map String Int -> M.Map Int String
invertMap = M.foldrWithKey (flip M.insert) M.empty

nameToLoc :: [LocExt] -> [Loc] -> String -> Loc
nameToLoc les ls extId =
    let
        locExtIdx =
            fromJust $ DL.findIndex (\le -> (le L.^. Load.id) == extId) les
        locExt   = les !! locExtIdx
        locIndex = fromJust
            $ DL.findIndex (\l -> (l L.^. loc) == (locExt L.^. name)) ls
    in
        ls !! locIndex

toItemLoc :: Map String Int -> Map String Int -> ItemLocExt -> ItemLocation
toItemLoc locsMap contsMap itemLoc = case itemLoc of
    ItemInvExt         -> ItemInv
    ItemLocExt       s -> ItemLoc . fromJust $ M.lookup s locsMap
    ItemNpcExt       s -> ItemNpc . fromJust $ M.lookup s locsMap
    ItemContainerExt s -> ItemContainer . fromJust $ M.lookup s contsMap

toItem :: Map String Int -> Map String Int -> ItemExt -> Item
toItem locsMap contsMap ie =
    let loc' = toItemLoc locsMap contsMap $ ie L.^. loc
    in  Item (ie L.^. name) (ie L.^. desc) loc'

toGame :: MonadState Game m => GameExt -> m Game
toGame g = do
    -- Starting location
    locUid <- genUid

    let locExts = g L.^. locations
    -- Construct a map of the external string IDs to generated UIDs
    -- First insert the starting location
    -- Then, fold the remaining locations into the map, inserting new entries
    -- with associated UIDs
    -- ["overlook_bath": 0]
    locsMap <- do
        let justStartLoc = M.insert (g L.^. location) locUid M.empty
        foldM foldIdGen justStartLoc locExts
    -- Invert the map from [String: UID] to [UID: String]
    -- [0: "overlook_bath"]
    let invLocsMap = invertMap locsMap
    -- Create Locs from LocExts
    -- [LocExt] -> [Loc]
    let locs       = fmap to locExts
    -- Construct map of [UID: Loc] for use in engine
    -- [0: "overlook_bath"] -> [0: Loc loc walk look]
    let locs'      = M.map (nameToLoc locExts locs) invLocsMap

    let contExts   = g L.^. containers
    -- ["car": 0]
    contsMap <- foldM foldIdGen M.empty contExts
    let contExtInjs = fmap (\c -> ContainerInj (contsMap, locsMap, c)) contExts
    let conts       = fmap to contExtInjs

    let connExts    = g L.^. connections
    let connExtInjs = fmap (\c -> ConnectionInj (locsMap, c)) connExts
    let conns       = fmap to connExtInjs

    let itemExts    = g L.^. items
    let items'      = fmap (toItem locsMap contsMap) itemExts

    let npcExts     = g L.^. npcs
    npcMap <- foldM foldIdGen M.empty npcExts
    let npcExtInjs = fmap (\n -> NpcInj (npcMap, locsMap, n)) npcExts
    let npcs       = fmap to npcExtInjs

    let input      = []
    let gen        = 0
    let ingest     = Ingest locsMap
    pure $ Game locUid locs' conns npcs items' conts input gen ingest
