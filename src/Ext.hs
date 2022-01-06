{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Ext where

import qualified Control.Lens as L
import Control.Monad.State (MonadState, foldM)
import Data.Aeson ((.:), (.:?), FromJSON(parseJSON), withObject)
import Data.Aeson.Types (Parser)
import qualified Data.List as DL (findIndex)
import Data.Map.Strict as M
    (Map, empty, foldrWithKey, insert, lookup, map, member)
import Data.Maybe (fromJust)
import Data.Types.Injective (Injective(..))
import GHC.Generics (Generic)
import Types
import UID (genUid)

type LocExtsMap = M.Map String UID
type NpcExtMap = M.Map String UID
type ContExtsMap = M.Map String UID
type EngineLocMap = M.Map UID String

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

invertMap :: LocExtsMap -> EngineLocMap
invertMap = M.foldrWithKey (flip M.insert) M.empty

nameToLoc :: [LocExt] -> [Loc] -> String -> Loc
nameToLoc les ls extId =
    let
        locExtIdx =
            fromJust $ DL.findIndex (\le -> (le L.^. Ext.id) == extId) les
        locExt   = les !! locExtIdx
        locIndex = fromJust
            $ DL.findIndex (\l -> (l L.^. loc) == (locExt L.^. name)) ls
    in ls !! locIndex

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

toItem :: LocExtsMap -> ContExtsMap -> ItemExt -> Item
toItem locsMap contsMap ie =
    let loc' = toItemLoc locsMap contsMap $ ie L.^. loc
    in Item (ie L.^. name) (ie L.^. desc) loc'

toItemLoc :: LocExtsMap -> ContExtsMap -> ItemLocExt -> ItemLocation
toItemLoc locsMap contsMap itemLoc = case itemLoc of
    ItemInvExt         -> ItemInv
    ItemLocExt       s -> ItemLoc . fromJust $ M.lookup s locsMap
    ItemNpcExt       s -> ItemNpc . fromJust $ M.lookup s locsMap
    ItemContainerExt s -> ItemContainer . fromJust $ M.lookup s contsMap

-- Container

data ContainerExt = ContainerExt
    { _containerExtId     :: String
    , _containerExtName   :: String
    , _containerExtLook   :: String
    , _containerExtDesc   :: String
    , _containerExtLoc    :: String
    , _containerExtCState :: String
    , _containerExtTrans  :: Bool
    }
    deriving Show

L.makeFields ''ContainerExt

instance FromJSON ContainerExt where
    parseJSON = withObject "ContainerExt" $ \obj -> do
        id     <- obj .: "id"
        name   <- obj .: "name"
        look   <- obj .: "look"
        desc   <- obj .: "desc"
        loc    <- obj .: "loc"
        cState <- obj .: "state"
        trans  <- obj .: "transparent"
        pure $ ContainerExt id name look desc loc cState trans

toContainer :: ContExtsMap -> LocExtsMap -> ContainerExt -> Container
toContainer contsMap locsMap container =
    let
        id'     = fromJust $ M.lookup (container L.^. Ext.id) contsMap
        name'   = container L.^. name
        look'   = container L.^. look
        desc'   = container L.^. desc
        loc'    = fromJust $ M.lookup (container L.^. loc) locsMap
        cState' = case container L.^. cState of
            "open"   -> Open
            "closed" -> Closed
            _        -> Closed
        trans'  = container L.^. trans
    in Container id' name' look' desc' loc' cState' trans'

newtype ContainerInj = ContainerInj (ContExtsMap, LocExtsMap, ContainerExt)

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
    "N"  -> N
    "S"  -> S
    "E"  -> E
    "W"  -> W
    "NW" -> NW
    "NE" -> NE
    "SW" -> SW
    "SE" -> SE
    "U"  -> U
    "D"  -> D
    _    -> N

instance Injective String Direction where
    to = toDir

toConnection :: LocExtsMap -> ConnectionExt -> Connection
toConnection m c =
    let
        lookup getter = fromJust $ M.lookup (c L.^. getter) m
        start' = lookup start
        end'   = lookup end
        dir'   = to $ c L.^. dir
    in Connection start' dir' end'

newtype ConnectionInj = ConnectionInj (LocExtsMap, ConnectionExt)

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
        Ext.DialogRole -> Types.DialogRole
        Ext.QuestRole  -> Types.QuestRole

data NpcExt = NpcExt
    { _npcExtId           :: String
    , _npcExtName         :: String
    , _npcExtGender       :: String
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
        gender       <- obj .: "gender"
        desc         <- obj .: "desc"
        role         <- obj .: "role"
        loc          <- obj .: "loc"
        alive        <- obj .: "alive"
        dialog       <- obj .: "dialog"
        dialogCursor <- obj .: "dialogCursor"
        quest        <- obj .: "quest"
        pure $ NpcExt
            id
            name
            gender
            desc
            role
            loc
            alive
            dialog
            dialogCursor
            quest

toNpc :: NpcExtMap -> LocExtsMap -> NpcExt -> Npc
toNpc npcMap locsMap n =
    let
        id'           = fromJust $ M.lookup (n L.^. Ext.id) npcMap
        name'         = n L.^. name
        gender'       = case n L.^. gender of
            "male"      -> Male
            "female"    -> Female
            "nonbinary" -> NonBinary
            _           -> Unknown
        desc'         = n L.^. desc
        role'         = case n L.^. role of
            "dialog" -> Types.DialogRole
            "quest"  -> Types.QuestRole
            _        -> Types.DialogRole
        loc'          = fromJust $ M.lookup (n L.^. loc) locsMap
        alive'        = n L.^. alive
        dialog'       = n L.^. dialog
        dialogCursor' = n L.^. dialogCursor
        quest'        = []
    in Npc
        id'
        name'
        gender'
        desc'
        role'
        loc'
        alive'
        dialog'
        dialogCursor'
        quest'

newtype NpcInj = NpcInj (NpcExtMap, LocExtsMap, NpcExt)

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

toGame :: MonadState Game m => GameExt -> m Game
toGame g = do
    -- Starting location
    locUid <- genUid

    let locExts = g L.^. locations

    {-
      Map external ID to internal UID
      Will be used provide UIDs to other types
      ["overlook_bath": 0]
    -}
    locExtsMap <- do
        let justStartLoc = M.insert (g L.^. location) locUid M.empty
        foldM foldIdGen justStartLoc locExts

    {- 
      Invert the ID map
      [0: "overlook_bath"] 
    -}
    let inverted  = invertMap locExtsMap

    -- Inject LocExt to Loc
    let locs      = fmap to locExts

    {-
      Transform external ID to Loc to complete engine map
      [0: Loc]
    -}
    let transform = nameToLoc locExts locs
    let locsMap   = M.map transform inverted

    -- Containers
    let contExts  = g L.^. containers
    contsExtMap <- foldM foldIdGen M.empty contExts
    let
        contExtInjs =
            fmap (\c -> ContainerInj (contsExtMap, locExtsMap, c)) contExts
    let conts       = fmap to contExtInjs

    -- Connections
    let connExts    = g L.^. connections
    let connExtInjs = fmap (\c -> ConnectionInj (locExtsMap, c)) connExts
    let conns       = fmap to connExtInjs

    -- Items
    let itemExts    = g L.^. items
    let items       = fmap (toItem locExtsMap contsExtMap) itemExts

    -- Npcs
    let npcExts     = g L.^. npcs
    npcMap <- foldM foldIdGen M.empty npcExts
    let npcExtInjs = fmap (\n -> NpcInj (npcMap, locExtsMap, n)) npcExts
    let npcs       = fmap to npcExtInjs

    -- Other
    let input      = []
    let gen        = 0

    -- Complete, loaded Game
    pure $ Game locUid locsMap conns npcs items conts input gen

-- Common

foldIdGen :: (MonadState Game m, HasId s String)
          => Map String Int
          -> s
          -> m (Map String Int)
foldIdGen m s = do
    let stringId = s L.^. Ext.id
    if M.member stringId m
        then pure m
        else do
            uid <- genUid
            pure $ M.insert stringId uid m
