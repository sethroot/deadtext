{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import           Control.Lens                   ( makeFields )
import           Control.Monad.State.Lazy       ( StateT )
import           Data.Aeson                     ( FromJSON
                                                , FromJSONKey
                                                , ToJSON
                                                , ToJSONKey
                                                )
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import           GHC.Generics                   ( Generic )

type UID = Int

data Direction = N | S | E | W | U | D
  deriving (Eq, Generic, FromJSON, ToJSON)

instance Show Direction where
    show N = "north"
    show S = "south"
    show E = "east"
    show W = "west"
    show U = "up"
    show D = "down"

data Loc
  = Start 
  | Next 
  | End
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

-- data Loc
--   = Lobby
--   | Second
--   | Third
--   | Fourth
--   | Fifth
--   | Sixth
--   | Seventh
--   | Roof
--   | Elevator
--   | LunchRoom
--   deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data Loc2 = Loc2
    { _loc2Uid  :: UID
    , _loc2Loc :: Loc
    , _loc2WalkDesc :: String
    , _loc2LookDesc :: String
    }
    deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data Role
  = DialogRole
  | QuestRole
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data Quest = Quest
    { _questStartText      :: String
    , _questCompletionText :: String
    , _questCursor         :: Int
    , _questSteps          :: [String]
    }
    deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data Npc = Npc
    { _npcUid          :: UID
    , _npcName         :: String
    , _npcDesc         :: String
    , _npcRole         :: Role
    , _npcLoc          :: Loc
    , _npcAlive        :: Bool
    , _npcDialog       :: [String]
    , _npcDialogCursor :: Int
    , _npcQuest        :: [Quest]
    }
    deriving (Show, Eq, Ord, Generic, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

data ItemLocation
  = ItemInv
  | ItemLoc Loc
  | ItemNpc UID
  | ItemContainer UID
  deriving (Show, Eq, Ord, Generic, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

data Item = Item
    { _itemName :: String
    , _itemDesc :: String
    , _itemLoc  :: ItemLocation
    }
    deriving (Show, Eq, Ord, Generic, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

data ContainerState = Open | Closed
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data Container = Container
    { _containerUid    :: Int
    , _containerName   :: String
    , _containerDesc   :: String
    , _containerLoc    :: Loc
    , _containerCState :: ContainerState
    }
    deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data Obj
  = ObjNpc Npc
  | ObjItem Item
  | ObjInv Item

data Ext = Ext
    { _extNpcs  :: [Npc]
    , _extItems :: [Item]
    }
    deriving (Show, Generic, FromJSON, ToJSON)

data Connection = Connection
    { _connectionStart :: Loc
    , _connectionDir   :: Direction
    , _connectionDest  :: Loc
    }
    deriving (Show, Generic, FromJSON, ToJSON)

data Location = Location
    { _locationItem  :: Item
    , _locationPlace :: Loc
    }
    deriving (Show, Generic, FromJSON, ToJSON)

data Movement = Movement
    { _movementStart     :: Loc
    , _movementDirection :: Direction
    }
    deriving (Show, Generic, FromJSON, ToJSON)

data Input = Input
    { _inputRaw    :: String
    , _inputNormal :: String
    }
    deriving (Show, Generic, FromJSON, ToJSON)

data Game = Game
    { _gameLoc        :: Loc 
    , _gameLocs       :: [Loc2]
    , _gameNpcs       :: [Npc]
    , _gameItems      :: [Item]
    , _gameContainers :: [Container]
    , _gameInput      :: [Input]
    , _gameUidGen     :: Int
    }
    deriving (Show, Generic, FromJSON, ToJSON)

type GameLoop = StateT Game IO ()

makeFields ''Direction
makeFields ''Loc
makeFields ''Loc2
makeFields ''ItemLocation
makeFields ''Item
makeFields ''Container
makeFields ''Obj
makeFields ''Ext
makeFields ''Npc
makeFields ''Role
makeFields ''Quest
makeFields ''Movement
makeFields ''Input
makeFields ''Game
makeFields ''Location
makeFields ''Connection
