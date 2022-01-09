{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens (makeFields)
import Control.Monad.State.Lazy (StateT)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import qualified Data.Map.Strict as M
import GHC.Generics (Generic)

type UID = Int

data Direction = N | S | E | W | NW | NE | SW | SE | U | D
  deriving (Eq, Generic, FromJSON, ToJSON)

instance Show Direction where
    show N  = "north"
    show S  = "south"
    show E  = "east"
    show W  = "west"
    show NW = "northwest"
    show NE = "northeast"
    show SW = "southwest"
    show SE = "southeast"
    show U  = "up"
    show D  = "down"

data Loc = Loc
    { _locLoc      :: String
    , _locWalkDesc :: String
    , _locLookDesc :: String
    }
    deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data Role = DialogRole
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
    , _npcGender       :: Gender
    , _npcDesc         :: String
    -- , _npcRel          :: NpcRel
    , _npcRole         :: Role
    , _npcLoc          :: UID
    , _npcAlive        :: Bool
    , _npcDialog       :: [String]
    , _npcDialogCursor :: Int
    , _npcQuest        :: [Quest]
    }
    deriving (Show, Eq, Ord, Generic, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

data Gender = Male | Female | NonBinary | Unknown
    deriving (Show, Eq, Ord, Generic, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

data NpcRel = NpcRel
    { _npcRelLevel       :: Int
    , _npcRelKnown       :: Bool
    , _npcRelDialog      :: [DialogGroup]
    , _npcRelDescription :: [String]
    }
    deriving (Show, Eq, Ord, Generic, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

newtype DialogGroup = DialogGroup [String]
    deriving (Show, Eq, Ord, Generic, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

data ItemLocation = ItemInv
    | ItemLoc UID
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
    { _containerUid    :: UID
    , _containerName   :: String
    , _containerLook   :: String
    , _containerDesc   :: String
    , _containerLoc    :: UID
    , _containerCState :: ContainerState
    , _containerTrans  :: Bool
    }
    deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data Obj
    = ObjNpc Npc
    | ObjItem Item
    | ObjInv Item
    | ObjCont Container

data Ext = Ext
    { _extNpcs  :: [Npc]
    , _extItems :: [Item]
    }
    deriving (Show, Generic, FromJSON, ToJSON)

data Connection = Connection
    { _connectionStart :: UID
    , _connectionDir   :: Direction
    , _connectionDest  :: UID
    }
    deriving (Show, Generic, FromJSON, ToJSON)

data Location = Location
    { _locationItem  :: Item
    , _locationPlace :: Loc
    }
    deriving (Show, Generic, FromJSON, ToJSON)

data Movement = Movement
    { _movementStart     :: UID
    , _movementDirection :: Direction
    }
    deriving (Show, Generic, FromJSON, ToJSON)

data Input = Input
    { _inputRaw    :: String
    , _inputNormal :: String
    }
    deriving (Show, Generic, FromJSON, ToJSON)

data Game = Game
    { _gameLoc         :: UID
    , _gameLocs        :: M.Map UID Loc
    , _gameConnections :: [Connection]
    , _gameNpcs        :: [Npc]
    , _gameItems       :: [Item]
    , _gameContainers  :: [Container]
    , _gameInput       :: [Input]
    , _gameUidGen      :: Int
    }
    deriving (Show, Generic, FromJSON, ToJSON)

data Env = Env
    { _envLocs :: M.Map UID Loc
    }

type App = ReaderT Env (StateT Game IO)

makeFields ''Direction
makeFields ''Loc
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
makeFields ''Env
