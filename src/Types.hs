{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Types where

import Control.Lens (makeFields)
import Control.Monad.State.Lazy (StateT)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON(toJSON), ToJSONKey, object)
import Data.Aeson.Types (FromJSON(parseJSON))
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

data MapLoc = OverlookBathroom | OverlookParkingLot | Cemetary | Room202

data Role = Monster | Dialog | Quest
    deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

-- data Quest = Quest
--     { _questStartText      :: String
--     , _questCompletionText :: String
--     , _questCursor         :: Int
--     , _questSteps          :: [String]
--     }
--     deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

-- Progress types
-- picked up item
-- talked to npc
-- killed key enemy
-- looked at item
-- gave item to npc
-- placed item in container
-- 
-- data Progress = Progress
--     { _

--     }

data ProgressType = PickedUpItem |
       TalkedToNPC |
    KilledKeyEnemy |
    LookedAtItem |
    GaveItemToNpc |
    PlacedItemInContainer

data Avatar = Avatar
    { _avatarName   :: String
    , _avatarCombat :: Combat
    }
    deriving (Generic, FromJSON, ToJSON)


instance Show Avatar where
    show (Avatar name _) = name

data Npc = Npc
    { _npcUid          :: UID
    , _npcName         :: String
    , _npcGender       :: Gender
    , _npcAlignment    :: Alignment
    , _npcActive       :: Bool
    , _npcHealth       :: Int
    , _npcCombat       :: [UID]
    , _npcDesc         :: String
    , _npcRole         :: Role
    , _npcLoc          :: UID
    , _npcAlive        :: Bool
    , _npcDialog       :: [String]
    , _npcDialogCursor :: Int
    -- , _npcQuest        :: [Quest]
    }
    deriving (Show, Eq, Ord, Generic, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

data Scene = Scene
    { _sceneLoc          :: UID
    , _sceneDialog       :: [SceneDialog]
    , _sceneDialogCursor :: Int
    , _sceneDescription  :: [String]
    }
    deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data SceneDialog =
    SceneDialog
      { _sceneDialogSpeaker   :: UID
      , _sceneDialogStatement :: String
      }
    | SceneNarration
      {
          _sceneNarrationText :: String
      }
      deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data Gender = Male | Female | NonBinary | Unknown
    deriving (Show, Eq, Ord, Generic, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

data Alignment = Neutral | Hostile
    deriving (Show, Eq, Ord, Generic, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

data CombatKind = Melee | Ranged
    deriving (Show, Eq, Ord, Generic, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

data CombatEffect = None | Stun
    deriving (Show, Eq, Ord, Generic, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

data Combat = Combat
    { _combatUid         :: UID
    , _combatKind        :: CombatKind
    , _combatDamage      :: Int
    , _combatEffect      :: CombatEffect
    , _combatDescription :: Npc -> String
    }
    deriving Generic
    -- deriving (Show, Eq, Ord, Generic, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

instance FromJSON Combat where
    parseJSON _ = return $ Combat 0 Melee 10 None (const "")

instance ToJSON Combat where
    toJSON _ = object []

data NpcRel = NpcRel
    { _npcRelLevel :: Int
    , _npcRelKnown :: Bool
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
    , _itemSyn  :: [String]
    , _itemDesc :: String
    , _itemLoc  :: ItemLocation
    , _itemUses :: [ItemUse]
    }
    deriving (Show, Eq, Ord, Generic)

data ItemUse = Eat | Drink | Readable | Key | Ammo | OnOff
    deriving (Show, Eq, Ord, Generic)
-- data Usable a where
--      UseEdible ::Usable Edible
--      UseKey ::Usable Key
--      UseAmmo ::Usable Ammo
--      NoUse ::Usable ()

data ContainerState = Open | Closed
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data ContainerTransparency = Opaque | Transparent
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data Container = Container
    { _containerUid    :: UID
    , _containerName   :: String
    , _containerLook   :: String
    , _containerDesc   :: String
    , _containerLoc    :: UID
    , _containerCState :: ContainerState
    , _containerTrans  :: ContainerTransparency
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
    deriving (Show, Generic)

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

data Movement = Movement
    { _movementStart     :: UID
    , _movementDirection :: Direction
    }
    deriving (Show, Generic, FromJSON, ToJSON)

data Input = Input
    { _inputRaw    :: String
    , _inputNormal :: String
    }
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Game = Game
    { _gameLoc         :: UID
    , _gameLocs        :: M.Map UID Loc
    , _gameConnections :: [Connection]
    , _gameAvatar      :: Maybe Avatar
    , _gameNpcs        :: [Npc]
    , _gameCombats     :: [Combat]
    , _gameScenes      :: [Scene]
    , _gameItems       :: [Item]
    , _gameContainers  :: [Container]
    , _gameInput       :: [Input]
    , _gameUidGen      :: Int
    }

data Env = Env {}

type App = ReaderT Env (StateT Game IO)



makeFields ''Direction
makeFields ''Loc
makeFields ''ItemLocation
makeFields ''Item
makeFields ''Container
makeFields ''Obj
makeFields ''Ext
makeFields ''Avatar
makeFields ''Npc
makeFields ''Combat
makeFields ''Role
-- makeFields ''Quest
makeFields ''Movement
makeFields ''Input
makeFields ''Game
makeFields ''Location
makeFields ''Connection
makeFields ''Env

