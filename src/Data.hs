{-# LANGUAGE FlexibleContexts #-}

module Data where

import           Control.Lens                   ( (.=) )
import           Control.Monad.State.Lazy       ( MonadState )
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import           Types
import           Util                           ( genUid )

initState :: Game
initState = Game Start [] [] [] [] [] 0

initWorld :: MonadState Game m => m ()
initWorld = do
    startUid <- genUid 
    let start = Loc2 startUid Start "Walk Start" "Look Start"
    nextUid <- genUid
    let next = Loc2 nextUid Next "Walk Next" "Look Next"
    endUid <- genUid
    let end = Loc2 endUid End "Walk End" "Look End"
    locs .= [start, next, end]
    joeUid <- genUid
    let joe = Npc
            joeUid
            "Joe"
            "Joe is old and withered."
            DialogRole
            Start
            True
            [ "Greetings, adventurer!"
            , "It's dangerous to go alone! Take this."
            , "What a gloomy day.."
            ]
            0
            []
    npcs .= [joe]
    lunchboxUid <- genUid
    let lunchbox = Container lunchboxUid
                             "Lunchbox"
                             "A vintage lunchbox."
                             Start
                             Closed
    containers .= [lunchbox]
    let trunk = Item "Trunk"
                     "A cardboard box with attractive logos decorating it."
                     (ItemLoc Next)
        macbook =
            Item "Macbook" "A standard Apple Macbook Pro." (ItemLoc End)
        snack = Item "Snack" "It looks delicious." (ItemLoc Start)
        spoon = Item "Spoon" "A normal spoon." (ItemContainer lunchboxUid)
    items .= [trunk, macbook, snack, spoon]

conns :: [Connection]
conns = 
  [ Connection Start S Next
  , Connection Next N Start
  , Connection Next S End
  , Connection End N Next
  ]

-- conns :: [Connection]
-- conns =
--     [ Connection Lobby     U Second
--     , Connection Second    D Lobby
--     , Connection Second    U Third
--     , Connection Third     D Second
--     , Connection Third     U Fourth
--     , Connection Fourth    D Third
--     , Connection Fourth    U Fifth
--     , Connection Fifth     D Fourth
--     , Connection Fifth     U Sixth
--     , Connection Sixth     D Fifth
--     , Connection Sixth     U Seventh
--     , Connection Seventh   D Sixth
--     , Connection Seventh   W Roof
--     , Connection Roof      E Seventh
--     , Connection Third     S LunchRoom
--     , Connection LunchRoom N Third
--     ]

-- walkDescMap :: M.Map Loc String
-- walkDescMap = M.fromList
--     [ (Lobby    , "You are standing in a lobby.")
--     , (Second   , "You are standing at the landing of the 2nd Floor.")
--     , (Third    , "You are standing at the landing of the 3rd Floor.")
--     , (Fourth   , "You are standing at the landing of the 4th Floor.")
--     , (Fifth    , "You are standing at the landing of the 5th Floor.")
--     , (Sixth    , "You are standing at the landing of the 6th Floor.")
--     , (Seventh  , "You are standing at the landing of the 7th Floor.")
--     , (Roof     , "You walk out onto the roof.,")
--     , (LunchRoom, "You are standing in a bustling lunch room.")
--     ]

-- lookMap :: M.Map Loc String
-- lookMap = M.fromList
--     [ (Lobby , "You are standing in the lobby of 325 W Ohio in Chicago IL.")
--     , (Fourth, "You hear the whirring buzz of hundreds of Macbooks..")
--     , (Roof  , "You are standing on a sunny roof deck.")
--     ]
