{-# LANGUAGE FlexibleContexts #-}

module Data where

import           Control.Lens                   ( (.=) )
import           Control.Monad.State.Lazy       ( MonadState )
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import           Types
import           UID                            ( genUid )

initState :: Game
initState = Game 0 M.empty [] [] [] [] [] 0 (Ingest M.empty)

initWorld :: MonadState Game m => m ()
initWorld = do
    startUid <- genUid
    let start = Loc startUid "Start" "Walk Start" "Look Start"
    nextUid <- genUid
    let next = Loc nextUid "Next" "Walk Next" "Look Next"
    endUid <- genUid
    let end = Loc endUid "End" "Walk End" "Look End"
    locs .= M.fromList [(startUid, start), (nextUid, next), (endUid, end)]
    connections
        .= [ Connection startUid S nextUid
           , Connection nextUid  N startUid
           , Connection nextUid  S endUid
           , Connection endUid   N nextUid
           ]
    joeUid <- genUid
    let joe = Npc
            joeUid
            "Joe"
            "Joe is old and withered."
            DialogRole
            startUid
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
                             startUid 
                             Closed
    containers .= [lunchbox]
    let trunk = Item "Trunk"
                     "A cardboard box with attractive logos decorating it."
                     (ItemLoc nextUid)
        macbook = Item "Macbook" "A standard Apple Macbook Pro." (ItemLoc endUid)
        snack   = Item "Snack" "It looks delicious." (ItemLoc startUid)
        spoon   = Item "Spoon" "A normal spoon." (ItemContainer lunchboxUid)
    items .= [trunk, macbook, snack, spoon]
