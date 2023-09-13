{-# LANGUAGE FlexibleContexts #-}

module Data where

import Control.Lens ((.=), (^.))
import Control.Monad.State.Lazy (MonadState)
import qualified Data.Map.Strict as M
import Types
import UID (genUid)

initState :: Game
initState = Game 0 M.empty [] Nothing [] [] [] [] [] [] 0

initEnv :: Env
initEnv = Env

setState :: MonadState Game m => m ()
setState = do
    bathroomUid <- genUid
    let
        bathroom = Loc
            "Bathroom"
            "You walk into a small, dimly lit bathroom. Along the wall are sinks with mirrors above them. In the air you smell mildew and ammonia."
            "You are standing in a small, dimly lit bathroom. Along the wall are sinks with mirrors above them. In the air you smell mildew and ammonia."
    overlookLotUid <- genUid
    let
        overlookLot = Loc
            "Overlook Parking Lot"
            "You enter a parking lot overlooking a lake."
            "You are standing in a parking lot overlooking a lake."
    cemetaryUid <- genUid
    let
        cemetary = Loc
            "A cemetary"
            "You have entered a cemetary surrounded by tall fencing"
            "You are standing in a cemetary surrounded by tall fencing. There are many graves here."
    room205Uid <- genUid
    let
        room205 = Loc
            "Room 202"
            "You have entered room 202. In the middle of the room, you see a mannequin. A bright light shines at you."
            "You are standing in room 202. In the middle of the room, you see a mannequin. A bright light shines at you."

    -- Wood Side Exterior

    woodSideExtWestUid <- genUid
    let woodSideExtWest = Loc "Wood Side Exterior West" "Apartment Ext West" ""

    woodSideExtEastUid <- genUid
    let woodSideExtEast = Loc "Wood Side Exterior East" "Apartment Ext East" ""

    -- Wood Side Apartments 1F

    westHallway1FSouthUid <- genUid
    let westHallway1FSouth = Loc "1st Floor West Hallway South" "1F West Hallway South" "1F West Hallway South"

    westHallway1FMidUid <- genUid
    let westHallway1FMid = Loc "1st Floor West Hallway Mid" "1F West Hallway Mid" "1F West Hallway Mid"

    westHallway1FNorthUid <- genUid
    let westHallway1FNorth = Loc "1st Floor West Hallway North" "1F West Hallway North" "1F West Hallway North"

    eastHallway1FSouthUid <- genUid
    let eastHallway1FSouth = Loc "1st Floor East Hallway South" "1F East Hallway South" "1F East Hallway South"

    eastHallway1FMidUid <- genUid
    let eastHallway1FMid = Loc "1st Floor East Hallway Mid" "1F East Hallway Mid" "1F East Hallway Mid"

    eastHallway1FNorthUid <- genUid
    let eastHallway1FNorth = Loc "1st Floor East Hallway North" "1F East Hallway North" "1F East Hallway North"

    room101Uid <- genUid
    let room101 = Loc "Room 101" "Room 101" "Room 101"
    room102Uid <- genUid
    let room102 = Loc "Room 102" "Room 102" "Room 102"
    room103Uid <- genUid
    let room103 = Loc "Room 103" "Room 103" "Room 103"
    room104Uid <- genUid
    let room104 = Loc "Room 104" "Room 104" "Room 104"
    room105Uid <- genUid
    let room105 = Loc "Room 105" "Room 105" "Room 105"
    room106Uid <- genUid
    let room106 = Loc "Room 106" "Room 106" "Room 106"
    room107Uid <- genUid
    let room107 = Loc "Room 107" "Room 107" "Room 107"

    locs .= M.fromList
        [ (bathroomUid          , bathroom)
        , (overlookLotUid       , overlookLot)
        , (cemetaryUid          , cemetary)
        , (room205Uid           , room205)
        , (woodSideExtWestUid   , woodSideExtWest)
        , (woodSideExtEastUid   , woodSideExtEast)
        , (westHallway1FSouthUid, westHallway1FSouth)
        , (westHallway1FMidUid  , westHallway1FMid)
        , (westHallway1FNorthUid, westHallway1FNorth)
        , (eastHallway1FSouthUid, eastHallway1FSouth)
        , (eastHallway1FMidUid  , eastHallway1FMid)
        , (eastHallway1FNorthUid, eastHallway1FNorth)
        , (room101Uid           , room101)
        , (room102Uid           , room102)
        , (room103Uid           , room103)
        , (room104Uid           , room104)
        , (room105Uid           , room105)
        , (room106Uid           , room106)
        , (room107Uid           , room107)
        ]
    loc .= woodSideExtWestUid
    connections
        .= [ Connection bathroomUid    S  overlookLotUid
           , Connection overlookLotUid N  bathroomUid
           , Connection overlookLotUid NW cemetaryUid
           , Connection cemetaryUid    SE overlookLotUid

           , Connection woodSideExtWestUid E woodSideExtEastUid
           , Connection woodSideExtEastUid W woodSideExtWestUid

           , Connection woodSideExtWestUid N westHallway1FSouthUid
           , Connection westHallway1FSouthUid S woodSideExtWestUid

           , Connection westHallway1FSouthUid N westHallway1FMidUid
           , Connection westHallway1FMidUid S westHallway1FSouthUid

           , Connection westHallway1FMidUid N westHallway1FNorthUid
           , Connection westHallway1FNorthUid S westHallway1FMidUid

           , Connection westHallway1FSouthUid E room101Uid
           , Connection room101Uid W westHallway1FSouthUid

           , Connection westHallway1FMidUid E room102Uid
           , Connection room102Uid W westHallway1FMidUid

           , Connection westHallway1FMidUid NE room103Uid
           , Connection room103Uid W westHallway1FMidUid

           , Connection westHallway1FNorthUid E room104Uid
           , Connection room104Uid W westHallway1FNorthUid
           ]
    jamesUid <- genUid
    let
        jamesCombat = Combat
            jamesUid
            Melee
            10
            None
            (\npc -> "James swings his fists at " ++ npc ^. name ++ ".")
    let james = Avatar "James" jamesCombat
    avatar .= Just james
    angelaUid <- genUid
    let
        angela = Npc
            angelaUid
            "Angela"
            Female
            Neutral
            True
            100
            []
            "Angela is frantically searching the cemetary for something."
            Dialog
            cemetaryUid
            True
            [ "I'm looking for my Mama... I-I mean my mother. It's been so long since I've seen her. I thought my father and brother were here, but...I can't find them either. ...I'm sorry...it's not your problem."
            , "Sorry, I didn't mean to bother you"
            , "Where is she..."
            ]
            0

    lyingFigureAttackUid <- genUid
    let
        lyingFigureAttack = Combat
            lyingFigureAttackUid
            Ranged
            10
            None
            (\npc -> "It sprays a vile mist at " ++ show npc ++ ".")
    lyingFigureUid <- genUid
    let
        lyingFigure0 = Npc
            lyingFigureUid
            "Lying Figure"
            Female
            Neutral
            True
            100
            [lyingFigureAttackUid]
            "The lying figure is motionless"
            Monster
            bathroomUid
            True
            []
            0
    combats .= [lyingFigureAttack]
    mannequinRoom205Uid <- genUid
    let
        mannequinRoom205 = Npc
            mannequinRoom205Uid
            "Mannequin"
            Female
            Hostile
            False
            100
            []
            "The figure appears to be mannequin legs attached to mannequin legs"
            Monster
            room205Uid
            True
            []
            0

    npcs .= [angela, lyingFigure0, mannequinRoom205]
    let
        cemetaryScene = Scene
            cemetaryUid
            [ SceneDialog jamesUid "Excuse me, I..."
            , SceneNarration
                "The young woman steps back from grave and gasps in surprise"
            , SceneDialog angelaUid "I, I'm sorry...I, I...  I was just..."
            , SceneDialog
                jamesUid
                "No, it's okay.  I didn't mean to scare you.  I'm kind of lost."
            , SceneDialog angelaUid "Lost..."
            ]
            0
            ["You see a young woman frantically searching the cemetary."]
    scenes .= [cemetaryScene]
    carUid <- genUid
    let
        car = Container
            carUid
            "Car"
            "You parked your car here."
            "It's a baby blue 1976 Chevy Nova."
            overlookLotUid
            Closed
            Transparent
    containers .= [car]
    let
        map' = Item
            "Map"
            []
            "It's a well-worn map of Silent Hill."
            -- (ItemContainer carUid)
            (ItemLoc room205Uid)
            [Readable]
    let
        flashlight = Item
            "Flashlight"
            ["light", "flash light"]
            "The flashlight is covered in scratches. It feels heavy."
            (ItemLoc room205Uid)
            [OnOff]

    items .= [map', flashlight]
